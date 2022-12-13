use std::fs;

type FileDescriptor = usize;
type Entries = Vec<FileDescriptor>;

#[derive(Debug, Clone)]
enum FileKind {
    RegularFile(usize),
    Directory(Entries),
}

#[derive(Debug, Clone)]
struct File {
    name: String,
    kind: FileKind,
    parent: Option<FileDescriptor>,
}

#[derive(Debug)]
struct FileSystem {
    files: Vec<File>,
}

impl FileSystem {
    // Move around
    fn cd(&self, fd: &FileDescriptor, target: &str) -> FileDescriptor {
        let file = self.files.get(*fd).unwrap();
        match &file.kind {
            FileKind::Directory(entries) => match target {
                "/" => match file.parent {
                    Some(parent) => self.cd(&parent, target),
                    None => *fd,
                },
                ".." => match file.parent {
                    Some(parent) => parent,
                    None => unreachable!(),
                },
                _ => *entries
                    .iter()
                    .find(|fd| self.files.get(**fd).unwrap().name == target)
                    .unwrap(),
            },
            _ => unreachable!(),
        }
    }
    // Creates files (first time we encounter a nested directory)
    fn ls(&mut self, fd: &FileDescriptor, entries: Vec<File>) {
        let mut children = vec![];
        for mut entry in entries {
            entry.parent = Some(*fd);
            self.files.push(entry);
            let new_fd = self.files.len() - 1;
            children.push(new_fd);
        }
        if let FileKind::Directory(child_entries) = &mut self.files.get_mut(*fd).unwrap().kind {
            child_entries.append(&mut children);
        }
    }

    fn process_commands(&mut self, lines: &str) {
        let mut cursor = 0usize;
        let mut last_ls_files = vec![];
        let mut lines = lines.lines().peekable();
        while let Some(line) = lines.next() {
            if let Some(target) = line.strip_prefix("$ cd ") {
                cursor = self.cd(&cursor, target);
            } else if line.starts_with("$ ls") {
                loop {
                    let this_line = lines.next().unwrap();
                    // Check to see if the line we are about to process is the last one
                    let mut last_one = false;
                    match lines.peek() {
                        Some(s) => {
                            if s.starts_with('$') {
                                last_one = true
                            }
                        }
                        None => last_one = true,
                    }
                    // Parse the line
                    let file = if let Some(dir_name) = this_line.strip_prefix("dir ") {
                        File {
                            name: dir_name.to_owned(),
                            kind: FileKind::Directory(vec![]),
                            parent: None,
                        }
                    } else {
                        let objects: Vec<&str> = this_line.split_ascii_whitespace().collect();
                        let size = str::parse::<usize>(objects[0]).unwrap();
                        let name = objects[1];
                        File {
                            name: name.to_owned(),
                            kind: FileKind::RegularFile(size),
                            parent: None,
                        }
                    };
                    last_ls_files.push(file);
                    if last_one {
                        // Add all the items we collected
                        self.ls(&cursor, last_ls_files);
                        // Clear
                        last_ls_files = vec![];
                        // Reset
                        break;
                    }
                }
            }
        }
    }

    fn size(&self, fd: &FileDescriptor) -> usize {
        match &self.files.get(*fd).unwrap().kind {
            FileKind::RegularFile(s) => *s,
            FileKind::Directory(entries) => entries
                .iter()
                .map(|fd| self.size(fd))
                .fold(0usize, usize::wrapping_add),
        }
    }
}

fn main() {
    let input = fs::read_to_string("resources/2022/7/input").unwrap();
    let mut file_system = FileSystem {
        files: vec![File {
            name: "/".to_owned(),
            kind: FileKind::Directory(vec![]),
            parent: None,
        }],
    };
    file_system.process_commands(&input);

    // Part 1
    let mut part_1 = 0usize;
    for (fd, file) in file_system.files.iter().enumerate() {
        if let FileKind::Directory(_) = file.kind {
            let sz = file_system.size(&fd);
            if sz <= 100000 {
                part_1 += sz;
            }
        }
    }
    println!("Part 1 - {part_1}");

    // Part 2
    let total_disk_space = 70000000usize;
    let unused_space = 30000000usize;
    let root_size = file_system.size(&0);
    let unused = total_disk_space - root_size;
    let mut part_2 = total_disk_space;
    for (fd, file) in file_system.files.iter().enumerate() {
        if let FileKind::Directory(_) = file.kind {
            let sz = file_system.size(&fd);
            if unused + sz >= unused_space && sz < part_2 {
                part_2 = sz;
            }
        }
    }
    println!("Part 2 - {part_2}");
}
