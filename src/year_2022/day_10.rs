use std::fs;

#[derive(Debug)]
struct Cpu {
    reg_x: i64,
    cycle: u64,
    ss: i64,
    screen: [[bool; 40]; 6],
}

impl Cpu {
    fn new() -> Self {
        Self {
            reg_x: 1,
            cycle: 1,
            ss: 0,
            screen: [[false; 40]; 6],
        }
    }

    fn update_ss(&mut self) {
        match self.cycle {
            20 | 60 | 100 | 140 | 180 | 220 => self.ss += self.reg_x * self.cycle as i64,
            _ => (),
        }
    }

    fn addx(&mut self, val: i64) {
        self.update_screen();
        self.cycle += 1;
        self.update_ss();
        self.update_screen();
        self.cycle += 1;
        self.reg_x += val;
        self.update_ss();
    }

    fn noop(&mut self) {
        self.update_screen();
        self.cycle += 1;
        self.update_ss();
    }

    fn update_screen(&mut self) {
        let row = ((self.cycle - 1) / 40) as i64;
        let col = ((self.cycle - 1) % 40) as i64;
        let sprite_l = self.reg_x - 1;
        let sprite_c = self.reg_x;
        let sprite_r = self.reg_x + 1;

        if col == sprite_l || col == sprite_c || col == sprite_r {
            self.screen[row as usize][col as usize] = true;
        }
    }
}

fn main() {
    let input = fs::read_to_string("resources/2022/10/input").unwrap();
    let mut cpu = Cpu::new();
    for line in input.lines() {
        if let Some(v) = line.strip_prefix("addx ") {
            cpu.addx(str::parse(v).unwrap());
        } else if line == "noop" {
            cpu.noop();
        } else {
            unreachable!()
        }
    }
    println!("Part 1 - {}", cpu.ss);
    println!("Part 2");
    for r in 0..6 {
        for c in 0..40 {
            if cpu.screen[r][c] {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}
