#![feature(drain_filter)]
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    cmp::{max, min},
    collections::HashMap,
    fs, u64,
};

#[derive(Debug)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn dist(&self, b: &Point) -> u64 {
        self.x.abs_diff(b.x) + self.y.abs_diff(b.y)
    }
}

#[derive(Debug, Clone, Copy)]
struct Range {
    lower: i64,
    upper: i64,
}

impl Range {
    fn overlaps(&self, other: &Range) -> bool {
        self.lower <= other.upper && other.lower <= self.upper
    }

    fn merge(&self, other: &Range) -> Self {
        assert!(self.overlaps(other), "Ranges must overlap to merge");
        Range {
            lower: min(self.lower, other.lower),
            upper: max(self.upper, other.upper),
        }
    }
}

fn parse(input: &str) -> (Point, Point) {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
        )
        .unwrap();
    }
    let captures = RE.captures(input).unwrap();
    let sensor = Point {
        x: str::parse(&captures[1]).unwrap(),
        y: str::parse(&captures[2]).unwrap(),
    };
    let beacon = Point {
        x: str::parse(&captures[3]).unwrap(),
        y: str::parse(&captures[4]).unwrap(),
    };
    (sensor, beacon)
}

type Row = i64;

fn update_coverage(sensor: &Point, beacon: &Point, cov: &mut HashMap<Row, Vec<Range>>) {
    let dist = sensor.dist(beacon);
    for row in (sensor.y - dist as i64)..=(sensor.y + dist as i64) {
        // Determine the range from this sensor/beacon pair
        let width = dist - sensor.y.abs_diff(row);
        let this_range = Range {
            lower: sensor.x - width as i64,
            upper: sensor.x + width as i64,
        };
        // Grab the row from the previous coverage
        match cov.get_mut(&row) {
            Some(ranges) => {
                // If this row exists in our coverage, we need to find all the ranges that overlap,
                // remove them from the coverage map and merge them with the new range
                let overlapping = ranges
                    .drain_filter(|range| range.overlaps(&this_range))
                    .collect::<Vec<_>>();
                ranges.push(overlapping.into_iter().fold(this_range, |x, y| x.merge(&y)))
            }
            // If there is no such row, simply add our range in a new vector
            None => {
                cov.insert(row, vec![this_range]);
            }
        };
    }
}

fn main() {
    let input = fs::read_to_string("resources/2022/15/input").unwrap();
    let pairs = input.lines().map(parse).collect::<Vec<_>>();
    let mut coverage = HashMap::new();
    for (s, b) in pairs {
        update_coverage(&s, &b, &mut coverage);
    }
    // Part 1
    let test_row_coverage: i64 = coverage
        .get(&2000000)
        .unwrap()
        .iter()
        .map(|range| range.upper - range.lower)
        .sum();
    println!("Part 1 - {test_row_coverage}");
    // Part 2
    let filtered = coverage
        .iter()
        .filter_map(|(k, v)| {
            if (0 <= *k) && (*k <= 4000000) && v.len() > 1 {
                let mut dirty_ranges = v.clone();
                dirty_ranges.sort_by(|x, y| x.lower.cmp(&y.lower));
                // Find all the pairs of ranges whose gap is one
                Some((
                    *k,
                    dirty_ranges
                        .windows(2)
                        .filter_map(|pair| {
                            let l = &pair[0];
                            let r = &pair[1];
                            if r.lower - l.upper == 2 {
                                Some(l.upper + 1)
                            } else {
                                None
                            }
                        })
                        .next()
                        .unwrap(),
                ))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    // There should only be one
    let (y, x) = filtered[0];
    let freq = (x * 4000000) + y;
    println!("Part 2 - {freq}");
}
