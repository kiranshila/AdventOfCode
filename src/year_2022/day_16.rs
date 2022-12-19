use itertools::Itertools;
use kstring::KString;
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    collections::{BTreeSet, HashMap, VecDeque},
    fs,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Valve {
    name: KString,
    flow_rate: u64,
    tunnels: BTreeSet<KString>,
}

#[derive(Debug)]
struct Graph(HashMap<KString, Valve>);

fn parse(input: &str) -> (KString, Valve) {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves?((?: [A-Z][A-Z],?)+)"
        )
        .unwrap();
    }
    let captures = RE.captures(input).unwrap();
    let name = KString::from(captures[1].to_string());
    let flow_rate = str::parse(&captures[2]).unwrap();
    let tunnels = captures[3]
        .split(',')
        .map(|s| KString::from(s.trim().to_string()))
        .collect();
    (
        name.clone(),
        Valve {
            name,
            flow_rate,
            tunnels,
        },
    )
}

fn distances() -> HashMap<Valve, HashMap<Valve, u64>> {
    // Floyd-Warshall
    let mut dist: HashMap<Valve, HashMap<Valve, u64>> = GRAPH
        .0
        .values()
        .map(|v| {
            (
                v.clone(),
                v.tunnels
                    .iter()
                    .map(|t| (GRAPH.0.get(t).unwrap().clone(), 1))
                    .collect(),
            )
        })
        .collect();
    let maximum = GRAPH.0.len();
    for valves in GRAPH.0.values().permutations(3) {
        let k = valves[0];
        let i = valves[1];
        let j = valves[2];
        let valve_i = dist.get(i).unwrap();
        let valve_k = dist.get(k).unwrap();
        let maybe_l = if let Some(x) = valve_i.get(k) {
            valve_k.get(j).map(|y| *x + *y)
        } else {
            None
        };
        let r = *valve_i.get(j).unwrap_or(&(maximum as u64));

        let new_dist = if let Some(l) = maybe_l {
            std::cmp::min(l, r)
        } else {
            r
        };

        dist.get_mut(i).unwrap().insert(j.clone(), new_dist);
    }
    dist
}

lazy_static! {
    static ref INPUT: String = fs::read_to_string("resources/2022/16/elijah_input").unwrap();
    static ref GRAPH: Graph = Graph(INPUT.lines().map(parse).collect());
    static ref DISTANCES: HashMap<Valve, HashMap<Valve, u64>> = distances();
}

impl Graph {
    fn max_pressure(&self, total_time: u64) -> HashMap<BTreeSet<Valve>, u64> {
        let mut max_relief = HashMap::new();
        let mut queue = VecDeque::new();
        queue.push_back(Step::new(GRAPH.0.get("AA").unwrap().clone(), total_time));
        while let Some(s) = queue.pop_front() {
            for next_step in s.traversals() {
                let new_released = next_step.released;
                let other_released = *(max_relief.get(&next_step.visited).unwrap_or(&0));
                max_relief.insert(
                    next_step.visited.clone(),
                    std::cmp::max(new_released, other_released),
                );
                queue.push_back(next_step);
            }
        }
        max_relief
    }

    fn part_1(&self) -> u64 {
        *self.max_pressure(30).values().max().unwrap()
    }

    fn part_2(&self) -> u64 {
        let paths = self.max_pressure(26);
        paths
            .iter()
            .combinations(2)
            .filter_map(|v| {
                let me = v[0];
                let elephant = v[1];
                if me.0.is_disjoint(elephant.0) {
                    Some(me.1 + elephant.1)
                } else {
                    None
                }
            })
            .max()
            .unwrap()
    }
}

#[derive(Debug)]
struct Step {
    valve: Valve,
    remaining_time: u64,
    released: u64,
    visited: BTreeSet<Valve>,
}

impl Step {
    fn new(start: Valve, remaining_time: u64) -> Self {
        Self {
            valve: start,
            remaining_time,
            released: 0,
            visited: BTreeSet::new(),
        }
    }

    fn traversals(&self) -> Vec<Self> {
        let mut ts = vec![];
        for (valve, steps) in DISTANCES.get(&self.valve).unwrap() {
            if self.visited.contains(valve) || valve.flow_rate == 0 {
                continue;
            } else {
                let time_remaining = self.remaining_time as i64 - *steps as i64 - 1;
                if time_remaining <= 0 {
                    continue;
                } else {
                    let mut new_visited = self.visited.clone();
                    new_visited.insert(valve.clone());
                    ts.push(Self {
                        valve: valve.clone(),
                        remaining_time: time_remaining as u64,
                        released: self.released + valve.flow_rate * time_remaining as u64,
                        visited: new_visited,
                    });
                }
            }
        }
        ts
    }
}

fn main() {
    dbg!(GRAPH.part_1());
}
