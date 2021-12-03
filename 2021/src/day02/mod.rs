use crate::solution::Solution;

use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

struct Position {
    horizontal: i32,
    depth: i32
}

impl Position {
    fn increase_horizontal(&mut self, x: i32) {
        self.horizontal += x;
    }

    fn increase_depth(&mut self, d: i32) {
        self.depth += d;
    }

    fn decrease_depth(&mut self, d: i32) {
        self.depth -= d;
    }
}

enum Command {
    Forward(i32),
    Down(i32),
    Up(i32),
}

fn commands_from_file(file: &fs::File) -> Vec<Command> {
    let reader = BufReader::new(file);
    let mut commands = Vec::new();
    for line in reader.lines().map(|l| l.unwrap()) {
        commands.push(match line.split_whitespace().collect::<Vec<&str>>().as_slice() {
            ["forward", nstr] => { Command::Forward(nstr.parse().unwrap()) }
            ["down", nstr] => { Command::Down(nstr.parse().unwrap()) }
            ["up", nstr] => { Command::Up(nstr.parse().unwrap()) }
            _ => { panic!("Unsupported command.") }
        })
    }
    commands
}

pub fn part1(file: &fs::File) -> String {
    let mut pos = Position { horizontal: 0, depth: 0 };
    for command in commands_from_file(file) {
        match command {
            Command::Forward(x) => { pos.increase_horizontal(x) }
            Command::Down(d) => { pos.increase_depth(d) }
            Command::Up(d) => { pos.decrease_depth(d) }
        }
    }
    return (pos.horizontal * pos.depth).to_string();
}

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: None }
}
