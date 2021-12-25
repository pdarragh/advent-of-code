use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

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

struct Position {
    horizontal: i32,
    depth: i32,
}

impl Position {
    fn eval_command(&mut self, command: &Command) {
        match command {
            Command::Forward(x) => { self.horizontal += x }
            Command::Down(d) => { self.depth += d }
            Command::Up(d) => { self.depth -= d }
        }
    }
}

struct Aim {
    pos: Position,
    aim: i32,
}

impl Aim {
    fn eval_command(&mut self, command: &Command) {
        match command {
            Command::Forward(x) => { self.pos.horizontal += x;
                                     self.pos.depth += self.aim * x }
            Command::Down(d) => { self.aim += d }
            Command::Up(d) => { self.aim -= d }
        }
    }
}

pub fn solution(file: &fs::File) -> (String, String) {
    let mut pos = Position { horizontal: 0, depth: 0 };
    let aim_pos = Position { horizontal: 0, depth: 0 };
    let mut aim = Aim { pos: aim_pos, aim: 0 };
    for command in commands_from_file(file).iter() {
        pos.eval_command(command);
        aim.eval_command(command);
    }
    ((pos.horizontal * pos.depth).to_string(),
     (aim.pos.horizontal * aim.pos.depth).to_string())
}
