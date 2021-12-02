mod solution;
mod day01;

use std::env;

fn main() {
    let solutions = vec![day01::solution()];
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        println!("Give the number of a day to run!");
        return;
    }
    let day = args[1].parse::<u64>().unwrap();
    if day > solutions.len() as u64 {
        println!("Day {} has not yet been implemented.", day);
        return;
    }
    if day < 1 || day > 24 {
        println!("{} is not a valid day for the Advent of Code!", day);
        return;
    }
    let solution = &solutions[day as usize - 1];
    if let Some(f1) = solution.part1 {
        println!("Part 1: {}", f1(format!("inputs/day{}.txt", day)));
    } else {
        println!("Part 1 not implemented.");
    }
    if let Some(f2) = solution.part2 {
        println!("Part 2: {}", f2(format!("inputs/day{}.txt", day)));
    } else {
        println!("Part 2 not implemented.");
    }
}
