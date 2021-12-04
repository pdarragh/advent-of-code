mod solution;
mod day01;
mod day02;
mod day03;

use std::env;
use std::fs;

fn main() {
    let solutions = vec![
        day01::solution(),
        day02::solution(),
        day03::solution(),
    ];
    let args: Vec<String> = env::args().collect();
    // Must supply an argument.
    // TODO: Maybe just run all available solutions?
    if args.len() <= 1 {
        println!("Give the number of a day to run!");
        return;
    }
    // Extract the day and ensure it is valid.
    let day = args[1].parse::<u64>().unwrap();
    if day > solutions.len() as u64 {
        println!("Day {} has not yet been implemented.", day);
        return;
    }
    if day < 1 || day > 24 {
        println!("{} is not a valid day for the Advent of Code!", day);
        return;
    }
    // Attempt to locate the corresponding input file.
    let mut input = env::current_dir().unwrap();
    input.push("inputs");
    input.push(format!("day{:0>2}.txt", day));
    if !input.is_file() {
        println!("Input file for day {} could not be found at expected location:", day);
        println!("    {}", input.display());
        return;
    }
    let solution = &solutions[day as usize - 1];
    let filename = input.to_str().unwrap();
    if let Ok(file) = fs::File::open(input.clone()) {
        // Execute the solution.
        if let Some(f1) = solution.part1 {
            println!("Day {} Part 1: {}", day, f1(&file));
        } else {
            println!("Day {} Part 1 not implemented.", day);
        }
    } else {
        println!("Could not open Day {} file: {}", day, filename);
        return;
    }
    if let Ok(file) = fs::File::open(input.clone()) {
        if let Some(f2) = solution.part2 {
            println!("Day {} Part 2: {}", day, f2(&file));
        } else {
            println!("Day {} Part 2 not implemented.", day);
        }
    } else {
        println!("Could not open Day {} file: {}", day, filename);
        return;
    }
}
