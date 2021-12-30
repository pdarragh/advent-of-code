use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

pub fn solution(file: &fs::File) -> (String, String) {
    let mut input = String::new();
    let _ = BufReader::new(file).read_to_string(&mut input).unwrap();
    let positions: Vec<u32> = input
        .split(',')
        .map(str::parse::<u32>)
        .map(|r| r.unwrap())
        .collect();
    let min_pos = positions.iter().min().unwrap().clone();
    let max_pos = positions.iter().max().unwrap().clone();
    let part1_fuel = (min_pos..(max_pos + 1))
        .map(|pos|
             positions
             .iter()
             .map(|p| p.max(&pos) - p.min(&pos))
             .sum::<u32>())
        .min()
        .unwrap();
    let part2_fuel = (min_pos..(max_pos + 1))
        .map(|pos|
             positions
             .iter()
             .map(|p| p.max(&pos) - p.min(&pos))
             .map(|d| ((d * d) + d) / 2)
             .sum::<u32>())
        .min()
        .unwrap();
    (part1_fuel.to_string(), part2_fuel.to_string())
}
