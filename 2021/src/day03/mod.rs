use crate::solution::Solution;

use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

fn flip_bit(b: i8) -> i8 {
    if b == 0 { 1 } else if b == 1 { 0 } else { panic!("Invalid bit: {}", b) }
}

fn counts_to_binary(counts: &Vec<i32>, binary: &mut Vec<i8>) {
    for &count in counts {
        if count > 0 { binary.push(1) }
        else if count < 0 { binary.push(0) }
        else { panic!("Balanced binary value!") }
    }
}

fn flip_binary(binary: &Vec<i8>) -> Vec<i8> {
    let mut flipped = binary.clone();
    for i in 0..flipped.len() {
        flipped[i] = flip_bit(flipped[i]);
    }
    flipped
}

fn binary_to_decimal(binary: &Vec<i8>) -> i32 {
    let mut value: i32 = 0;
    let mut pow: i32 = 1;
    for b in binary.iter().rev() {
        value += (*b as i32) * pow;
        pow *= 2;
    }
    value
}

fn gamma_and_epsilon(counts: &Vec<i32>) -> (i32, i32) {
    let mut binary = Vec::new();
    counts_to_binary(counts, &mut binary);
    let gamma = binary_to_decimal(&binary);
    let epsilon = binary_to_decimal(&flip_binary(&binary));
    (gamma, epsilon)
}

fn char_to_bit(c: char) -> i32 {
    if c == '0' { -1 }
    else if c == '1' { 1 }
    else { panic!("Invalid bit char: {}", c) }
}

pub fn part1(file: &fs::File) -> String {
    let reader = BufReader::new(file);
    let mut counts = Vec::new();
    for line in reader.lines().map(|l| l.unwrap()) {
        if counts.is_empty() {
            line.chars().for_each(|c| counts.push(char_to_bit(c)));
        } else {
            for (i, c) in line.chars().enumerate() {
                counts[i] += char_to_bit(c);
            }
        }
    }
    let (gamma, epsilon) = gamma_and_epsilon(&counts);
    (gamma * epsilon).to_string()
}

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: None }
}
