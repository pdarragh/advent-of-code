use crate::solution::Solution;

use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

fn flip_bit(b: i32) -> i32 {
    if b == 0 { 1 } else if b == 1 { 0 } else { panic!("Invalid bit: {}", b) }
}

fn counts_to_binary(counts: &Vec<i32>, binary: &mut Vec<i32>) {
    for &count in counts {
        if count >= 0 { binary.push(1) }
        else  { binary.push(0) }
    }
}

fn flip_binary(binary: &Vec<i32>) -> Vec<i32> {
    let mut flipped = binary.clone();
    for i in 0..flipped.len() {
        flipped[i] = flip_bit(flipped[i]);
    }
    flipped
}

fn binary_to_decimal(binary: &Vec<i32>) -> i32 {
    let mut value: i32 = 0;
    let mut pow: i32 = 1;
    for b in binary.iter().rev() {
        value += *b * pow;
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

fn bit_to_count(b : &i32) -> i32 {
    if *b == 1 { 1 } else { -1 }
}

fn compute_oxygen(values: &Vec<Vec<i32>>) -> i32 {
    let mut remaining = values.clone();
    let indices = values[0].len();
    for idx in 0..indices {
        let count = remaining.iter().fold(0, |a, v| a + bit_to_count(&v[idx]));
        let keep = if count >= 0 { 1 } else { -1 };
        remaining.retain(|v| v[idx] == keep);
        if remaining.len() == 1 { break }
    }
    if remaining.len() != 1 {
        panic!("Problem in oxygen calculation; {} items remaining.", remaining.len());
    } else {
        let mut binary = Vec::with_capacity(indices);
        counts_to_binary(&remaining[0], &mut binary);
        binary_to_decimal(&binary)
    }
}

fn compute_co2(values: &Vec<Vec<i32>>) -> i32 {
    let mut remaining = values.clone();
    let indices = values[0].len();
    for idx in 0..indices {
        let count = remaining.iter().fold(0, |a, v| a + bit_to_count(&v[idx]));
        let keep = if count < 0 { 1 } else { -1 };
        remaining.retain(|v| v[idx] == keep);
        if remaining.len() == 1 { break }
    }
    if remaining.len() != 1 {
        panic!("Problem in CO2 calculation; {} items remaining.", remaining.len());
    } else {
        let mut binary = Vec::with_capacity(indices);
        counts_to_binary(&remaining[0], &mut binary);
        binary_to_decimal(&binary)
    }
}

fn oxygen_and_co2(values: &Vec<Vec<i32>>) -> (i32, i32) {
    (compute_oxygen(values), compute_co2(values))
}

pub fn part2(file: &fs::File) -> String {
    let reader = BufReader::new(file);
    let mut values = Vec::new();
    for line in reader.lines().map(|l| l.unwrap()) {
        let value = Vec::from(line.chars().map(char_to_bit).collect::<Vec<i32>>());
        values.push(value);
    }
    let (oxygen, co2) = oxygen_and_co2(&values);
    (oxygen * co2).to_string()
}

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: Some(part2) }
}
