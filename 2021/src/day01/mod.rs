use crate::solution::Solution;

use std::fs;
use std::i32::MIN;
use std::io::BufReader;
use std::io::prelude::*;

pub fn part1(file: &fs::File) -> String {
    let reader = BufReader::new(file);
    let mut prev = MIN;
    let mut greater = 0;
    for line in reader.lines().map(|l| l.unwrap()) {
        let curr = line.parse::<i32>().unwrap();
        if prev != MIN && curr > prev {
            greater += 1;
        }
        prev = curr;
    }
    greater.to_string()
}

pub fn part2(file: &fs::File) -> String {
    let reader = BufReader::new(file);
    let mut a = MIN;
    let mut b = MIN;
    let mut prev_sum = MIN;
    let mut greater = 0;
    for (idx, line) in reader.lines().map(|l| l.unwrap()).enumerate() {
        let curr = line.parse::<i32>().unwrap();
        if idx == 0 {
            a = curr;
            continue;
        }
        if idx == 1 {
            b = curr;
            continue;
        }
        if idx == 2 {
            prev_sum = a + b + curr;
        } else {
            let curr_sum = a + b + curr;
            if curr_sum > prev_sum {
                greater += 1;
            }
            prev_sum = curr_sum;
        }
        a = b;
        b = curr;
    }
    greater.to_string()
}

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: Some(part2) }
}
