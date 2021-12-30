use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

const BASE_TIMER: usize = 6;
const NEW_FISH_EXTRA: usize = 2;

const PART1_DAYS_TO_SIMULATE: u16 = 80;
const PART2_DAYS_TO_SIMULATE: u16 = 256;

pub fn solution(file: &fs::File) -> (String, String) {
    let mut lifetimes: Vec<u64> = vec![0; BASE_TIMER + NEW_FISH_EXTRA + 1];
    let mut input = String::new();
    if let Ok(_) = BufReader::new(file).read_to_string(&mut input) {
        input
            .split(',')
            .map(str::parse::<usize>)
            .map(|r| r.unwrap())
            .for_each(|v| lifetimes[v] += 1);
    }
    fn increment(lifetimes: &mut Vec<u64>) {
        let frisky_fish = lifetimes[0];
        for index in 1..(BASE_TIMER + NEW_FISH_EXTRA + 1) {
            lifetimes[index - 1] = lifetimes[index];
        }
        lifetimes[BASE_TIMER] += frisky_fish;
        lifetimes[BASE_TIMER + NEW_FISH_EXTRA] = frisky_fish;
    }
    (0..PART1_DAYS_TO_SIMULATE).for_each(|_| increment(&mut lifetimes));
    let part1 = lifetimes.iter().sum::<u64>();
    (PART1_DAYS_TO_SIMULATE..PART2_DAYS_TO_SIMULATE).for_each(|_| increment(&mut lifetimes));
    let part2 = lifetimes.iter().sum::<u64>();
    (part1.to_string(), part2.to_string())
}
