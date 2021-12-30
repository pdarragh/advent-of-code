use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

const BASE_TIMER: usize = 6;
const NEW_FISH_EXTRA: usize = 2;

const DAYS_TO_SIMULATE: u8 = 80;

pub fn solution(file: &fs::File) -> (String, String) {
    let mut lifetimes: Vec<u32> = vec![0; BASE_TIMER + NEW_FISH_EXTRA + 1];
    let mut input = String::new();
    if let Ok(_) = BufReader::new(file).read_to_string(&mut input) {
        input
            .split(',')
            .map(str::parse::<usize>)
            .map(|r| r.unwrap())
            .for_each(|v| lifetimes[v] += 1);
    }
    for _ in 0..DAYS_TO_SIMULATE {
        let frisky_fish = lifetimes[0];
        for index in 1..(BASE_TIMER + NEW_FISH_EXTRA + 1) {
            lifetimes[index - 1] = lifetimes[index];
        }
        lifetimes[BASE_TIMER] += frisky_fish;
        lifetimes[BASE_TIMER + NEW_FISH_EXTRA] = frisky_fish;
    }
    (lifetimes.iter().sum::<u32>().to_string(), "part2".to_string())
}
