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
