use std::fs;
use std::i32::MIN;
use std::io::BufReader;
use std::io::prelude::*;

pub fn solution(file: &fs::File) -> (String, String) {
    let reader = BufReader::new(file);
    let mut prev_a = MIN;
    let mut greater_a = 0;
    let mut a = MIN;
    let mut b = MIN;
    let mut prev_sum = MIN;
    let mut greater_b = 0;
    for (idx, line) in reader.lines().map(|l| l.unwrap()).enumerate() {
        let curr = line.parse::<i32>().unwrap();
        if prev_a != MIN && curr > prev_a {
            greater_a += 1;
        }
        prev_a = curr;
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
                greater_b += 1;
            }
            prev_sum = curr_sum;
        }
        a = b;
        b = curr;
    }
    (greater_a.to_string(), greater_b.to_string())
}
