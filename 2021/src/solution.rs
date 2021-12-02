use std::fs;

pub struct Solution {
    pub part1: Option<fn(&fs::File) -> String>,
    pub part2: Option<fn(&fs::File) -> String>,
}
