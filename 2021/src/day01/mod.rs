mod part1;

use crate::solution::Solution;
use part1::part1;

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: None }
}
