use crate::solution::Solution;

use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fs;
use std::ops::{Index, IndexMut};
use std::io::BufReader;
use std::io::prelude::*;

struct Row((i32, bool), (i32, bool), (i32, bool), (i32, bool), (i32, bool));

impl Index<i8> for Row {
    type Output = (i32, bool);

    fn index(&self, index: i8) -> &Self::Output {
        match index {
            0 => { &self.0 }
            1 => { &self.1 }
            2 => { &self.2 }
            3 => { &self.3 }
            4 => { &self.4 }
            _ => { panic!("Invalid index of Row: {}", index) }
        }
    }
}

impl IndexMut<i8> for Row {
    fn index_mut(&mut self, index: i8) -> &mut Self::Output {
        match index {
            0 => { &mut self.0 }
            1 => { &mut self.1 }
            2 => { &mut self.2 }
            3 => { &mut self.3 }
            4 => { &mut self.4 }
            _ => { panic!("Invalid index of Row: {}", index) }
        }
    }
}

impl Row {
    pub fn from_vec(vec: &Vec<i32>) -> Self {
        if vec.len() == 5 {
            return Self((vec[0], false), (vec[1], false), (vec[2], false), (vec[3], false), (vec[4], false));
        }
        panic!("Row expected exactly 5 elements in Vec; found {}", vec.len());
    }

    pub fn from_string(s: &String) -> Self {
        Self::from_vec(&s.split_whitespace().map(|si| si.parse::<i32>().unwrap()).collect())
    }
}

struct Board {
    rows: (Row, Row, Row, Row, Row),
    map: HashMap<i32, (i8, i8)>
}

impl Index<i8> for Board {
    type Output = Row;

    fn index(&self, y: i8) -> &Self::Output {
        match y {
            0 => { &self.rows.0 }
            1 => { &self.rows.1 }
            2 => { &self.rows.2 }
            3 => { &self.rows.3 }
            4 => { &self.rows.4 }
            _ => { panic!("Invalid row index of Board: {}", y) }
        }
    }
}

impl IndexMut<i8> for Board {
    fn index_mut(&mut self, y: i8) -> &mut Self::Output {
        match y {
            0 => { &mut self.rows.0 }
            1 => { &mut self.rows.1 }
            2 => { &mut self.rows.2 }
            3 => { &mut self.rows.3 }
            4 => { &mut self.rows.4 }
            _ => { panic!("Invalid row index of Board: {}", y) }
        }
    }
}

impl Index<(i8, i8)> for Board {
    type Output = (i32, bool);

    fn index(&self, (x, y): (i8, i8)) -> &Self::Output {
        &self[y][x]
    }
}

impl IndexMut<(i8, i8)> for Board {
    fn index_mut(&mut self, (x, y): (i8, i8)) -> &mut Self::Output {
        &mut self[y][x]
    }
}

impl Board {
    pub fn from_strings(ss: &Vec<String>) -> Self {
        if ss.len() == 5 {
            let mut rows: Vec<Row> = ss.iter().map(|s| Row::from_string(s)).collect();
            let mut board =  Self {
                rows: (rows.remove(0), rows.remove(0), rows.remove(0), rows.remove(0), rows.remove(0)),
                map: HashMap::with_capacity(25)
            };
            for y in 0..5 {
                for x in 0..5 {
                    let (n, _) = board[(x, y)];
                    if board.map.contains_key(&n) {
                        panic!("Duplicate number on board: {}", n);
                    }
                    board.map.insert(n, (x, y));
                }
            }
            return board;
        }
        panic!("Board expected exactly 5 strings to make rows; found {}", ss.len());
    }

    pub fn mark(&mut self, (x, y): (i8, i8)) {
        match self[(x, y)] {
            (n, _) => { self[(x, y)] = (n, true) }
        }
    }

    pub fn try_mark_number(&mut self, number: i32) -> bool {
        let t: (i8, i8);
        match self.map.entry(number) {
            Entry::Occupied(o) => { t = *o.get() }
            Entry::Vacant(_) => { return false }
        }
        self.mark(t);
        return true;
    }

    pub fn is_marked(&self, (x, y): (i8, i8)) -> bool {
        match self[(x, y)] {
            (_, b) => { b }
        }
    }

    pub fn has_won(&self) -> bool {
        (0..5).any(|y| (0..5).all(|x| self.is_marked((x, y))))
            || (0..5).any(|x| (0..5).all(|y| self.is_marked((x, y))))
    }

    pub fn sum_unmarked(&self) -> i32 {
        (0..5).fold(0,
                    |y_sum, y| (0..5).fold(y_sum,
                                           |x_sum, x| match self[(x, y)] { (n, false) => { x_sum + n }
                                                                            _         => { x_sum }} ))
    }
}

pub fn part1(file: &fs::File) -> String {
    let reader = BufReader::new(file);
    let mut it = reader.lines().map(|l| l.unwrap());

    // Get the list of numbers.
    let numbers: Vec<i32> = it.next()
        .unwrap()
        .trim()
        .split(",")
        .map(|s| s.parse::<i32>().unwrap())
        .collect();
    let _ = it.next();          // Burn a line.

    // Process the boards.
    let mut boards: Vec<Board> = Vec::new();
    let mut buffer: Vec<String> = Vec::new();
    for line in it {
        if line.trim().is_empty() {
            // Process the buffer into a board.
            boards.push(Board::from_strings(&buffer));
            buffer.clear();
        } else {
            // Accumulate the line into the buffer.
            buffer.push(line);
        }
    }

    let mut board_sum = None;
    let mut last_number = numbers[0];

    'numbers: for number in numbers {
        last_number = number;
        for board_index in 0..boards.len() {
            let board = &mut boards[board_index];
            if board.try_mark_number(number) {
                if board.has_won() {
                    board_sum = Some(board.sum_unmarked());
                    break 'numbers;
                }
            }
        }
    }

    if board_sum.is_none() {
        panic!("No winning board found!");
    }

    let answer = board_sum.unwrap() * last_number;
    return answer.to_string();
}

pub fn solution() -> Solution {
    Solution { part1: Some(part1), part2: None }
}
