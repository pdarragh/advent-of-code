use core::cmp::Ordering;
use std::fmt;
use std::fs;
use std::io::BufReader;
use std::io::prelude::*;
use std::iter;

#[derive(Eq, PartialEq, Clone, Debug)]
struct Coord { x: i32, y: i32 }

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Coord {
    fn from_string(s: &str) -> Coord {
        match s.split(",").collect::<Vec<&str>>()[..] {
            [x_str, y_str] => {
                x_str.parse::<i32>()
                    .and_then(
                        |x|
                        y_str.parse::<i32>()
                            .and_then(
                                |y|
                                Ok(Coord { x, y } ) ))
                    .or(Err(format!("Invalid coord input: '{}'", s))) }
            _ => Err(format!("Invalid coord input: '{}'", s))
        }.unwrap()
    }

    fn next_in_direction(&self, direction: Direction) -> Coord {
        match direction {
            Direction::N => { Coord { x: self.x, y: self.y - 1 } }
            Direction::E => { Coord { x: self.x + 1, y: self.y } }
            Direction::S => { Coord { x: self.x, y: self.y + 1 } }
            Direction::W => { Coord { x: self.x - 1, y: self.y } }
            Direction::NE => { Coord { x: self.x + 1, y: self.y - 1 } }
            Direction::SE => { Coord { x: self.x + 1, y: self.y + 1 } }
            Direction::SW => { Coord { x: self.x - 1, y: self.y + 1 } }
            Direction::NW => { Coord { x: self.x - 1, y: self.y - 1 } }
        }
    }

    fn compare(&self, other: &Coord) -> Option<Direction> {
        // Grid has (0, 0) in top-left, so x increases left-to-right and y
        // increases top-to-bottom.
        match (self.x.cmp(&other.x), self.y.cmp(&other.y)) {
            (Ordering::Less, Ordering::Less) => { Some(Direction::NW) }
            (Ordering::Less, Ordering::Equal) => { Some(Direction::W) }
            (Ordering::Less, Ordering::Greater) => { Some(Direction::SW) }
            (Ordering::Equal, Ordering::Less) => { Some(Direction::N) }
            (Ordering::Equal, Ordering::Equal) => { None }
            (Ordering::Equal, Ordering::Greater) => { Some(Direction::S) }
            (Ordering::Greater, Ordering::Less) => { Some(Direction::NE) }
            (Ordering::Greater, Ordering::Equal) => { Some(Direction::E) }
            (Ordering::Greater, Ordering::Greater) => { Some(Direction::SE) }
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Direction { N, S, E, W,
                 NE, SE, SW, NW }

#[derive(Debug)]
struct SegmentParseError { input: String }

impl fmt::Display for SegmentParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Could not parse segment from input: {}", self.input)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Segment { start: Coord, end: Coord, direction: Direction }

impl Segment {
    fn from_string(s: &str) -> Result<Segment, SegmentParseError> {
        match s.split(" -> ").collect::<Vec<&str>>()[..] {
            [c1, c2] => {
                let c1 = Coord::from_string(c1);
                let c2 = Coord::from_string(c2);
                if let Some(d) = c2.compare(&c1) {
                    Ok(Segment { start: c1,
                                 end: c2,
                                 direction: d } )
                } else {
                    Err(SegmentParseError { input: s.to_string() } )
                }
            }
            _ => { Err(SegmentParseError { input: s.to_string() } ) }
        }
    }

    fn is_horizontal_or_vertical(&self) -> bool {
        match self.direction {
            Direction::N | Direction::E | Direction::S | Direction::W => { true }
            _ => { false }
        }
    }

    fn extreme(&self) -> Coord {
        Coord { x: self.start.x.max(self.end.x),
                y: self.start.y.max(self.end.y) }
    }
}

impl IntoIterator for Segment {
    type Item = Coord;
    type IntoIter = SegmentIterator;

    fn into_iter(self) -> Self::IntoIter {
        SegmentIterator { end: self.end.clone(),
                          next: self.start.clone(),
                          direction: self.direction }
    }
}

struct SegmentIterator { end: Coord,
                         next: Coord,
                         direction: Direction }

impl Iterator for SegmentIterator {
    type Item = Coord;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(d) = self.end.compare(&self.next) {
            if d != self.direction { return None };
        }
        let result = self.next.clone();
        self.next = self.next.next_in_direction(self.direction);
        Some(result)
    }
}

pub fn solution(file: &fs::File) -> (String, String) {
    let reader = BufReader::new(file);
    let mut segments = Vec::new();
    let mut extreme = Coord { x: 0, y: 0 };
    for line in reader.lines().map(|l| l.unwrap()) {
        if let Ok(segment) = Segment::from_string(&line) {
            extreme = Coord { x: extreme.x.max(segment.extreme().x),
                              y: extreme.y.max(segment.extreme().y) };
            segments.push(segment);
        }
    }
    let width: usize = (extreme.x + 1).try_into().unwrap();
    let mut hv_depth_map: Vec<Vec<i32>> =
        (0..(extreme.y + 1))
        .map(|_| {iter::repeat(0)
                  .take(width)
                  .collect::<Vec<i32>>()} )
        .collect();
    let mut all_depth_map: Vec<Vec<i32>> =
        (0..(extreme.y + 1))
        .map(|_| {iter::repeat(0)
                  .take(width)
                  .collect::<Vec<i32>>()} )
        .collect();
    for segment in segments {
        let is_horizontal_or_vertical = segment.is_horizontal_or_vertical();
        for coord in segment {
            let y_idx: usize = coord.y.try_into().unwrap();
            let x_idx: usize = coord.x.try_into().unwrap();
            if is_horizontal_or_vertical {
                hv_depth_map[y_idx][x_idx] += 1;
            }
            all_depth_map[y_idx][x_idx] += 1;
        }
    }
    let hv_intersections = hv_depth_map
        .iter()
        .fold(0,
              |dm_acc, row|
              row.iter().fold(dm_acc,
                              |r_acc, cell|
                              {r_acc + if *cell >= 2 { 1 } else { 0 }} ));
    let all_intersections = all_depth_map
        .iter()
        .fold(0,
              |dm_acc, row|
              row.iter().fold(dm_acc,
                              |r_acc, cell|
                              {r_acc + if *cell >= 2 { 1 } else { 0 }} ));
    (hv_intersections.to_string(), all_intersections.to_string())
}
