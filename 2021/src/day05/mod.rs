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
        }
    }

    fn compare(&self, other: &Coord) -> Option<Direction> {
        // Grid has (0, 0) in top-left, so x increases left-to-right and y
        // increases top-to-bottom.
        if self.x > other.x && self.y == other.y {
            Some(Direction::E)
        } else if self.x < other.x && self.y == other.y {
            Some(Direction::W)
        } else if self.y > other.y && self.x == other.x {
            Some(Direction::S)
        } else if self.y < other.y && self.x == other.x {
            Some(Direction::N)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum Direction { N, S, E, W }

#[derive(Debug)]
struct SegmentParseError { input: String }

impl fmt::Display for SegmentParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Could not parse segment from input: {}", self.input)
    }
}

#[derive(Debug)]
struct SegmentDirectionError { start: Coord, end: Coord }

impl fmt::Display for SegmentDirectionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Could not identify direction for segment with start {} and end {}.",
               self.start, self.end)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Segment { start: Coord, end: Coord }

impl Segment {
    fn from_string(s: &str) -> Result<Segment, SegmentParseError> {
        match s.split(" -> ").collect::<Vec<&str>>()[..] {
            [c1, c2] => { Ok(Segment { start: Coord::from_string(c1),
                                       end:   Coord::from_string(c2) }) }
            _ => { Err(SegmentParseError { input: s.to_string() } ) }
        }
    }

    fn direction(&self) -> Result<Direction, SegmentDirectionError> {
        match self.end.compare(&self.start) {
            Some(d) => { Ok(d) }
            None => { Err(SegmentDirectionError { start: self.start.clone(),
                                                  end: self.end.clone() } ) }
        }
    }

    // fn contains(&self, coord: &Coord) -> bool {
    //     match self.direction() {
    //         Direction::N => { coord.y >= self.end.y && coord.y <= self.start.y }
    //         Direction::E => { coord.x >= self.start.x && coord.x <= self.end.x }
    //         Direction::S => { coord.y >= self.start.y && coord.y <= self.end.y }
    //         Direction::W => { coord.x >= self.end.x && coord.x <= self.start.x }
    //     }
    // }

    fn extreme(&self) -> Result<Coord, SegmentDirectionError> {
        self.direction()
            .and_then(|d|
                      match d {
                          Direction::N => { Ok(self.start.clone()) }
                          Direction::E => { Ok(self.end.clone()) }
                          Direction::S => { Ok(self.end.clone()) }
                          Direction::W => { Ok(self.start.clone()) }
                      } )
    }
}

impl IntoIterator for Segment {
    type Item = Coord;
    type IntoIter = SegmentIterator;

    fn into_iter(self) -> Self::IntoIter {
        SegmentIterator { end: self.end.clone(),
                          next: self.start.clone(),
                          direction: self.direction().unwrap() }
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
            if let Ok(segment_extreme) = segment.extreme() {
                extreme = Coord { x: extreme.x.max(segment_extreme.x),
                                  y: extreme.y.max(segment_extreme.y) };
                segments.push(segment);
            }
        }
    }
    let width: usize = (extreme.x + 1).try_into().unwrap();
    let mut depth_map: Vec<Vec<i32>> =
        (0..(extreme.y + 1))
        .map(|_| {iter::repeat(0)
                  .take(width)
                  .collect::<Vec<i32>>()} )
        .collect();
    for segment in segments {
        // println!("segment: {:?}", segment);
        for coord in segment {
            // println!("  {}", coord);
            let y_idx: usize = coord.y.try_into().unwrap();
            let x_idx: usize = coord.x.try_into().unwrap();
            depth_map[y_idx][x_idx] += 1;
        }
    }
    let intersections = depth_map
        .iter()
        .fold(0,
              |dm_acc, row|
              row.iter().fold(dm_acc,
                              |r_acc, cell|
                              {r_acc + if *cell >= 2 { 1 } else { 0 }} ));

    // for row in depth_map {
    //     for cell in row {
    //         if cell == 0 {
    //             print!(".");
    //         } else {
    //             print!("{}", cell);
    //         }
    //     }
    //     print!("\n");
    // }

    (intersections.to_string(), String::from("part 2"))
}
