use std::collections::HashSet;
use std::fs;
use std::io::BufReader;
use std::io::prelude::*;

#[derive(Eq, PartialEq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Wire { A, B, C, D, E, F, G }

impl Wire {
    fn from_char(c: char) -> Wire {
        match c {
            'a' => { Wire::A }
            'b' => { Wire::B }
            'c' => { Wire::C }
            'd' => { Wire::D }
            'e' => { Wire::E }
            'f' => { Wire::F }
            'g' => { Wire::G }
            _ => { panic!("Invalid wire label: {}", c) }
        }
    }
}

type Digit = Vec<Wire>;

struct Display { samples: Vec<Digit>, output: Vec<Digit> }

/*

0:      1:      2:      3:      4:

aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
gggg    ....    gggg    gggg    ....


5:      6:      7:      8:      9:

aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
gggg    gggg    ....    gggg    gggg


ZERO:   abcefg   (6)
ONE:    cf       (2)  !
TWO:    acdeg    (5)
THREE:  acdfg    (5)
FOUR:   bcdf     (4)  !
FIVE:   abdfg    (5)
SIX:    abdefg   (6)
SEVEN:  acf      (3)  !
EIGHT:  abcdefg  (7)  !
NINE:   abcdfg   (6)

 */

fn digit_from_string(s: &str) -> Digit {
    s.chars().map(|c| Wire::from_char(c)).collect()
}

enum Digits { Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine }

impl Digits {
    fn to_digit(&self) -> Digit {
        digit_from_string(
            match self {
                Digits::Zero  => { "abcefg" }
                Digits::One   => { "cf" }
                Digits::Two   => { "acdeg" }
                Digits::Three => { "acdfg" }
                Digits::Four  => { "bcdf" }
                Digits::Five  => { "abdfg" }
                Digits::Six   => { "abdefg" }
                Digits::Seven => { "acf" }
                Digits::Eight => { "abcdefg" }
                Digits::Nine  => { "abcdfg" }
        })
    }
}

pub fn solution(file: &fs::File) -> (String, String) {
    let reader = BufReader::new(file);
    let mut displays: Vec<Display> = Vec::new();
    for line in reader.lines().map(|l| l.unwrap()) {
        let display =
            if let [raw_signals, raw_output] = line.splitn(2, " | ").collect::<Vec<&str>>()[..] {
                Display { samples: raw_signals.split_whitespace().map(digit_from_string).collect(),
                          output: raw_output.split_whitespace().map(digit_from_string).collect() }
            } else { panic!("Could not extract digits.") };
        displays.push(display);
    }
    let lengths = HashSet::from([Digits::One.to_digit().len(),
                                 Digits::Four.to_digit().len(),
                                 Digits::Seven.to_digit().len(),
                                 Digits::Eight.to_digit().len()]);
    let part1_counts =
        displays.iter().fold(0, |acc, display|
                             acc + display
                                   .output
                                   .iter()
                                   .filter(|d| lengths.contains(&d.len()) )
                                   .collect::<Vec<&Vec<Wire>>>()
                                   .len());

    (part1_counts.to_string(), "part2".to_string())
}
