#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::{
    fs,
    str,
    process,
};

use regex::Regex;

mod part01;
mod part02;

pub struct Config {
    pub filename: String,
    pub part: i32,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("Not enough arguments");
        }

        let filename = args[1].clone();
        let part = if args.len() < 3 {
            1
        } else {
            args[2].parse().unwrap_or(1)
        };

        Ok(Config{filename, part})
    }
}

pub fn run(config: Config){
    let contents = fs::read_to_string(&config.filename)
        .expect("Something went wrong when reading the file");
    
    let v = preprocess(contents);

    if config.part == 1 {
        let res = part01::runpart1(v).unwrap_or_else(|err| {
            println!("Problem while searching for solution: {}", err);
            process::exit(1);
        });
        println!("Part1: {}", res);
    } else {
        let res = part02::runpart2(v).unwrap_or_else(|err| {
            println!("Problem while searching for solution: {}", err);
            process::exit(1);
        });
        println!("Part2: {}", res);
    }
}

fn preprocess(contents: String) -> Vec<(String, String)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^Step\s(.).*step\s(.).*$").unwrap();
    }
    let mut res = Vec::new();
    let ll = contents.lines();
    for line in ll {
        let caps = RE.captures(line).unwrap();
        res.push((String::from(&caps[1]), String::from(&caps[2])));
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_preprocess() {
        let s = String::from("Step C must be finished before step A can begin.");
        assert_eq!(preprocess(s), [(String::from("C"), String::from("A"))]);

        let s = String::from("Step C must be finished before step A can begin.\nStep D must be finished before step B can begin.");
        assert_eq!(preprocess(s), [(String::from("C"), String::from("A")), ("D".to_owned(), "B".to_owned())]);
    }
}
