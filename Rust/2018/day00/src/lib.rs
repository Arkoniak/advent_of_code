use std::{
    fs,
    str,
    process,
};

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
    
    let v = contents.lines();

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
