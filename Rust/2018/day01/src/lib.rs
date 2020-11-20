use std::fs;
use std::str;
use std::collections::HashSet;

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
    
    let v = contents.split("\n");
    // println!("{:?}", v);

    if config.part == 1 {
        let res = summarize(v);
        println!("Part1: result of calibration is: {}", res);
    } else {
        let res2 = rep_freq(v);
        println!("Part2: repeating frequency is: {}", res2);
    }
}

fn summarize(v: str::Split<&str>) -> i64 {
    let mut res = 0;

    for s in v {
        res += parse(s);
        // println!("{}", s);
    }   
    
    return res
}

fn rep_freq(v: str::Split<&str>) -> i64 {
    let mut freq = 0;
    let mut set = HashSet::new();
    set.insert(freq);

    loop {
        let vv = v.clone();
        for s in vv {
            let delta = parse(s);
            if delta == 0 {
                continue
            }
            freq += parse(s);
            // println!("Freq: {}, set: {:?}", freq, set);
            if set.contains(&freq) {
                // println!("Result: {}", freq);
                return freq;
            } else {
                set.insert(freq);
            }
        }
    }
}

fn parse(s: &str) -> i64 {
    let i = s.parse::<i64>().unwrap_or(0);
    // I'll leave it here for future references;
    // let i = match s.parse::<i64>() {
    //     Ok(i) => i,
    //     Err(e) => {
    //         0
    //     }
    // }
    //
    // How to index into first byte
    // if s[0..1] == String::from("+") {
    //     1
    // } else {
    //     -1
    // }
    return i
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let s1 = String::from("+10");
        let s2 = String::from("-5");
        assert_eq!(10, parse(&s1));
        assert_eq!(-5, parse(&s2));
    }

    #[test]
    fn test_summarize() {
        let v = "+5\n-7".split("\n");
        assert_eq!(summarize(v), -2);
    }

    #[test]
    fn test_rep_freq() {
        let v = "+1\n-1".split("\n");
        assert_eq!(rep_freq(v), 0);

        let v = "+3,+3,+4,-2,-4".split(",");
        assert_eq!(rep_freq(v), 10);

        let v = "-6,+3,+8,+5,-6".split(",");
        assert_eq!(rep_freq(v), 5);

        let v = "+7,+7,-2,-7,-4".split(",");
        assert_eq!(rep_freq(v), 14);
    }
}
