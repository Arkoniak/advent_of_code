use std::str;
use std::collections::HashMap;

pub fn runpart1(v: str::Lines) -> i64 {
    let mut d = 0;
    let mut t = 0;

    for s in v {
        let (d1, d2) = getrepeats(s);
        d += d1;
        t += d2;
    }

    d*t
}

fn getrepeats(s: &str) -> (i64, i64) {
    let mut dict = HashMap::new();
    for c in s.chars() {
        let cnt = dict.entry(c).or_insert(0);
        *cnt += 1;
    }

    let mut d = 0;
    let mut t = 0;

    for cnt in dict.values() {
        if *cnt == 2 {
            d = 1;
        } else if *cnt == 3 {
            t = 1;
        }
    }

    (d, t)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert_eq!(getrepeats(&"bababc"), (1, 1));
        assert_eq!(getrepeats(&"abcdef"), (0, 0));
        assert_eq!(getrepeats(&"abbcde"), (1, 0));
        assert_eq!(getrepeats(&"abcccd"), (0, 1));
        assert_eq!(getrepeats(&"aabcdd"), (1, 0));
        assert_eq!(getrepeats(&"abcdee"), (1, 0));
        assert_eq!(getrepeats(&"ababab"), (0, 1));
    }
}
