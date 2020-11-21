use std::str;

pub fn runpart2(v: str::Lines) -> Result<String, &'static str> {
    let mut vv: Vec<&str> = Vec::new();
    for s in v {
        vv.push(s);
    }
    for i in 0..vv.len() {
        for j in (i+1)..vv.len() {
            if isclose(vv[i], vv[j]) {
                return Ok(combine(vv[i], vv[j]));
            }
        }
    }

    Err("No close IDs are found")
}

fn isclose(s1: &str, s2: &str) -> bool {
    let iter = s1.chars().zip(s2.chars());

    let mut cnt = 0;
    for s in iter {
        if s.0 != s.1 {
            cnt += 1;
        }
        if cnt >= 2 {
            return false
        }
    }

    true
}

fn combine(s1: &str, s2: &str) -> String {
    let mut s = String::new();
    let iter = s1.chars().zip(s2.chars());

    for c in iter {
        if c.0 == c.1 {
            s.push(c.0);
        }
    }

    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert!(isclose(&"abcde", &"pbcde"));
        assert!(!isclose(&"abcds", &"pbcde"));
    }

    #[test]
    fn test2() {
        assert_eq!(combine(&"abcde", &"pbcde"), String::from("bcde"));
    }
}
