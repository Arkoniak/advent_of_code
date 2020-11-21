use std::{
    str,
};

use regex::Regex;

pub fn runpart1(v: str::Lines) -> Result<i64, &'static str> {
    let mut vv: Vec<&str> = Vec::new();
    for s in v {
        vv.push(s);
    }
    let mut m = Matrix::new(1000, 1000);
    for s in vv {
        parse(&mut m, s);       
    }

    let mut cnt = 0;
    for i in 0..1000 {
        for j in 0..1000 {
            if m.get(i, j) > 1 {
                cnt += 1;
            }
        }
    }
    Ok(cnt)
}

pub struct Matrix {
    v: Vec<i64>,
    width: usize,
    height: usize,
}

pub fn getrect(s: &str) -> (i64, usize, usize, usize, usize) {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$").unwrap();
    }
    let caps = RE.captures(s).unwrap();
    // let cap1 = caps.get(1).map(|m| m.as_str()).unwrap();
    (caps[1].parse().unwrap(),
     caps[2].parse().unwrap(), 
     caps[3].parse().unwrap(), 
     caps[4].parse().unwrap(),
     caps[5].parse().unwrap())
}

pub fn parse(m:&mut Matrix, s: &str) {
    let (_, x0, y0, w, h) = getrect(s);

    for i in x0..(x0 + w) {
        for j in y0..(y0 + h) {
            let val = m.get(i, j);
            m.set(i, j, val + 1);
        }
    }
}

impl Matrix {
    pub fn new(w: usize, h: usize) -> Self {
        Matrix {
            v: vec![0; w*h],
            width: w,
            height: h,
        }
    }

    fn coord(&self, i: usize, j: usize) -> usize {
        j * self.height + i
    }

    pub fn set(&mut self, i: usize, j: usize, x: i64) {
        let idx = self.coord(i, j);
        self.v[idx] = x;
    }

    pub fn get(&self, i: usize, j: usize) -> i64 {
        self.v[self.coord(i, j)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matrix() {
        let mut m = Matrix::new(2, 2);
        m.set(0, 0, 10);
        m.set(0, 1, 5);
        m.set(1, 0, -2);
        m.set(1, 1, 3);

        assert_eq!(m.get(0, 0), 10);
        assert_eq!(m.get(0, 1), 5);
        assert_eq!(m.get(1, 0), -2);
        assert_eq!(m.get(1, 1), 3);
    }

    #[test]
    fn test_getrect() {
        let x = getrect(&"#123 @ 3,2: 5x4");
        assert_eq!(x, (123, 3, 2, 5, 4));
    }

    #[test]
    fn test_parse() {
        let mut m = Matrix::new(9, 11);
        parse(&mut m, &"#123 @ 3,2: 5x4");
        assert_eq!(m.get(3, 2), 1);
        assert_eq!(m.get(7, 2), 1);
        assert_eq!(m.get(3, 5), 1);
        assert_eq!(m.get(7, 5), 1);

        assert_eq!(m.get(3, 1), 0);
        assert_eq!(m.get(2, 2), 0);
        assert_eq!(m.get(8, 2), 0);
        assert_eq!(m.get(7, 1), 0);
        assert_eq!(m.get(3, 6), 0);
        assert_eq!(m.get(2, 5), 0);
        assert_eq!(m.get(8, 5), 0);
        assert_eq!(m.get(7, 6), 0);
    }
}
