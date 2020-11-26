use std::{
    fs,
    str,
    process,
    ops::IndexMut,
    ops::Index,
    convert::TryFrom,
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

fn preprocess(contents: String) -> Vec<i64> {
    let mut v = Vec::new();
    let it = contents.split(" ");
    for t in it {
        let el = t.trim().parse().unwrap();
        v.push(el);
    }

    v
}

/////////////////////////////////////////////////////////
// Auxiliary structure
/////////////////////////////////////////////////////////
pub struct Grid<T> {
    v: Vec<T>,
    ox:i64,
    oy:i64,
    width: usize,
    height: usize,
}

impl<T: Copy> Grid<T> {
    pub fn new(w: i64, h: i64, ox: i64, oy: i64, default: T) -> Self {
        // let buffer: Vec<T> = unsafe {
        //     vec![std::mem::zeroed(); w*h]
        // };
        Grid {
            v: vec![default; usize::try_from(w*h).unwrap()],
            ox: ox,
            oy: oy,
            width: usize::try_from(w).unwrap(),
            height: usize::try_from(h).unwrap(),
        }
    }

    fn coord(&self, i: i64, j: i64) -> usize {
        let i1 = usize::try_from(i - self.ox).unwrap();
        let j1 = usize::try_from(j - self.oy).unwrap();
        i1 * self.height + j1
    }

    pub fn set(&mut self, i: i64, j: i64, x: T) {
        let idx = self.coord(i, j);
        self.v[idx] = x;
    }

    pub fn get(&self, i: i64, j: i64) -> T {
        self.v[self.coord(i, j)]
    }
}

impl<T: Copy> Index<(i64, i64)> for Grid<T> {
    type Output = T;

    fn index(&self, idx: (i64, i64)) -> &Self::Output {
        &self.v[self.coord(idx.0, idx.1)]
    }
}

impl<T: Copy> IndexMut<(i64, i64)> for Grid<T> {
    fn index_mut(&mut self, idx: (i64, i64)) -> &mut Self::Output{
        let idx = self.coord(idx.0, idx.1);
        &mut self.v[idx]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grid() {
        let mut m = Grid::new(2, 2, 0, 0, 0);
        m.set(0, 0, 10);
        m.set(0, 1, 5);
        m.set(1, 0, -2);
        m.set(1, 1, 3);

        assert_eq!(m.get(0, 0), 10);
        assert_eq!(m.get(0, 1), 5);
        assert_eq!(m.get(1, 0), -2);
        assert_eq!(m.get(1, 1), 3);
        m[(1, 1)] = 5;
        assert_eq!(m[(1, 1)], 5);

        let mut m = Grid::new(2, 2, -2, -1, 0);
        m.set(-2, -1, 10);
        m.set(-2, 0, 5);
        m.set(-1, -1, -2);
        m.set(-1, 0, 3);

        assert_eq!(m.get(-2, -1), 10);
        assert_eq!(m.get(-2, 0), 5);
        assert_eq!(m.get(-1, -1), -2);
        assert_eq!(m.get(-1, 0), 3);
        assert_eq!(m[(-2, -1)], 10);

        let mut m = Grid::new(2, 2, 0, 0, (1, 2));
        m.set(0, 0, (0, 0));
        m.set(0, 1, (0, 1));
        m.set(1, 0, (1, 0));

        assert_eq!(m.get(0, 0), (0, 0));
        assert_eq!(m.get(0, 1), (0, 1));
        assert_eq!(m.get(1, 0), (1, 0));
        assert_eq!(m.get(1, 1), (1, 2));
        assert_eq!(m[(1, 1)], (1, 2));

        m[(0, 0)] = (-1, -2);
        assert_eq!(m[(0, 0)], (-1, -2));
    }

    #[test]
    fn test_preprocess() {
        let s = String::from("");
        assert_eq!(preprocess(s), 0);
    }
}

