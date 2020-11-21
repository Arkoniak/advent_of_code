use std::str;

use crate::part01::Matrix;
use crate::part01::parse;
use crate::part01::getrect;

pub fn runpart2(v: str::Lines) -> Result<i64, &'static str> {
    let mut vv: Vec<&str> = Vec::new();
    for s in v {
        vv.push(s);
    }
    let mut m = Matrix::new(1000, 1000);
    for s in &vv {
        parse(&mut m, s);       
    }

    for s in &vv {
        let (id, x0, y0, w, h) = getrect(s);
        let mut t = true;
        for i in x0..(x0 + w) {
            for j in y0..(y0 + h) {
                let val = m.get(i, j);
                if val > 1 {
                    t = false;
                    break;
                }
            }
        }
        if t {
            return Ok(id);
        }
    }

    Err("Rectangular was not found...")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
    }
}
