use std::str;

pub fn runpart2(v: Vec<(i64, i64)>) -> Result<i64, &'static str> {
    let x0 = match v.iter().min_by_key(|x| x.0) {
        Some(x) => x.0,
        None => return Err("Vector is empty!"),
    };
    let x1 = match v.iter().max_by_key(|x| x.0) {
        Some(x) => x.0,
        None => return Err("Vector is empty!"),
    };
    let y0 = match v.iter().min_by_key(|x| x.1) {
        Some(x) => x.1,
        None => return Err("Vector is empty!"),
    };
    let y1 = match v.iter().max_by_key(|x| x.1) {
        Some(x) => x.1,
        None => return Err("Vector is empty!"),
    };
    
    let mut res = 0;

    for j in y0..(y1 + 1) {
        for i in x0..(x1 + 1) {
            let sumd: i64 = v.iter().map(|el| dist(&(i, j), el)).sum();
            if sumd < 10000 {
                res += 1;
            }
        }
    }

    Ok(res)
}

fn dist(x: &(i64, i64), y: &(i64, i64)) -> i64 {
    (x.0 - y.0).abs() + (x.1 - y.1).abs()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
    }
}
