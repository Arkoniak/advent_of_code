use std::str;

pub fn runpart1(v: Vec<i64>) -> Result<i64, &'static str> {
    let res = recur(&v, 0);

    Ok(res.1)
}

fn recur(v: &Vec<i64>, idx: usize) -> (usize, i64) {
    let nnodes = v[idx];
    let mut meta = 0;
    let mut idx0 = idx + 2;
    if nnodes > 0 {
        for i in 0..nnodes {
            let (idx1, meta1) = recur(v, idx0);
            idx0 = idx1;
            meta += meta1;
        }
    }

    let nmeta = v[idx + 1] as usize;
    if nmeta > 0 {
        for i in 0..nmeta {
            meta += v[idx0 + i];
        }
    }

    return (idx0 + nmeta, meta)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
    }
}
