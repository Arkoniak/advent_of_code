use std::str;

pub fn runpart2(v: Vec<i64>) -> Result<i64, &'static str> {
    let res = recur(&v, 0);

    Ok(res.1)
}

fn recur(v: &Vec<i64>, idx: usize) -> (usize, i64) {
    let nnodes = v[idx];
    let nmeta = v[idx + 1] as usize;
    let mut idx0 = idx + 2;
    let mut meta = 0;
    let mut metas: Vec<i64> = Vec::new();
    if nnodes > 0 {
        for _ in 0..nnodes {
            let (idx1, meta1) = recur(v, idx0);
            idx0 = idx1;
            metas.push(meta1);
        }
    } else {
        for i in 0..nmeta {
            meta += v[idx0 + i];
        }

        return (idx0 + nmeta, meta);
    }

    if nmeta > 0 {
        for i in 0..nmeta {
            let id = (v[idx0 + i] - 1) as usize;
            if id < metas.len() {
                meta += metas[id];
            }
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
