use std::{
    str,
    // convert::TryFrom,
    // collections::VecDeque,
};

// use super::Grid;

pub fn runpart1(v: Vec<(i64, i64)>) -> Result<i64, &'static str> {
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
    
    let mut vres = vec![(0, false); v.len()];

    for j in y0..(y1 + 1) {
        for i in x0..(x1 + 1) {
            let excl = (j == y0) || (j == y1) || (i == x0) || (i == x1);
            let mut mind = y1 - y0 + x1 - x0 + 1; // this is our infinity
            let mut cnt = 0;
            let mut id = (v.len() + 1) as usize; 
            for el in v.iter().enumerate() {
                let d = dist(&(i, j), el.1);
                if d < mind {
                    mind = d;
                    cnt = 1;
                    id = el.0;
                } else if d == mind {
                    cnt += 1;
                }
            }
            if cnt == 1 {
                vres[id] = (vres[id].0 + 1, vres[id].1 || excl)
            }
        }
    }

    let res = vres.iter().filter(|x| !x.1).max_by_key(|x| x.0).unwrap().0;

    Ok(res)
}

fn dist(x: &(i64, i64), y: &(i64, i64)) -> i64 {
    (x.0 - y.0).abs() + (x.1 - y.1).abs()
}

//////////////////////////////////////////////////////////////////
// This is failed version, but I leave it here, for future references
//////////////////////////////////////////////////////////////////

// pub fn runpart1(v: Vec<(i64, i64)>) -> Result<i64, &'static str> {
//     let x0 = match v.iter().min_by_key(|x| x.0) {
//         Some(x) => x.0,
//         None => return Err("Vector is empty!"),
//     };
//     let x1 = match v.iter().max_by_key(|x| x.0) {
//         Some(x) => x.0,
//         None => return Err("Vector is empty!"),
//     };
//     let y0 = match v.iter().min_by_key(|x| x.1) {
//         Some(x) => x.1,
//         None => return Err("Vector is empty!"),
//     };
//     let y1 = match v.iter().max_by_key(|x| x.1) {
//         Some(x) => x.1,
//         None => return Err("Vector is empty!"),
//     };

//     println!("Rectangle: ({}, {}) - ({}, {})", x0, y0, x1, y1);
//     let mut m = super::Grid::new(x1 - x0 + 1, y1 - y0 + 1, x0, y0, (usize::try_from(0).unwrap(), y1 - y0 + x1 - x0 + 2));
//     let mut caps = Vec::new();

//     for el in v.iter().enumerate() {
//         m[*el.1] = (el.0 + 1, 0);
//         let mut seed = VecDeque::new();
//         seed.push_back((el.0 + 1, 0, (el.1).0, (el.1).1));
//         caps.push(seed);
//     }

//     loop {
//         let mut done = true;
//         for seed in caps.iter_mut() {
//             if seed.is_empty() {
//                 continue;
//             }
//             done = false;
//             let (id, lev, x, y) = seed.pop_front().unwrap();
//             if solder(&mut m, (id, lev + 1, x + 1, y)) {
//                 seed.push_back((id, lev + 1, x + 1, y));
//             }
//             if solder(&mut m, (id, lev + 1, x - 1, y)) {
//                 seed.push_back((id, lev + 1, x - 1, y));
//             };
//             if solder(&mut m, (id, lev + 1, x, y + 1)) {
//                 seed.push_back((id, lev + 1, x, y + 1));
//             };
//             if solder(&mut m, (id, lev + 1, x, y - 1)) {
//                 seed.push_back((id, lev + 1, x, y - 1));
//             };
//         }

//         if done {
//             break;
//         }
//     }

//     let mut res = Vec::new();
//     for i in 0..v.len() {
//         res.push((0, false));
//     }

//     for j in y0..(y1 - y0 + 2) {
//         for i in x0..(x1 - x0 + 2) {
//             let id = (m[(i, j)].0 as i64) - 1;
//             if id != -1 {
//                 let excl = (j == y0) || (j == y1 - y0 + 1) || (i == x0) || (i == x1 - x0 + 1);
//                 let id1 = (id as usize);
//                 res[id1] = (res[id1].0 + 1, excl);
//             }
//         }
//     }
    
//     for j in y0..(y1 - y0 + 2) {
//         for i in x0..(x1 - x0 + 2) {
//             print!("{}", m[(i, j)].0);
//         }
//         print!("\n");
//     }

//     println!("{:?}", res);

//     let res = res.iter().filter(|x| !x.1).max_by_key(|x| x.0).unwrap().0;
//     println!("{:?}", res);

//     Ok(res)
// }

// fn solder(m: &mut Grid<(usize, i64)>, el: (usize, i64, i64, i64)) -> bool {
//     if el.2 < m.ox {
//         return false;
//     }

//     if el.2 >= (m.ox as i64) + (m.width as i64) {
//         return false;
//     }

//     if el.3 < m.oy {
//         return false;
//     }

//     if el.3 >= m.oy + (m.height as i64) {
//         return false;
//     }

//     // println!("WTF??? ({}, {}) : ({}, {}) : ({}, {})", el.2, el.3, m.ox, m.oy, m.ox + (m.width as i64), m.oy + (m.height as i64));
//     let v = m[(el.2, el.3)];
//     // println!("Иии? ({}, {}) : ({}, {})", el.0, el.1, v.0, v.1);

//     if v.1 < el.1 {
//         return false;
//     }

//     if v.0 == el.0 {
//         return false;
//     }

//     if v.1 == el.1 {
//         m[(el.2, el.3)] = (0, el.1);
//         return true;
//     }

//     m[(el.2, el.3)] = (el.0, el.1);
//     return true;
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
    }
}
