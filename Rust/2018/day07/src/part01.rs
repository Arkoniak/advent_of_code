use std::collections::{
    HashMap,
};

#[derive(Debug)]
struct Node {
    parents: Vec<String>,
    children: Vec<String>,
    ready: bool,
}

pub fn runpart1(v: Vec<(String, String)>) -> Result<String, &'static str> {
    let mut g = HashMap::new();

    for el in v {
        let p = g.entry(el.0.clone()).or_insert(Node {parents: Vec::new(), children: Vec::new(), ready: false});
        p.children.push(el.1.clone());
        let c = g.entry(el.1).or_insert(Node {parents: Vec::new(), children: Vec::new(), ready: false});
        c.parents.push(el.0.clone());
    }
    
    let mut res = Vec::new();
    let mut stack = find_roots(&g);

    while !stack.is_empty() {
        stack.sort_by(|a, b| b.cmp(a));
        let c = stack.pop().unwrap();
        g.get_mut(&c).unwrap().ready = true;

        for nc in &g.get(&c).unwrap().children {
            if is_ready(&g, &nc) {
                stack.push(nc.clone());
            }
        }
        res.push(c);
    }

    Ok(res.join(""))
}

fn is_ready(g: &HashMap<String, Node>, nc: &String) -> bool {
    for p in &g.get(nc).unwrap().parents {
        if !&g.get(p).unwrap().ready {
            return false;
        }
    }

    return true
}

fn find_roots(g: &HashMap<String, Node>) -> Vec<String> {
    let mut res = Vec::new();
    for (k, v) in g.iter() {
        if v.parents.is_empty() {
            res.push(k.clone());
        }
    }
    
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
    }
}
