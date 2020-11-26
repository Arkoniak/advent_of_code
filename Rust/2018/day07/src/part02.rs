use std::collections::{
    HashMap,
};

use std::cmp::{
    Ord,
    Ordering,
    max,
};

// https://stackoverflow.com/questions/29884402/how-do-i-implement-ord-for-a-struct
#[derive(Debug, PartialOrd, Eq, Clone)]
struct Job {
    name: String,
    start: i64,
    end: i64,
    ready: bool,
}

impl Job {
    pub fn new(name: String) -> Self {
        Job {name: name, start: 0, end: 0, ready: false}
    }
}

impl Ord for Job {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.start, &self.name).cmp(&(other.start, &other.name))
    }
}

impl PartialEq for Job {
    fn eq(&self, other: &Self) -> bool {
        (self.start, &self.name) == (other.start, &other.name)
    }
}

#[derive(Debug)]
struct Node {
    parents: Vec<String>,
    children: Vec<String>,
    job: Job,
}

impl Node {
    pub fn new(name: String) -> Self {
        Node {
            parents: Vec::new(),
            children: Vec::new(),
            job: Job::new(name),
        }
    }
}

pub fn runpart2(v: Vec<(String, String)>) -> Result<i64, &'static str> {
    let mut g = HashMap::new();

    for el in v {
        let p = g.entry(el.0.clone()).or_insert(Node::new(el.0.clone()));
        p.children.push(el.1.clone());
        let c = g.entry(el.1.clone()).or_insert(Node::new(el.1.clone()));
        c.parents.push(el.0.clone());
    }

    let mut workers = [0; 5];
    let mut res = Vec::new();
    let mut stack = find_roots(&g);

    while !stack.is_empty() {
        stack.sort_by(|a, b| b.cmp(a));
        let cname = stack.pop().unwrap().name;
        {
            let c0 = g.get_mut(&cname).unwrap();
            let c = &mut c0.job;
            c.ready = true;
            let worker_id = position_min_copy(&workers);
            c.start = max(workers[worker_id], c.start);
            c.end = c.start + get_weight(&cname);
            workers[worker_id] = c.end;
            res.push(c.clone());
        }

        let children = g.get(&cname).unwrap().children.clone();

        for nc in children {
            let (is_ready, start) = ready(&g, &nc);
            if is_ready {
                let mut job = &mut g.get_mut(&nc).unwrap().job;
                job.start = start;
                stack.push(job.clone());
            }
        }
    }
    
    // println!("{:?}", res);
    Ok(res[res.len() - 1].end)
}

fn get_weight(c: &String) -> i64 {
    c.chars().next().unwrap() as i64 - 'A' as i64 + 1 + 60
}

fn find_roots(g: &HashMap<String, Node>) -> Vec<Job> {
    let mut res = Vec::new();
    for v in g.values() {
        if v.parents.is_empty() {
            res.push(v.job.clone());
        }
    }
    
    res
}

fn ready(g: &HashMap<String, Node>, nc: &String) -> (bool, i64) {
    let mut maxend = 0;
    for p in &g.get(nc).unwrap().parents {
        let job: &Job = &g.get(p).unwrap().job;
        if !job.ready {
            return (false, maxend);
        }
        if job.end > maxend {
            maxend = job.end;
        }
    }

    return (true, maxend)
}

// https://stackoverflow.com/questions/57813951/whats-the-fastest-way-of-finding-the-index-of-the-maximum-value-in-an-array
fn position_min_copy<T: Ord + Copy>(slice: &[T]) -> usize {
    slice.iter().enumerate().min_by_key(|(_, &value)| value).map(|(idx, _)| idx).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let workers = vec![10, 15, 5, 20];
        assert_eq!(position_min_copy(&workers), 2);
    }
}
