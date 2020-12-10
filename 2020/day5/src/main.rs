#[derive(Eq)]
struct Point {
    row : usize,
    col : usize
}

impl Ord for Point {
    fn cmp(&self, other : &Self) -> std::cmp::Ordering {
        let lhs = self.row * 8 + self.col;
        let rhs = other.row * 8 + other.col;
        lhs.cmp(&rhs)
    }
}

impl PartialOrd  for Point {
    fn partial_cmp(&self, other : &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        (self.row * 8 + self.col) == (other.row * 8 + other.col)
    }
}

fn do_search (s : &str, l : usize, r : usize, lc : char, hc: char) -> usize {
    let mut _l = l;
    let mut _r = r;
    for c in s.chars() {
        if c == lc {
            _r = (_l + _r) / 2;
        }
        else if c == hc {
            _l = ((_l + _r) / 2) + 1;
        }
    }

    return (_l + _r) / 2;
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");

    let mut seats : Vec<Point> = contents.lines().map(|s| {
        let (lhs, rhs) = s.split_at(7);
        Point {
            row : do_search (lhs, 0, 127, 'F', 'B'),
            col : do_search(rhs, 0, 7, 'L', 'R')
        }
    }).collect();
    seats.sort();

    let result1 = (&seats).iter().max().unwrap();

    let mut old_id = 0;
    let mut result2 : usize = 0;
    for point in &seats {
        let id = point.row * 8 + point.col;
        if old_id + 2== id{
            result2 = old_id + 1;
            break;
        }
        old_id = id;
    }

    println!("part1 = {}", result1.row * 8 + result1.col);
    println!("part2 = {}", result2);

}
