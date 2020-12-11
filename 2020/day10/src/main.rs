use std::collections::HashMap;

fn routes (ints : &Vec<usize>, idx : usize, seen : &mut HashMap<usize, usize>) -> usize {
    if idx == ints.len() - 1 {
        1
    }
    else if let Some(count) = seen.get(&idx) {
        *count
    }
    else {
        let current = ints[idx];
        let x1op = ints.get(idx + 1);
        let x2op = ints.get(idx + 2);
        let x3op = ints.get(idx + 3);
        let mut count = 0;
        if let Some(x1) = x1op {
            let diff = x1 - current;
            if 1 <= diff && diff <= 3 {
                count += routes(ints, idx + 1, seen);
            }
        }
        if let Some(x2) = x2op {
            let diff = x2 - current;
            if 1 <= diff && diff <= 3 {
                count += routes(ints, idx + 2, seen);
            }
        }
        if let Some(x3) = x3op {
            let diff = x3 - current;
            if 1 <= diff && diff <= 3 {
                count += routes(ints, idx + 3, seen);
            }
        }
        seen.insert(idx, count);
        count
    }
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let mut ints : Vec<usize> = contents.lines().map(|s| s.parse::<usize>().unwrap()).collect();
    ints.sort();
    let device_jolt = ints.last().unwrap() + 3;

    let mut chain : Vec<usize> = Vec::new();
    let mut jolt1count = 0;
    let mut jolt3count = 0;
    let mut current = 0;
    ints.push(device_jolt);
    for x in &ints {
        let diff = x - current;
        if (1..=3).contains(&diff) {
            if diff == 1 {
                jolt1count += 1;
            }
            else if diff == 3 {
                jolt3count += 1;
            }
            chain.push(*x);
            current = *x;
        }
    }
    ints.insert(0, 0);
    let r : &mut HashMap<usize, usize> = &mut HashMap::new();
    
    println!("{} {}", jolt1count, jolt3count);
    println!("part1 = {}", jolt1count * jolt3count);
    println!("part2 = {}", routes(&ints, 0, r));

}
