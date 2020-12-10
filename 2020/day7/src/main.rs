use std::collections::HashMap;

fn depth_first_search(map : &HashMap<&str, HashMap<&str, usize>>, key : &str, item : &str) -> bool {
    match map.get(key) {
        Some(innermap) => {
            if innermap.contains_key(item) {
                true
            }
            else {
                let mut result = false;
                for value in innermap.keys() {
                    if depth_first_search(map, value, item) {
                        result = true;
                        break;
                    }
                }
                result
            }},
        None => {
            false
        }
    }
}

fn count_bags(map : &HashMap<&str, HashMap<&str, usize>>, key : &str) -> usize {
    match map.get(key) {
        Some(innermap) => {
            let mut count = 0;
            for (bag, num) in innermap {
                count += num + (num * count_bags(map, bag));
            }
            count
        },
        None => 0
    }
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");

    let lines : Vec<&str> = contents.lines().map(|s| s.strip_suffix(".").unwrap()).collect();
    let mut map : HashMap<&str, HashMap<&str, usize>> = HashMap::new();
    for line in lines {
        let vec : Vec<&str> = line.splitn(2, "contain").collect();
        assert_eq!(vec.len(), 2);
        let parent = vec.first().unwrap().trim().trim_end_matches('s');
        let mut children : Vec<(&str, usize)> = vec.last().unwrap().split(",").map(|s| {
            let pair : Vec<&str>= s.trim().trim_end_matches('s').splitn(2, " ").collect();
            let first = match  pair.first().unwrap().parse::<usize>() {
                Ok(i) => i,
                Err(_) => 0
            };
            (pair.last().cloned().unwrap(),first)
        }).collect();
        children.retain(|(_, i)| *i != 0);
        if children.len() > 0 {
            map.insert(parent, match map.get(parent) {
                Some(innermap) => innermap.to_owned(),
                None => children.iter().cloned().collect()
            });
        }
    }
    let mut result1 = 0;
    for key in map.keys() {
        if depth_first_search(&map, key, "shiny gold bag") {
            result1 += 1;
        }
    }
    println!("part1 = {}", result1);

    let result2 = count_bags(&map, "shiny gold bag");
    println!("part2 = {}", result2);
}
