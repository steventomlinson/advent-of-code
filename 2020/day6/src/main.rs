use std::collections::HashSet;
use std::collections::HashMap;
fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let groups: Vec<Vec<&str>> = contents.split("\n\n").map(|s| s.lines().collect()).collect();

    let mut result1 = 0;
    for group in &groups {
        let mut set : HashSet<char> = HashSet::<char>::new();
        for person in group {
            let nset : HashSet<char> = person.chars().collect();
            set = set.union(&nset).cloned().collect();
        }
        result1 += set.len();
    }

    let mut result2 = 0;
    for group in groups {
        let mut map : HashMap<char, usize> = HashMap::<char, usize>::new();
        for person in &group {
            for c in person.chars() {
                map.insert(c, match map.get(&c) {
                    Some(v) => v.clone() + 1,
                    None => 1
                });
            }
        }
        result2 += map.values().filter(|u| (*u).clone() == group.len()).count();
    }
    println!("part1 = {}", result1);
    println!("part2 = {}", result2);
}
