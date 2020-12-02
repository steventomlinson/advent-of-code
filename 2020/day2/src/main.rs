#[derive(Debug)]
struct Item{
    a: i32,
    b: i32,
    c: char,
    s: String
}

fn parse_item(s : &str) -> Item {
    let v : Vec<&str> = s.split(" ").collect();
    assert_eq!(v.len(), 3);
    let (a, b) = {
        let ss : Vec<&str> = v[0].split("-").collect();
        (ss[0].parse::<i32>().unwrap(), ss[1].parse::<i32>().unwrap(),)
    };

    return Item{
        a: a,
        b: b,
        c: v[1].chars().next().unwrap(),
        s: v[2].to_string(),
    };
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let items : Vec<Item> = contents.lines().map(|s| parse_item(s)).collect();
    let mut result = 0;
    for x in &items {
        let count : i32 = x.s.matches(x.c).count() as i32;
        if x.a <= count && count <= x.b {
            result += 1;
        }
    }
    println!("part1 = {}", result);

    result = 0;
    for x in &items {
        if (x.s.chars().nth((x.a - 1)  as usize).unwrap() == x.c) ^ (x.s.chars().nth((x.b - 1) as usize).unwrap() == x.c) {
            result += 1;
        }
    }
    println!("part2 = {}", result);
}
