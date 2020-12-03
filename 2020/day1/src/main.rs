fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let mut ints : Vec<i32> = contents.lines().map(|s| s.parse::<i32>().unwrap()).collect();

    ints.sort();

    let mut i = 0;
    let mut j = ints.len() - 1;
    let mut result;
    loop {
        let a = ints[i];
        let b = ints[j];
        let c = a + b;
        if c == 2020 {
            result = a * b;
            break;
        }
        else if c > 2020 {
            j = j - 1;
            i = 0;
        }
        else {
            i = i + 1;
        }
    }
    println!("part1 = {}", result);

    i = 0;
    j = ints.len() - 1;
    let mut k = i + 1;
    loop {
        let a = ints[i];
        let b = ints[j];
        let c = ints[k];
        let d = a + b + c;
        if d == 2020 {
            result = a * b * c;
            break;
        }
        else if d > 2020 {
            if i + 1 == k {
                j = j - 1;
                i = 0;
                k = 1;
            } else {
                i = i + 1;
                k = i + 1;
            }
        }
        else {
            k = k + 1;
        }
    }
    println!("part2 = {}", result);
}
