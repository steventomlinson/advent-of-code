fn to_hash_map<T : std::iter::Iterator<Item=String>> (iter : T) -> std::collections::HashMap<String, String> {
    let mut result = std::collections::HashMap::new();
    for item in iter {
        let item : Vec<&str> = item.split(":").collect();
        result.insert(item.get(0).unwrap().to_string(), item.get(1).unwrap().to_string());
    }

    return result;
}

fn is_number_range(s : String, (l, u) : (i32, i32)) -> bool {
    let ir = s.parse::<i32>();
    return match ir {
        Ok(i) => l <= i && i <= u,
        Err(_) => false
    };
}

fn check_height(s : String) -> bool {
    return match s.split_at(s.len() - 2) {
        (s, "cm") => is_number_range(s.to_owned(), (150, 193)),
        (s, "in") => is_number_range(s.to_owned(), (59, 76)),
        _ => false
    }
}

fn check_rgb_color(s : String) -> bool {
    return match s.split_at(1) {
        ("#", s) => {
            if s.len() == 6 {
                s.chars().all(|c| char::is_numeric(c) || ('a' <= c  && c <= 'f'))
            }
            else {
                false
            }
        }
        _ => false
    }
} 

fn valid_values(set : &std::collections::HashMap<String, String>) -> bool {
    let byr = set.get("byr").unwrap().to_owned();
    let iyr = set.get("iyr").unwrap().to_owned();
    let eyr = set.get("eyr").unwrap().to_owned();
    let hgt = set.get("hgt").unwrap().to_owned();
    let hcl = set.get("hcl").unwrap().to_owned();
    let ecl = set.get("ecl").unwrap().to_owned();
    let pid = set.get("pid").unwrap().to_owned();
    
    

    return 
        is_number_range(byr, (1920, 2020)) &&
        is_number_range(iyr, (2010, 2020)) &&
        is_number_range(eyr, (2020, 2030)) &&
        check_height(hgt) &&
        check_rgb_color(hcl) && 
        ["amb".to_owned(), "blu".to_owned(), "brn".to_owned(), "gry".to_owned(), "grn".to_owned(), "hzl".to_owned(), "oth".to_owned()].contains(&ecl) &&
        (&pid).len() == 9 && pid.chars().all(char::is_numeric);
}

fn main() {    
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let passports : Vec<std::collections::HashMap<String, String>> = contents.split("\n\n").map(|s| to_hash_map(s.split_whitespace().map(|s| s.to_owned()))).collect();
    println!("{:?}", passports);
    let set : std::collections::HashSet<String> = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().cloned().map(|s| s.to_string()).collect();
    let mut result1 : i32 = 0;
    let mut result2 : i32 = 0;
    for x in passports {
        if set.is_subset(&x.keys().cloned().collect()) {
            result1 += 1;
            if valid_values(&x) {
                result2 += 1;
            }
        }
    }
    println!("part1 = {}", result1);
    println!("part2 = {}", result2);
}
