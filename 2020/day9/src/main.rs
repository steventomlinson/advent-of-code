fn find_invalid(vec : Vec<usize>, preamble : usize) -> (usize, usize) {
    let idx = preamble;
    let mut first = 0;
    for n in idx ..vec.len() {
        let item = vec[n];
        let subvec : Vec<usize> = vec[n-preamble..n].to_vec();
        let mut found = false;
        for (nidx, x) in subvec.iter().enumerate() {
            let mut newsubvec = subvec.clone();
            newsubvec.remove(nidx);
            newsubvec.sort();
            if item > *x && newsubvec.binary_search(&(item - x)).is_ok() {
                found = true;
                break
            }
        }
        if !found {
            first = item;
        }
    }

    let mut l = 0;
    let mut h = 1;
    while vec[l..=h].iter().cloned().sum::<usize>() != first {
        l += 1;
        h += 1;
        if h == vec.len() {
            h = h - l + 1;
            l = 0;
        }
    }
    let mut range = vec[l..=h].to_vec();
    range.sort();
    (first, range.first().unwrap() + range.last().unwrap())

}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let ints : Vec<usize> = contents.lines().map(|s| s.parse::<usize>().unwrap()).collect();

    let (result1, result2) = find_invalid(ints, 25);
    println!("part1 = {}", result1);
    println!("part2 = {}", result2);
}
