fn calculate_route(map : &Vec<&str>, (xstep, ystep) : (usize, usize)) -> String {
    let mut result : String = "".to_owned();
    // We know the map is infinite in the x direction and loops so we'll get the tile width
    let width = map.first().unwrap().len();
    let height = map.len();

    let mut x = 0;
    let mut y = 0;
    while y < height {
        result.push(map.get(y).unwrap().chars().nth(x).unwrap());

        x += xstep;
        if x >= width {
            x = x - width;
        }

        y += ystep;
    }

    return result;
}

fn do_part1 (map : &Vec<&str>, step : (usize, usize)) -> usize {
    return calculate_route(map, step).matches("#").count()
}

fn main() {   
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    let lines : Vec<&str> = contents.lines().collect();

    let result1 = do_part1(&lines, (3, 1));
    println!("part1 = {}", result1);

    let result2 = result1 * do_part1(&lines, (1, 1)) * do_part1(&lines, (5, 1)) * do_part1(&lines, (7, 1)) * do_part1(&lines, (1, 2));
    println!("part2 = {}", result2);
}
