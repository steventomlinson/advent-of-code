#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone, Copy)]
enum Block {
    Floor,
    Empty,
    Occupied
}

fn build_adj_vec (vec : &Vec<Vec<Block>>, x : usize, y : usize) -> Vec<Block> {
    let mut adj = Vec::<Block>::new();
    if x > 0 {
        adj.push(vec[y][x - 1]);
        if y > 0 {
            adj.push(vec[y - 1][x - 1]);
        }
        if y < vec.len() - 1 {
            adj.push(vec[y + 1][x - 1]);
        }
    }
    if x < vec[y].len() - 1 {
        adj.push(vec[y][x + 1]);
        if y > 0 {
            adj.push(vec[y - 1][x + 1]);
        }
        if y < vec.len() - 1 {
            adj.push(vec[y + 1][x + 1]);
        }
    }

    if y > 0 {
        adj.push(vec[y - 1][x]);
    }
    if y < vec.len() - 1 {
        adj.push(vec[y + 1][x]);
    }
    adj
}

fn other_build_adj_vec (vec : &Vec<Vec<Block>>, x : usize, y : usize) -> Vec<Block> {
    let mut adj = Vec::<Block>::new();
    
    let mut inner_x : usize;
    let mut inner_y : usize;
    if x > 0 {
        inner_x = x - 1;
        inner_y = y;
        while vec[inner_y][inner_x] == Block::Floor && inner_x > 0 {
            inner_x -= 1;
        }
        adj.push(vec[inner_y][inner_x]);
        if y > 0 {
            inner_x = x - 1;
            inner_y = y - 1;        
            while vec[inner_y][inner_x] == Block::Floor && inner_x > 0 && inner_y > 0 {
                inner_x -= 1;
                inner_y -= 1;
            }
            adj.push(vec[inner_y][inner_x]);
        }   
        if y < vec.len() - 1 {            
            inner_x = x - 1;
            inner_y = y + 1;        
            while vec[inner_y][inner_x] == Block::Floor && inner_x > 0 && inner_y < vec.len() - 1 {
                inner_x -= 1;
                inner_y += 1;
            }
            adj.push(vec[inner_y][inner_x]);
        }
    }
    if x < vec[y].len() - 1 {        
        inner_x = x + 1;
        inner_y = y;
        while vec[inner_y][inner_x] == Block::Floor && inner_x < vec[y].len() - 1 {
            inner_x += 1;
        }
        adj.push(vec[inner_y][inner_x]);
        if y > 0 {
            inner_x = x + 1;
            inner_y = y - 1;        
            while vec[inner_y][inner_x] == Block::Floor && inner_x < vec[y].len() - 1 && inner_y > 0 {
                inner_x += 1;
                inner_y -= 1;
            }
            adj.push(vec[inner_y][inner_x]);
        }
        if y < vec.len() - 1 {            
            inner_x = x + 1;
            inner_y = y + 1;        
            while vec[inner_y][inner_x] == Block::Floor && inner_x < vec[y].len() - 1 && inner_y < vec.len() - 1 {
                inner_x += 1;
                inner_y += 1;
            }
            adj.push(vec[inner_y][inner_x]);
        }
    }

    if y > 0 {
        inner_y = y - 1;
        while vec[inner_y][x] == Block::Floor && inner_y > 0 {
            inner_y -= 1;
        }
        adj.push(vec[inner_y][x]);
    }
    if y < vec.len() - 1 {
        inner_y = y + 1;
        while vec[inner_y][x] == Block::Floor && inner_y < vec.len() - 1 {
            inner_y += 1;
        }
        adj.push(vec[inner_y][x]);
    }
    adj
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");

    let grid : Vec<Vec<Block>> = contents.lines().map(|s| s.chars().map(|c| 
        match c {
            'L' => Block::Empty,
            '#' => Block::Occupied,
            _   => Block::Floor
        }
    ).collect()).collect();

    let mut mut_grid = grid.clone();
    let mut change_count = 1;
    while change_count != 0 {
        change_count = 0;
        let old_grid = mut_grid.clone();
        for (y, row) in old_grid.iter().enumerate() {
            for (x, block) in row.iter().enumerate() {
                let adj = build_adj_vec(&old_grid, x, y);
                let ocount = adj.iter().filter(|&b| *b == Block::Occupied).count();
                match block {
                    Block::Empty => {
                        if ocount == 0 {
                            change_count += 1;
                            mut_grid[y][x] = Block::Occupied;
                        }
                    },
                    Block::Occupied => {
                        if ocount >= 4 {
                            change_count += 1;
                            mut_grid[y][x] = Block::Empty;
                        }
                    },
                    Block::Floor => ()
                };
            }
        }
    }

    let result1 = {
        mut_grid.iter().map(|row| row.iter().filter(|&b| *b == Block::Occupied).count()).sum::<usize>()
    };

    mut_grid = grid;
    change_count = 1;
    while change_count != 0 {
        change_count = 0;
        let old_grid = mut_grid.clone();
        for (y, row) in old_grid.iter().enumerate() {
            for (x, block) in row.iter().enumerate() {
                let adj = other_build_adj_vec(&old_grid, x, y);
                let ocount = adj.iter().filter(|&b| *b == Block::Occupied).count();
                match block {
                    Block::Empty => {
                        if ocount == 0 {
                            change_count += 1;
                            mut_grid[y][x] = Block::Occupied;
                        }
                    },
                    Block::Occupied => {
                        if ocount >= 5 {
                            change_count += 1;
                            mut_grid[y][x] = Block::Empty;
                        }
                    },
                    Block::Floor => ()
                };
            }
        }
    }

    let result2 = {
        mut_grid.iter().map(|row| row.iter().filter(|&b| *b == Block::Occupied).count()).sum::<usize>()
    };

    println!("part1 = {}", result1);
    println!("part2 = {}", result2);

}
