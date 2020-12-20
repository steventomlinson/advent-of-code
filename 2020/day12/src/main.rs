
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone, Copy)]
struct Point {
    x : isize,
    y : isize
}

struct Ship {
    loc : Point,
    rot : isize
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone, Copy)]
enum ActionOp {
    North,
    South,
    East,
    West,
    Left,
    Right,
    Forward
}


impl std::str::FromStr for ActionOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "N" => Ok(ActionOp::North),
            "S" => Ok(ActionOp::South),
            "E" => Ok(ActionOp::East),
            "W" => Ok(ActionOp::West),
            "L" => Ok(ActionOp::Left),
            "R" => Ok(ActionOp::Right),
            "F" => Ok(ActionOp::Forward),
            _ => Err({let mut ss = "Unknown op ".to_owned(); ss.push_str(s); ss})
        }
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone, Copy)]
struct Action {
    op : ActionOp,
    d  : isize
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    
    let actions = contents.lines().map(|s| {
        let (ops, datas) = s.split_at(1);
        Action {
            op : ops.parse::<ActionOp>().unwrap(),
            d  : datas.parse::<isize>().unwrap()
        }
    });

    let mut ship = Ship {
        loc : Point {
            x : 0,
            y : 0
        },
        rot : 0
    };

    for action in actions {
        match action.op {
            ActionOp::North => ship.loc.y += action.d,
            ActionOp::South => ship.loc.y -= action.d,
            ActionOp::East => ship.loc.x += action.d,
            ActionOp::West =>ship.loc.x -= action.d,
            ActionOp::Left => if ship.rot + action.d >= 360 {
                ship.rot = ship.rot + action.d - 360;
            }
            else {
                ship.rot += action.d
            },
            ActionOp::Right => if ship.rot - action.d < 0 {
                ship.rot = ship.rot - action.d + 360;
            }
            else {
                ship.rot -= action.d
            },
            ActionOp::Forward => {
                match ship.rot {
                    0   => ship.loc.x += action.d,
                    90  => ship.loc.y += action.d,
                    180 => ship.loc.x -= action.d,
                    270 => ship.loc.y -= action.d,
                    _   => ()
                }
            }
        }
    }

    println!("part1 = {}", ship.loc.x.abs() + ship.loc.y.abs());

    let actions = contents.lines().map(|s| {
        let (ops, datas) = s.split_at(1);
        Action {
            op : ops.parse::<ActionOp>().unwrap(),
            d  : datas.parse::<isize>().unwrap()
        }
    });

    let mut waypoint = Point {
        x : 10,
        y : 1
    };

    let mut ship = Ship {
        loc : Point {
            x : 0,
            y : 0
        },
        rot : 0
    };

    for action in actions {
        match action.op {
            ActionOp::North => waypoint.y += action.d,
            ActionOp::South => waypoint.y -= action.d,
            ActionOp::East => waypoint.x += action.d,
            ActionOp::West =>waypoint.x -= action.d,
            ActionOp::Left => match action.d {
                90  => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x - ydiff;
                    waypoint.y = ship.loc.y + xdiff;
                }
                180 => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x - xdiff;
                    waypoint.y = ship.loc.y - ydiff;
                }
                270 => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x + ydiff;
                    waypoint.y = ship.loc.y - xdiff;
                }
                _ => ()
            },
            ActionOp::Right => match action.d {
                90  => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x + ydiff;
                    waypoint.y = ship.loc.y - xdiff;
                }
                180 => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x - xdiff;
                    waypoint.y = ship.loc.y - ydiff;
                }
                270 => {
                    let xdiff = waypoint.x - ship.loc.x;
                    let ydiff = waypoint.y - ship.loc.y;
                    waypoint.x = ship.loc.x - ydiff;
                    waypoint.y = ship.loc.y + xdiff;
                }
                _ => ()
            },
            ActionOp::Forward => {
                let xdiff = waypoint.x - ship.loc.x;
                let ydiff = waypoint.y - ship.loc.y;
                ship.loc.x += action.d * (waypoint.x - ship.loc.x);
                ship.loc.y += action.d * (waypoint.y - ship.loc.y);
                waypoint.x = ship.loc.x + xdiff;
                waypoint.y = ship.loc.y + ydiff;
            },
        }
        
        println!("{:?} {:?}", ship.loc, waypoint);
    }

    
    println!("part2 = {}", ship.loc.x.abs() + ship.loc.y.abs());

}
