use num::bigint::{ToBigInt};
//use num::bigint::BigInt;
use num::BigInt;
use num::Integer;

fn bc (a : &BigInt, b : &BigInt) -> (BigInt, BigInt) {
    let mut r  : BigInt = a.clone();
    let mut rp : BigInt = b.clone();
    let mut u  : BigInt = 1.to_bigint().unwrap();
    let mut v  : BigInt = 0.to_bigint().unwrap();
    let mut up : BigInt = 0.to_bigint().unwrap();
    let mut vp : BigInt = 1.to_bigint().unwrap();
    while rp != 0.to_bigint().unwrap() {
        let q = &r/ &rp;
        let rs = r;
        let us = u;
        let vs = v;
        r = rp.clone();
        u = up.clone();
        v = vp.clone();
        rp = rs - &q * rp;
        up = us - &q * up;
        vp = vs - &q * vp;
    }
    (u, v)
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");
    
    let timestamp : usize = contents.lines().nth(0).unwrap().parse().unwrap();
    let cycles = contents.lines().nth(1).unwrap().split(',').filter(|c| *c != "x").map(|s| {
        let i : usize= s.parse().unwrap();
        (i, i - timestamp % i)
    });
    let mut item = (0, 0);
    for (id, rem) in cycles {
        if rem < item.1 || item.0 == 0{
            item = (id, rem)
        }
    }
    
    println!("part1 = {}", item.0 * item.1);
    
    let new_cycles : Vec<(BigInt, BigInt)> = contents.lines().nth(1).unwrap().split(',').enumerate().filter(|(_, s)| *s != "x").map(|(idx, s)| {
        let i : BigInt= s.parse().unwrap();
        (idx.to_bigint().unwrap(), i)
    }).collect();


    let mut prod : BigInt= 1.to_bigint().unwrap();
    let mut a : BigInt = 0.to_bigint().unwrap();
    for (idx, i) in new_cycles {
        println!("{} {}", prod, a);
        let (u, v) = bc(&prod, &i);
        a = (a * &i * v) + (-idx * &prod * u);
        prod = i * prod;
    }

    println!("{} {}", prod, a);
    println!("{}", a.mod_floor(&prod));
}
