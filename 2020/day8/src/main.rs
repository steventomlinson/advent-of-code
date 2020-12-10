use std::collections::HashSet;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone, Copy)]
enum OpCode {
    acc,
    jmp,
    nop
}

#[derive(Debug)]
#[derive(Clone, Copy)]
struct Instruction {
    oc : OpCode,
    d  : i32,
}

struct Console {
    areg : i32,
    pc   : i32,
    code : Vec<Instruction>
}

fn parse_instruction(s : &str) -> Result<Instruction, &str> {
    let toks : Vec<&str> = s.split_whitespace().collect();
    if toks.len() != 2 {
        return Err("");
    } else {
        let raw_instr : (&str, Result<i32, _>) = (toks.first().unwrap(), toks.last().unwrap().parse::<i32>());

        match raw_instr {
            ("acc", Ok(i)) => Ok(Instruction {
                oc : OpCode::acc,
                d : i
            }),
            ("jmp", Ok(i)) => Ok(Instruction {
                oc : OpCode::jmp,
                d : i
            }),
            ("nop", Ok(_)) => Ok(Instruction {
                oc : OpCode::nop,
                d : 0
            }),
            _ => Err("")
        }
    }
}

fn parse_input (s : &str) -> Result<Vec<Instruction>, &str> {
    s.lines().map(parse_instruction).collect()
}

fn main() {
    let args : Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let filename : &String = &args[1];
    let contents : String = std::fs::read_to_string(filename)
        .expect("Unable to read file!");

    let mut console = Console {
        areg : 0,
        pc   : 0,
        code : parse_input(&contents).unwrap()
    };
    let mut set : HashSet<i32> =  HashSet::new();
    while !set.contains(&console.pc) {
        set.insert(console.pc);
        let instr : &Instruction = console.code.get(console.pc as usize).unwrap();
        match instr {
            Instruction{oc : OpCode::acc, d} => {console.areg += d; console.pc += 1;},
            Instruction{oc : OpCode::jmp, d} => {console.pc += d},
            Instruction{oc : OpCode::nop, d:_} => console.pc += 1
        };
    }
    println!("part1 = {}", console.areg);

    let mut terminated = false;
    let mut idx = 0;

    while !terminated {
        while console.code.get(idx).unwrap().oc == OpCode::acc {
            idx += 1;
        }

        let instr = console.code[idx];
        console.code[idx].oc = match instr {
            Instruction{oc:OpCode::jmp, d:_} => OpCode::nop,
            Instruction{oc:OpCode::nop, d:_} => OpCode::jmp,
            Instruction{oc, d:_} => oc,
        };

        set.clear();
        console.areg = 0;
        console.pc = 0;

        while !set.contains(&console.pc) {
            set.insert(console.pc);
            let instr : &Instruction = &console.code.get(console.pc as usize).unwrap();
            match instr {
                Instruction{oc : OpCode::acc, d} => {console.areg += d; console.pc += 1;},
                Instruction{oc : OpCode::jmp, d} => {console.pc += d},
                Instruction{oc : OpCode::nop, d:_} => console.pc += 1
            };
            if console.pc as usize == console.code.len() {
                terminated = true;
                break;
            }
        }        
        console.code[idx].oc = instr.oc;
        idx += 1;
    }
    println!("part1 = {}", console.areg);


}
