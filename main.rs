extern crate regex;
use regex::Regex;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::vec::Vec;
use std::str::Chars;
use std::iter::Peekable;
use std::rc::Rc;

mod Grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug)]
pub enum Expression {
    Integer(i64)
}

pub enum Type {
    
}

#[derive(Debug)]
pub enum AstNode {
    Block {
        statements : Vec<Rc<AstNode>>
    },
    If {
      condition : Rc<Expression>,
      statement : Rc<AstNode>
    },
    FnDef {
        name: String,
        returnType: Type
    }
}

fn main() {
    match Grammar::fn_definition("fn a() -> i8") {
        Ok(r) => println!("Parsed as: {:?}", r),
        Err(e) => println!("Parse error: {}", e)
    }

    let filename = "test.65";

    println!("In file {}", filename);

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    println!("With text:\n{}", contents);
}
