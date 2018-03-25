#![feature(nll)]
#![feature(underscore_lifetimes)]

extern crate core;
extern crate typed_arena;
use std::fs::File;
use std::io::prelude::*;
use std::vec::Vec;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::HashMap;
use typed_arena::Arena;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug)]
pub struct Arg<'a> {
    value : &'a Expression<'a>,
    name : Option<String>
}

#[derive(Debug)]
pub struct FnArg {
    name: String,
    declared_type: Type
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus, LogicalNot, BitwiseNot, Indirect, AddressOf
}

#[derive(Debug)]
pub enum BinaryOp {
    LogicalOr, LogicalAnd, BitwiseOr, BitwiseXor, BitwiseAnd, Equal, NotEqual, LT, LE, GT, GE, LShift, RShift, Plus, Minus, Times, Divide, Mod
}

#[derive(Debug)]
pub enum Expression<'a> {
    Integer { 
        value: i64,
        loc : usize
    },
    Identifier {
        name: String,
        loc: usize
    },
    Call {
        callee: &'a Expression<'a>,
        args: Vec<Arg<'a>>,
        loc: usize
    },
    Dot {
        expression: &'a Expression<'a>,
        property: String,
        loc: usize
    },
    Subscript {
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        loc: usize
    },
    UnaryOp {
        expression: &'a Expression<'a>,
        op: UnaryOp,
        loc: usize
    },
    BinaryOp {
        a: &'a Expression<'a>,
        op: BinaryOp,
        b: &'a Expression<'a>,
        loc: usize
    },
    Cast {
        expression: &'a Expression<'a>,
        new_type: Type,
        loc: usize
    }
}

#[derive(Debug)]
pub struct TypeArg {
    value : Type,
    name : Option<String>
}

#[derive(Debug)]
pub enum Type {
    Identifier(String),
    GenericType {
        name: String,
        args: Vec<TypeArg>
    },
    AnonymousStruct(Vec<(String, Type)>)
}

#[derive(Debug)]
pub struct OptionValue<'a> {
    name: Option<String>,
    value: &'a Expression<'a>
}

pub struct Ast<'a> {
    node_factory: Arena<AstNode<'a>>,
    expr_factory: Arena<Expression<'a>>,
}

impl<'a> Ast<'a> {
    fn new<'b>() -> Ast<'b> {
        Ast {
            node_factory: Arena::new(),
            expr_factory: Arena::new()
        }
    }

    fn parse(ast: &'a Ast<'a>, str: &String) -> Vec<&'a AstNode<'a>> {
        grammar::program(str, ast).unwrap()
    }
    
    fn create(&'a self, node: AstNode<'a>) -> &'a AstNode {
        self.node_factory.alloc(node)
    }

    fn create_exp(&'a self, exp: Expression<'a>) -> &'a Expression {
        self.expr_factory.alloc(exp)
    }
}

#[derive(Debug)]
pub enum AstNode<'a> {
    Block {
        statements : Vec<&'a AstNode<'a>>
    },
    If {
      condition : &'a Expression<'a>,
      consequent : &'a AstNode<'a>,
      alternate : Option<&'a AstNode<'a>>
    },
    FnDef {
        name: String,
        args: Vec<FnArg>,
        return_type: Type,
        body: &'a AstNode<'a>
    },
    Return {
        expression: Option<&'a Expression<'a>>
    },
    Expression {
        expression: &'a Expression<'a>
    },
    VarDefinition {
        name: String,
        declared_type: Option<Type>,
        initializer: Option<&'a Expression<'a>>
    },
    OptionBlock {
        options: Vec<&'a OptionValue<'a>>,
        statement: &'a AstNode<'a>
    },
    StructDefinition {
        name: String,
        fields: Vec<(String, Type)>
    }
}

fn main() {
    let filename = "test.65";

    println!("In file {}", filename);

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    println!("With text:\n{}", contents);

    let ast = Box::new(Ast::new());
    let parsed = Ast::parse(&ast, &contents);
    println!("Parsed as\n{:#?}", parsed);
}

trait HasId {
    fn id(&self) -> u64;
}

struct HashById<T>(T);

impl<T> Hash for HashById<T> where T : HasId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            &HashById(ref inner) => inner.id().hash(state)
        }
    }
}

trait Children<'a, T> {
    fn children(&'a self) -> Vec<&'a T>;
}

impl<'a> Children<'a, AstNode<'a>> for Vec<&'a AstNode<'a>> {
    fn children(&'a self) -> Vec<&'a AstNode<'a>> {
        self.clone()
    }
}

impl<'a> Children<'a, AstNode<'a>> for AstNode<'a> {
    fn children(&'a self) -> Vec<&'a AstNode<'a>> {
        match self {
            &AstNode::Block { ref statements } => statements.clone(),
            &AstNode::If { consequent, alternate, .. } => {
                let mut res = vec![consequent];
                if alternate.is_some() { res.push(alternate.unwrap()); }
                res
            },
            &AstNode::FnDef { body, .. } => vec![body],
            &AstNode::OptionBlock { statement, .. } => vec![statement],
            &AstNode::Return {..} => vec![],
            &AstNode::Expression {..} => vec![],
            &AstNode::VarDefinition {..} => vec![],
            &AstNode::StructDefinition {..} => vec![]
        }
    }
}

impl<'a> Children<'a, Expression<'a>> for AstNode<'a> {
    fn children(&'a self) -> Vec<&'a Expression<'a>> {
        match self {
            &AstNode::Block { .. } => vec![],
            &AstNode::If { condition, .. } => vec![condition],
            &AstNode::FnDef { .. } => vec![],
            &AstNode::OptionBlock { .. } => vec![], // TODO
            &AstNode::Return { expression } => 
                if expression.is_some() { vec![expression.unwrap()] }
                else { vec![] },
            &AstNode::Expression { expression } => vec![expression],
            &AstNode::VarDefinition { initializer, .. } =>
                if initializer.is_some() { vec![initializer.unwrap()] }
                else { vec![] },
            &AstNode::StructDefinition {..} => vec![]
        }
    }
}

impl<'a> Children<'a, Expression<'a>> for Expression<'a> {
    fn children(&'a self) -> Vec<&'a Expression<'a>> {
        match self {
            &Expression::Integer {..} => vec![],
            &Expression::Identifier {..} => vec![],
            &Expression::Call { callee, ref args, .. } => {
                let mut result: Vec<&'a Expression<'a>> = args.clone()
                    .into_iter()
                    .map(|a| { a.value })
                    .collect();
                result.push(callee);
                result
            },
            &Expression::Dot { expression, .. } => vec![expression],
            &Expression::Subscript { array, index, .. } => vec![array, index],
            &Expression::UnaryOp { expression, .. } => vec![expression],
            &Expression::BinaryOp { a, b, .. } => vec![a, b],
            &Expression::Cast { expression, .. } => vec![expression]
        }
    }
}