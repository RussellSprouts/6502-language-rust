#![feature(nll)]
#![feature(underscore_lifetimes)]

extern crate core;
extern crate typed_arena;
use std::fs::File;
use std::io::prelude::*;
use std::vec::Vec;
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::HashMap;
use typed_arena::Arena;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug)]
pub struct SymbolTable<'a, T: 'a> {
    table: HashMap<String, T>,
    parent: Option<&'a SymbolTable<'a, T>>
}

impl<'a, T> SymbolTable<'a, T> {
    fn new() -> SymbolTable<'a, T> {
        SymbolTable {
            table: HashMap::new(),
            parent: None
        }
    }

    fn with_parent(parent: &'a SymbolTable<T>) -> SymbolTable<'a, T> {
        SymbolTable {
            table: HashMap::new(),
            parent: Some(&parent)
        }
    }

    fn insert(&mut self, key: String, value: T) {
        self.table.insert(key, value);
    }

    fn get(&self, key: String) -> Option<&T> {
        match self.table.get(&key) {
            v @ Some(..) => v,
            None => match self.parent {
                Some(ref parent) => parent.get(key),
                None => None
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FnArg<'a> {
    name: &'a str,
    declared_type: Type<'a>
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Minus, LogicalNot, BitwiseNot, Indirect, AddressOf, IORead
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    LogicalOr, LogicalAnd, BitwiseOr, BitwiseXor, BitwiseAnd, Equal, NotEqual, LT, LE, GT, GE, LShift, RShift, Plus, Minus, Times, Divide, Mod, Assignment, IOWrite
}

#[derive(Debug, Copy, Clone)]
pub enum Expression<'a> {
    Integer { 
        value: i64,
        loc : usize,
        id: u64
    },
    Identifier {
        name: &'a str,
        loc: usize,
        id: u64
    },
    Call {
        callee: &'a Expression<'a>,
        args: &'a [(Option<&'a str>, &'a Expression<'a>)],
        loc: usize,
        id: u64
    },
    Dot {
        expression: &'a Expression<'a>,
        property: &'a str,
        loc: usize,
        id: u64
    },
    Subscript {
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        loc: usize,
        id: u64
    },
    UnaryOp {
        expression: &'a Expression<'a>,
        op: UnaryOp,
        loc: usize,
        id: u64
    },
    BinaryOp {
        a: &'a Expression<'a>,
        op: BinaryOp,
        b: &'a Expression<'a>,
        loc: usize,
        id: u64
    },
    Cast {
        expression: &'a Expression<'a>,
        new_type: Type<'a>,
        loc: usize,
        id: u64
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeArg<'a> {
    value : Type<'a>,
    name : Option<&'a str>
}

#[derive(Debug, Copy, Clone)]
pub enum Type<'a> {
    Identifier(&'a str),
    GenericType {
        name: &'a str,
        args: &'a [TypeArg<'a>]
    },
    AnonymousStruct(&'a [(&'a str, Type<'a>)])
}

#[derive(Debug, Copy, Clone)]
pub struct OptionValue<'a> {
    name: Option<&'a str>,
    value: &'a Expression<'a>
}

pub struct Ast<'a> {
    node_factory: Arena<AstNode<'a>>,
    node_arr_factory: Arena<Vec<AstNode<'a>>>,
    expr_factory: Arena<Expression<'a>>,
    str_factory: Arena<String>,
    type_arg_arr_factory: Arena<Vec<TypeArg<'a>>>,
    fn_arg_arr_factory: Arena<Vec<FnArg<'a>>>,
    field_arr_factory: Arena<Vec<(&'a str, Type<'a>)>>,
    arg_arr_factory: Arena<Vec<(Option<&'a str>, &'a Expression<'a>)>>,
    last_id: Cell<u64>
}

trait AstCreate<'a, T: ?Sized> {
    fn create(&'a self, node: &T) -> &'a T;
}

impl<'a> Ast<'a> {
    fn new<'b>() -> Ast<'b> {
        Ast {
            node_factory: Arena::new(),
            node_arr_factory: Arena::new(),
            expr_factory: Arena::new(),
            str_factory: Arena::new(),
            arg_arr_factory: Arena::new(),
            type_arg_arr_factory: Arena::new(),
            fn_arg_arr_factory: Arena::new(),
            field_arr_factory: Arena::new(),
            last_id: Cell::new(0)
        }
    }

    fn parse(ast: &'a Ast<'a>, str: &String) -> AstNode<'a> {
        grammar::program(str, ast).unwrap()
    }

    fn id(&self) -> u64 {
        self.last_id.set(self.last_id.get() + 1);
        self.last_id.get()
    }
}

impl<'a> AstCreate<'a, str> for Ast<'a> {
    fn create(&'a self, s: &str) -> &'a str {
        self.str_factory.alloc(s.to_string())
    }
}

impl<'a> AstCreate<'a, Expression<'a>> for Ast<'a> {
    fn create(&'a self, exp: &Expression<'a>) -> &'a Expression {
        self.expr_factory.alloc((*exp).clone())
    }
}

impl<'a> AstCreate<'a, AstNode<'a>> for Ast<'a> {
    fn create(&'a self, node: &AstNode<'a>) -> &'a AstNode {
        self.node_factory.alloc((*node).clone())
    }
}

impl<'a> AstCreate<'a, [AstNode<'a>]> for Ast<'a> {
    fn create(&'a self, nodes: &[AstNode<'a>]) -> &'a [AstNode<'a>] {
        self.node_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [TypeArg<'a>]> for Ast<'a> {
    fn create(&'a self, nodes: &[TypeArg<'a>]) -> &'a [TypeArg<'a>] {
        self.type_arg_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [FnArg<'a>]> for Ast<'a> {
    fn create(&'a self, nodes: &[FnArg<'a>]) -> &'a [FnArg<'a>] {
        self.fn_arg_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [(&'a str, Type<'a>)]> for Ast<'a> {
    fn create(&'a self, nodes: &[(&'a str, Type<'a>)]) -> &'a [(&'a str, Type<'a>)] {
        self.field_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [(Option<&'a str>, &'a Expression<'a>)]> for Ast<'a> {
    fn create(&'a self, nodes: &[(Option<&'a str>, &'a Expression<'a>)]) -> &'a [(Option<&'a str>, &'a Expression<'a>)] {
        self.arg_arr_factory.alloc(Vec::from(nodes))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AstNode<'a> {
    Block {
        statements : &'a [AstNode<'a>],
        loc: usize, id: u64
    },
    If {
        condition : &'a Expression<'a>,
        consequent : &'a AstNode<'a>,
        alternate : Option<&'a AstNode<'a>>,
        loc: usize, id: u64
    },
    FnDef {
        name: &'a str,
        args: &'a [FnArg<'a>],
        return_type: Type<'a>,
        body: &'a AstNode<'a>,
        loc: usize, id: u64
    },
    Return {
        expression: Option<&'a Expression<'a>>,
        loc: usize, id: u64
    },
    Expression {
        expression: &'a Expression<'a>,
        loc: usize, id: u64
    },
    VarDefinition {
        name: &'a str,
        declared_type: Option<Type<'a>>,
        initializer: Option<&'a Expression<'a>>,
        loc: usize, id: u64
    },
    OptionBlock {
        options: &'a [&'a OptionValue<'a>],
        statement: &'a AstNode<'a>,
        loc: usize, id: u64
    },
    StructDefinition {
        name: &'a str,
        fields: &'a [(&'a str, Type<'a>)],
        loc: usize, id: u64
    }
}

fn main() {
    let mut table: SymbolTable<String> = SymbolTable::new();
    let table2 = SymbolTable::with_parent(&table);

    let filename = "test.65";

    println!("In file {}", filename);

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    println!("With text:\n{}", contents);

    let ast = Ast::new();
    let parsed = Ast::parse(&ast, &contents);
    println!("Parsed as\n{:#?}", parsed);

    println!("Parsed {:#?}", Children::<Expression>::children(&parsed));
}

trait Children<'a, T> {
    fn children(&'a self) -> Vec<&'a T>;
}

impl<'a> Children<'a, AstNode<'a>> for AstNode<'a> {
    fn children(&'a self) -> Vec<&'a AstNode<'a>> {
        match self {
            &AstNode::Block { ref statements, .. } => statements.into_iter().collect(),
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
        let mut direct_children = match self {
            &AstNode::If { condition, .. } => vec![condition],
            &AstNode::Return { expression, .. } => 
                if expression.is_some() { vec![expression.unwrap()] }
                else { vec![] },
            &AstNode::VarDefinition { initializer, .. } =>
                if initializer.is_some() { vec![initializer.unwrap()] }
                else { vec![] },
            &AstNode::Expression { expression, .. } => vec![expression],
            &AstNode::Block { .. } => vec![],
            &AstNode::FnDef { .. } => vec![],
            &AstNode::OptionBlock { .. } => vec![], // TODO
            &AstNode::StructDefinition {..} => vec![]
        };

        let subnodes: Vec<&'a AstNode<'a>> = self.children();
        let mut subnode_children: Vec<&'a Expression<'a>> =
            subnodes.into_iter()
                    .flat_map(|n| { n.children() })
                    .collect();
        direct_children.append(&mut subnode_children);
        direct_children
    }
}

impl<'a> Children<'a, Expression<'a>> for Expression<'a> {
    fn children(&'a self) -> Vec<&'a Expression<'a>> {
        match self {
            &Expression::Call { callee, ref args, .. } => {
                let mut result: Vec<&'a Expression<'a>> = args.clone()
                    .into_iter()
                    .map(|a| { a.1 })
                    .collect();
                result.push(callee);
                result
            },
            &Expression::Dot { expression, .. } => vec![expression],
            &Expression::Subscript { array, index, .. } => vec![array, index],
            &Expression::UnaryOp { expression, .. } => vec![expression],
            &Expression::BinaryOp { a, b, .. } => vec![a, b],
            &Expression::Cast { expression, .. } => vec![expression],
            &Expression::Integer {..} => vec![],
            &Expression::Identifier {..} => vec![],
        }
    }
}

trait PrintAst {
    fn print_pretty(&self, indent: String) -> String;
}

impl<'a> PrintAst for AstNode<'a> {
    fn print_pretty(&self, indent: String) -> String {
        use AstNode::*;
        match self {
            &Block { statements, .. } => format!(
                "{}(Block\n{})", indent,
                statements.into_iter()
                    .map(|s| { format!("{}{}", indent, s.print_pretty(indent.clone() + "  ")) })
                    .collect::<Vec<String>>().join("\n")),
            &If { condition, consequent, alternate, .. } => format!(
                "{}(If ({})\n{}\n{}ELSE\n{}", indent, "", "CONSEQUENT", indent, "ALTERNATE"
            ),
            _ => format!("{}STATEMENT", indent)
        }
    }
}

