#![feature(nll)]
#![feature(underscore_lifetimes)]

extern crate core;
extern crate typed_arena;
#[macro_use]
extern crate im;

use std::fs::File;
use std::io::prelude::*;
use std::vec::Vec;
use std::cell::Cell;
use typed_arena::Arena;

use im::HashMap;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug)]
pub struct SymbolTable<'a, T: 'a> {
    tables: HashMap<u64, HashMap<String, &'a T>>,
    id: u64
}

impl<'a, T> SymbolTable<'a, T> {
    fn new() -> SymbolTable<'a, T> {
        SymbolTable {
            tables: HashMap::new(),
            id: 0
        }
    }

    fn add(m: HashMap<String, T>) {

    }
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
pub struct Expression2<'a> {
    loc: usize,
    id: u64,
    is: Expression2Type<'a>
}

#[derive(Debug, Copy, Clone)]
pub enum Expression2Type<'a> {
    Integer(i64),
    FixedPoint {
        value: i64,
        scale: i64
    },
    Identifier(&'a str),
    ArrayAccess {
        array: &'a Expression2<'a>,
        index: &'a Expression2<'a>
    },
    Dot {
        expression: &'a Expression2<'a>,
        property: &'a str
    },
    Call {
        callee: &'a Expression2<'a>,
        args: &'a [(Option<&'a str>, &'a Expression2<'a>)]
    },
    UnaryOp {
        expression: &'a Expression2<'a>,
        op: UnaryOp
    },
    BinaryOp {
        a: &'a Expression2<'a>,
        op: BinaryOp,
        b: &'a Expression2<'a>
    },
    Cast {
        ex: &'a Expression2<'a>,
        new_type: Type<'a>
    },
    StructInitialization {
        struct_type: Type<'a>,
        args: &'a [(Option<&'a str>, &'a Expression2<'a>)],
    },
    If {
        condition: &'a Expression2<'a>,
        consequent: &'a Expression2<'a>,
        alternate: Option<&'a Expression2<'a>>
    },
    For {
        var: &'a str,
        iter: &'a Expression2<'a>,
        body: &'a Expression2<'a>
    },
    Block {
        body: &'a [&'a Expression2<'a>],
        semi_terminated: bool
    },
    OptionBlock {
        options: Option<i64>, // todo
        body: &'a Expression2<'a>
    },
    LetWithInitializer {
        var: &'a str,
        declared_type: Option<Type<'a>>,
        init: &'a Expression2<'a>
    },
    Let {
        var: &'a str,
        declared_type: Type<'a>
    },
    Function {
        name: Option<&'a str>,
        args: &'a [(&'a str, Type<'a>)],
        return_type: Type<'a>,
        body: &'a Expression2<'a>,
    },
    StructDefinition {
        name: &'a str,
        fields: &'a [(&'a str, Type<'a>)]
    },
    Return {
        value: &'a Expression2<'a>
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

pub struct Ast<'a> {
    expr_arr_factory: Arena<Vec<&'a Expression2<'a>>>,
    expr_factory: Arena<Expression2<'a>>,
    str_factory: Arena<String>,
    type_arg_arr_factory: Arena<Vec<TypeArg<'a>>>,
    field_arr_factory: Arena<Vec<(&'a str, Type<'a>)>>,
    arg_arr_factory: Arena<Vec<(Option<&'a str>, &'a Expression2<'a>)>>,
    last_id: Cell<u64>
}

trait AstCreate<'a, T: ?Sized> {
    fn create(&'a self, node: &T) -> &'a T;
}

impl<'a> Ast<'a> {
    fn new<'b>() -> Ast<'b> {
        Ast {
            expr_arr_factory: Arena::new(),
            expr_factory: Arena::new(),
            str_factory: Arena::new(),
            arg_arr_factory: Arena::new(),
            type_arg_arr_factory: Arena::new(),
            field_arr_factory: Arena::new(),
            last_id: Cell::new(0)
        }
    }

    fn parse(ast: &'a Ast<'a>, str: &String) -> Expression2<'a> {
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

impl<'a> AstCreate<'a, Expression2<'a>> for Ast<'a> {
    fn create(&'a self, exp: &Expression2<'a>) -> &'a Expression2 {
        self.expr_factory.alloc((*exp).clone())
    }
}

impl<'a> AstCreate<'a, [&'a Expression2<'a>]> for Ast<'a> {
    fn create(&'a self, nodes: &[&'a Expression2<'a>]) -> &'a [&'a Expression2<'a>] {
        self.expr_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [TypeArg<'a>]> for Ast<'a> {
    fn create(&'a self, nodes: &[TypeArg<'a>]) -> &'a [TypeArg<'a>] {
        self.type_arg_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [(&'a str, Type<'a>)]> for Ast<'a> {
    fn create(&'a self, nodes: &[(&'a str, Type<'a>)]) -> &'a [(&'a str, Type<'a>)] {
        self.field_arr_factory.alloc(Vec::from(nodes))
    }
}

impl<'a> AstCreate<'a, [(Option<&'a str>, &'a Expression2<'a>)]> for Ast<'a> {
    fn create(&'a self, nodes: &[(Option<&'a str>, &'a Expression2<'a>)]) -> &'a [(Option<&'a str>, &'a Expression2<'a>)] {
        self.arg_arr_factory.alloc(Vec::from(nodes))
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

    let ast = Ast::new();
    let parsed = Ast::parse(&ast, &contents);
    println!("Parsed as\n{:#?}", parsed);

    println!("Parsed {:#?}", Children::<Expression2>::children(&parsed));

    let global_scope = resolve_symbols(&parsed, &HashMap::new());
    println!("Global scope: {:#?}", global_scope);
}

trait Children<'a, T> {
    fn children(&'a self) -> Vec<&'a T>;
}

impl<'a> Children<'a, Expression2<'a>> for Expression2<'a> {
    fn children(&'a self) -> Vec<&'a Expression2<'a>> {
        use Expression2Type::*;
        match self.is {
            Call { callee, ref args, .. } => {
                let mut result: Vec<&'a Expression2<'a>> = args.clone()
                    .into_iter()
                    .map(|a| { a.1 })
                    .collect();
                result.push(callee);
                result
            },
            StructInitialization { ref args, .. } => {
                args.into_iter()
                    .map(|a| { a.1 })
                    .collect()
            },
            Dot { expression, .. } => vec![expression],
            ArrayAccess { array, index, .. } => vec![array, index],
            UnaryOp { expression, .. } => vec![expression],
            BinaryOp { a, b, .. } => vec![a, b],
            Cast { ex, .. } => vec![ex],
            If { condition, consequent, alternate, .. } => {
                match alternate {
                    Some(a) => vec![condition, consequent, a],
                    None => vec![condition, consequent]
                }
            },
            Block { body, .. } => body.into_iter().map(|s| { *s }).collect(),
            For { iter, body, .. } => vec![iter, body],
            OptionBlock { body, .. } => vec![body],
            Function { body, .. } => vec![body],
            Return { value } => vec![value],
            LetWithInitializer { init, .. } => vec![init],
            Let { .. } => vec![],
            Integer {..} => vec![],
            FixedPoint {..} => vec![],
            Identifier {..} => vec![],
            StructDefinition {..} => vec![]
        }
    }
}

fn resolve_symbols<'a>(expr: &'a Expression2<'a>, map: &HashMap<String, Expression2<'a>>) -> i64 {
    use Expression2Type::*;

    let mut new_map = map.clone();

    // These expressions add to the scope of their children
    match expr.is {
        Identifier(name) => {
            match map.get(&name.to_string()) {
                None => panic!("Unresolved identifier! {}", name),
                _ => ()
            }
        },
        For { var, .. } => new_map = map.insert(var.to_string(), expr),
        Function { args, .. } => {
            for &(name, _t) in args {
                new_map = new_map.insert(name.to_string(), expr)
            }
        },
        Block { body, .. } => {
            for child in expr.children() {
                // These expressions add to the scope that encloses them.
                match child.is {
                    Let { var, .. } => new_map = new_map.insert(var.to_string(), child),
                    LetWithInitializer { var, .. } => new_map = new_map.insert(var.to_string(), child),
                    Function { name, .. } => if (name.is_some()) {
                        new_map = new_map.insert(name.unwrap().to_string(), child)
                    },
                    _ => ()
                }
                resolve_symbols(child, &new_map);
            }
            return 0
        },
        _ => ()
    }
    for child in expr.children() {
        resolve_symbols(child, &new_map);
    }
    0
}

trait PrintAst {
    fn print_pretty(&self, indent: String) -> String;
}

/*fn resolve_symbols_expr<'a>(expr: &'a Expression<'a>, map: &HashMap<String, AstNode<'a>>) {
    match expr {
        &Expression::Identifier { name, .. } => {
            match map.get(&name.to_string()) {
                None => panic!("Unresolved identifier! {}", name),
                _ => ()
            };
        },
        _ => ()
    };
    for child in expr.children() {
        resolve_symbols_expr(child, &map);
    }
}

fn resolve_symbols<'a>(node: &'a AstNode<'a>, map: &HashMap<String, AstNode<'a>>) -> HashMap<String, AstNode<'a>> {
    use AstNode::*;

    for expr in node.children() {
        resolve_symbols_expr(expr, &map);
    }

    let mut map = map.clone();

    for c in node.children() {
        match c {
            &FnDef { name, args, .. } => {
                println!("Found fn {}", name);
                // in the current scope, add the function name,
                // so later code can refer to it.
                map = map.insert(name.to_string(), c);
                let mut inner_map = map.clone();
                for arg in args {
                    if let &FnArgDef { name, .. } = arg {
                        inner_map = inner_map.insert(name.to_string(), arg);
                    } else {
                        panic!("FnDef.arg values must be FnDefArg");
                    }
                }
                // inside the scope, include the fn args.
                resolve_symbols(c, &inner_map);
            },
            &VarDefinition { name, .. } => {
                // resolve symbols in the initializer before adding the
                // new mapping.
                resolve_symbols(c, &map);
                println!("Found let {}", name);
                map = map.insert(name.to_string(), c);
            },
            _ => {
                resolve_symbols(c, &map);
            }
        }
    }
    map
}*/