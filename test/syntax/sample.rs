//! A sample of Rust for the colouring tests.
#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::io::{Read as _, Write};
use crate::parts::*;

/// Largest number of items kept.
pub const MAX_ITEMS: usize = 1_024;
static GREETING: &str = "hello\tworld\n";

/* A block comment. */
/** A block doc comment. */
#[derive(Debug, Clone, PartialEq)]
pub struct Point<T> {
    pub x: T,
    y: T,
}

#[derive(Debug)]
enum Shape {
    Circle { radius: f64 },
    Square(f64),
    Empty,
}

trait Area {
    fn area(&self) -> f64;
    fn name(&self) -> String { String::from("shape") }
}

impl Area for Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Circle { radius } => std::f64::consts::PI * radius * radius,
            Shape::Square(side) => side * side,
            Shape::Empty => 0.0,
        }
    }
}

impl<T: Display> fmt::Display for Point<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

macro_rules! square {
    ($x:expr) => {
        $x * $x
    };
}

async fn fetch(url: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut body = Vec::new();
    let _ = url.len();
    body.push(b'x');
    Ok(body)
}

fn counts(words: &[&str]) -> HashMap<String, usize> {
    let mut map = HashMap::new();
    for word in words.iter() {
        *map.entry(word.to_string()).or_insert(0) += 1;
    }
    map
}

fn main() {
    let (a, mut b) = (1u32, 2.5_f64);
    let Point { x, y: why } = Point { x: 1, y: 2 };
    let c = 'c';
    let raw = r#"raw "string""#;
    let bytes = b"bytes";
    let shapes = vec![Shape::Circle { radius: 1.0 }, Shape::Square(2.0), Shape::Empty];
    let total: f64 = shapes.iter().map(|s| s.area()).sum();
    b += 1.0;
    if let Some(first) = shapes.first() {
        println!("{:?} {} {}", first, total, u32::MAX);
    } else if a > 0 && !false {
        eprintln!("none");
    }
    while b < 10.0 {
        b *= 2.0;
    }
    let n = loop {
        break square!(3);
    };
    let label = 'outer: loop {
        break 'outer n;
    };
    let result = counts(&["a", "b", "a"]);
    assert_eq!(result.get("a"), Some(&2));
    let _unused = (x, why, c, raw, bytes, label, None::<i32>);
    let closure = move |v: i64| -> i64 { v << 2 };
    std::process::exit(closure(0) as i32);
}
