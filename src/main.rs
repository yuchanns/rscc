use anyhow::{anyhow, Result};
use std::{env, iter::Peekable, process::ExitCode, vec::IntoIter};

#[derive(Debug, PartialEq)]
enum TokenKind {
    Punct,
    Num(isize),
    Eof,
}

#[derive(Debug)]
struct Token<'a> {
    kind: TokenKind,
    #[allow(dead_code)]
    pos: usize,
    lexeme: &'a str,
}

fn new_token(kind: TokenKind, pos: usize, lexeme: &str) -> Token {
    Token { kind, pos, lexeme }
}

fn tokenize(mut input: &str) -> Result<Peekable<IntoIter<Token>>> {
    let mut tokens = Vec::new();
    let mut pos = 0;

    while !input.is_empty() {
        // Skip whitespace characters
        if input.starts_with(|c: char| c.is_ascii_whitespace()) {
            pos += 1;
            input = &input[1..];
            continue;
        }

        // Numeric literal
        if input.starts_with(|c: char| c.is_ascii_digit()) {
            let end = input
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(input.len());
            let (lexeme, rest) = input.split_at(end);
            let Ok(num) = lexeme.parse::<isize>() else {
                return Err(anyhow!("{lexeme}: invalid number of inputs"));
            };
            tokens.push(new_token(TokenKind::Num(num), pos, lexeme));
            pos += end;
            input = rest;
            continue;
        }

        // Punctuator
        if input.starts_with(|c: char| c.is_ascii_punctuation()) {
            tokens.push(new_token(TokenKind::Punct, pos, &input[..1]));
            pos += 1;
            input = &input[1..];
            continue;
        }

        return Err(anyhow!("invalid token"));
    }

    tokens.push(new_token(TokenKind::Eof, pos, ""));

    Ok(tokens.into_iter().peekable())
}

fn get_number(token: &Token) -> Result<isize> {
    let TokenKind::Num(num) = token.kind else {
        return Err(anyhow!("expected a number"));
    };
    Ok(num)
}

fn equal(token: &Token, op: &str) -> bool {
    token.lexeme == op
}

fn skip(tokens: &mut Peekable<IntoIter<Token>>, op: &str) -> Result<()> {
    let Some(tok) = tokens.peek() else {
        return Err(anyhow!("expected token"));
    };
    if !equal(tok, op) {
        return Err(anyhow!("expected '{op}'"));
    }
    tokens.next();
    Ok(())
}

fn main() -> ExitCode {
    if let Err(err) = run() {
        eprintln!("{:?}", err);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    let mut tokens = tokenize(arg)?;

    let Some(tok) = tokens.next() else {
        return Err(anyhow!("expected a token"));
    };

    #[cfg(not(target_os = "macos"))]
    {
        println!("  .global main");
        println!("main:");
    }
    #[cfg(target_os = "macos")]
    {
        println!("  .global _main");
        println!("_main:");
    }
    println!("  mov x0, #{}", get_number(&tok)?);

    while let Some(tok) = tokens.peek() {
        if tok.kind == TokenKind::Eof {
            break;
        }
        if equal(tok, "+") {
            tokens.next();
            let Some(tok) = tokens.next() else {
                return Err(anyhow!("expected a token"));
            };
            println!("  add x0, x0, #{}", get_number(&tok)?);
        }
        skip(&mut tokens, "-")?;
        let Some(tok) = tokens.next() else {
            return Err(anyhow!("expected a token"));
        };
        println!("  sub x0, x0, #{}", get_number(&tok)?);
    }

    println!("  ret");

    Ok(())
}
