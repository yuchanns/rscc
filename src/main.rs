use anyhow::{anyhow, Error, Result};
use std::{env, iter::Peekable, mem::take, process::ExitCode, sync::OnceLock, vec::IntoIter};

static GLOBAL_INPUT: OnceLock<String> = OnceLock::new();

fn current_input() -> &'static str {
    GLOBAL_INPUT.get().expect("GlobalInput not initialized")
}

fn new_error_at(loc: usize, message: &str) -> Error {
    anyhow!("{}\n{}^ {}", current_input(), " ".repeat(loc), message)
}

fn new_error_tok(tok: &Token, message: &str) -> Error {
    new_error_at(tok.pos, message)
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Punct,
    Num(isize),
    Eof,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    pos: usize,
    lexeme: &'static str,
}

fn new_token(kind: TokenKind, pos: usize, lexeme: &'static str) -> Token {
    Token { kind, pos, lexeme }
}

fn tokenize() -> Result<Peekable<IntoIter<Token>>> {
    let mut input = current_input();
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
                return Err(new_error_at(
                    pos,
                    &format!("{lexeme}: invalid number of inputs"),
                ));
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

        return Err(new_error_at(pos, "invalid token"));
    }

    tokens.push(new_token(TokenKind::Eof, pos, ""));

    Ok(tokens.into_iter().peekable())
}

fn get_number(token: &Token) -> Result<isize> {
    let TokenKind::Num(num) = token.kind else {
        return Err(new_error_tok(token, "expected a number"));
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
        return Err(new_error_tok(tok, &format!("expected '{op}'")));
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
    let mut args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_mut_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    GLOBAL_INPUT
        .set(take(arg))
        .expect("failed to initialize GlobalInput");

    let mut tokens = tokenize()?;

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
