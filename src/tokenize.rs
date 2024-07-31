use std::{iter::Peekable, mem::take, sync::OnceLock, vec::IntoIter};

use anyhow::{anyhow, Error, Result};

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    /// Keywords or punctuators
    Punct,
    /// Numeric literal
    Num(isize),
    /// End-of-file markers
    Eof,
}

/// Token type
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,      // Token kind
    pub pos: usize,           // Token location
    pub lexeme: &'static str, // Token raw content
}

/// Input string
static GLOBAL_INPUT: OnceLock<String> = OnceLock::new();

pub fn current_input() -> &'static str {
    GLOBAL_INPUT.get().expect("GlobalInput not initialized")
}

pub fn new_error_at(loc: usize, message: &str) -> Error {
    anyhow!("{}\n{}^ {}", current_input(), " ".repeat(loc), message)
}

pub fn new_error_tok(tok: &Token, message: &str) -> Error {
    new_error_at(tok.pos, message)
}

/// Consumes the current token if it matches `op`
pub fn equal(token: &Token, op: &str) -> bool {
    token.lexeme == op
}

/// Ensure that the current token is `op`
pub fn skip(tokens: &mut Peekable<IntoIter<Token>>, op: &str) -> Result<()> {
    let Some(tok) = tokens.peek() else {
        return Err(anyhow!("expected token"));
    };
    if !equal(tok, op) {
        return Err(new_error_tok(tok, &format!("expected '{op}'")));
    }
    tokens.next();
    Ok(())
}

/// Create a new token
fn new_token(kind: TokenKind, pos: usize, lexeme: &'static str) -> Token {
    Token { kind, pos, lexeme }
}

/// Read a punctuator token from input and returns its length
fn read_punct(input: &str) -> Option<usize> {
    let mut chars = input.chars();
    match (chars.next(), chars.next()) {
        (Some(a), Some(b)) if "=!<>".contains(a) && b == '=' => Some(2),
        (Some(a), _) if a.is_ascii_punctuation() => Some(1),
        _ => None,
    }
}

/// Tokenize `current_input` and nreturns new tokens
pub fn tokenize(input: &mut String) -> Result<Peekable<IntoIter<Token>>> {
    GLOBAL_INPUT
        .set(take(input))
        .expect("failed to initialize GlobalInput");
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
        if let Some(punct_len) = read_punct(input) {
            tokens.push(new_token(TokenKind::Punct, pos, &input[..punct_len]));
            pos += punct_len;
            input = &input[punct_len..];
            continue;
        }

        return Err(new_error_at(pos, "invalid token"));
    }

    tokens.push(new_token(TokenKind::Eof, pos, ""));

    Ok(tokens.into_iter().peekable())
}