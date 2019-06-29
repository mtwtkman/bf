use std::collections::HashMap;
use std::io::Read;
use std::{error, fmt};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
enum BfError {
    TokenizeError,
    InvalidBracketPair(i8),
    GetCharError,
    NotFoundCloseBracket(i8),
    NotFoundOpenBracket(i8),
}

impl fmt::Display for BfError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BfError::TokenizeError => write!(f, "cannot tokenize."),
            BfError::InvalidBracketPair(pos) => write!(f, "invalid bracket pair. position {}", pos),
            BfError::GetCharError => write!(f, "invalid character"),
            BfError::NotFoundCloseBracket(pos) => {
                write!(f, "not found close bracket correspond one at {}", pos)
            }
            BfError::NotFoundOpenBracket(pos) => {
                write!(f, "not found open bracket correcspond one at {}", pos)
            }
        }
    }
}

impl error::Error for BfError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Token {
    Inc,
    Dec,
    Next,
    Prev,
    GetChar,
    PutChar,
    Open,
    Close,
}

impl Token {
    fn tokenize(ch: &char) -> Result<Self, BfError> {
        match ch {
            '+' => Ok(Token::Inc),
            '-' => Ok(Token::Dec),
            '>' => Ok(Token::Next),
            '<' => Ok(Token::Prev),
            ',' => Ok(Token::GetChar),
            '.' => Ok(Token::PutChar),
            '[' => Ok(Token::Open),
            ']' => Ok(Token::Close),
            _ => Err(BfError::TokenizeError),
        }
    }
}

type Tokens = Vec<Token>;

fn tokenize_code(src: &str) -> Result<Tokens, BfError> {
    src.chars().fold(Ok(Vec::new()), move |acc, c| {
        acc.and_then(move |mut tokens| {
            Token::tokenize(&c).and_then(|t| {
                tokens.push(t);
                Ok(tokens)
            })
        })
    })
}

type BracketMap = HashMap<i8, i8>;

fn build_bracket_map(tokens: &Tokens) -> Result<BracketMap, BfError> {
    let mut depth = 0;
    let mut pairs: BracketMap = HashMap::new();
    let mut nest: HashMap<usize, usize> = HashMap::new();
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::Open => {
                pairs.insert(i as i8, -1);
                depth += 1;
                nest.insert(depth, i);
            }
            Token::Close => {
                if let Some(start_pos) = nest.get(&depth) {
                    pairs.entry(*start_pos as i8).and_modify(|e| {
                        *e = i as i8;
                    });
                    depth -= 1;
                } else {
                    return Err(BfError::InvalidBracketPair(i as i8));
                }
            }
            _ => {}
        }
    }
    let mut missing = -1;
    for (start, end) in &pairs {
        if *end == -1 {
            missing = *start;
            break;
        }
    }
    if missing != -1 {
        Err(BfError::InvalidBracketPair(missing))
    } else {
        Ok(pairs)
    }
}

struct Memory {
    cells: Vec<usize>,
    cell_pos: usize,
    tokens: Tokens,
    token_pos: usize,
    bracket_map: BracketMap,
    reverse_bracket_map: BracketMap,
}

impl Memory {
    fn new(size: usize, tokens: Tokens, bracket_map: BracketMap) -> Memory {
        let cells = vec![0; size];
        let mut reverse_bracket_map: BracketMap = HashMap::new();
        for (start, close) in &bracket_map {
            reverse_bracket_map.insert(*close, *start);
        }
        Memory {
            cells,
            cell_pos: 0,
            tokens,
            token_pos: 0,
            bracket_map,
            reverse_bracket_map,
        }
    }
    fn prev_cell(&mut self) {
        self.cell_pos -= 1;
    }
    fn next_cell(&mut self) {
        self.cell_pos += 1;
    }
    fn next_token(&mut self) {
        self.token_pos += 1;
    }
    fn current_token(&self) -> Token {
        self.tokens[self.token_pos]
    }
    fn current_cell(&self) -> usize {
        self.cells[self.cell_pos]
    }
    fn increment(&mut self) {
        let new_val = vec![self.current_cell() + 1];
        let start = self.cell_pos;
        let end = self.cell_pos + 1;
        self.cells.splice(start..end, new_val.clone());
    }
    fn decrement(&mut self) {
        let new_val = vec![self.current_cell() - 1];
        let start = self.cell_pos;
        let end = self.cell_pos + 1;
        self.cells.splice(start..end, new_val.clone());
    }
    fn get_char(&mut self) -> Result<(), BfError> {
        if let Some(v) = std::io::stdin()
            .bytes()
            .next()
            .and_then(|result| result.ok())
            .map(|v| v as usize)
        {
            let start = self.cell_pos;
            let end = self.cell_pos + 1;
            let d = vec![v];
            self.cells.splice(start..end, d.clone());
            Ok(())
        } else {
            Err(BfError::GetCharError)
        }
    }
    fn put_char(&self) {
        print!("{}", char::from(self.current_cell() as u8).to_string());
    }
    fn open(&mut self) -> Result<(), BfError> {
        if self.current_cell() == 0 {
            if let Some(close_pos) = self.bracket_map.get(&(self.token_pos as i8)) {
                self.token_pos = *close_pos as usize;
            } else {
                return Err(BfError::NotFoundCloseBracket(self.token_pos as i8));
            }
        }
        Ok(())
    }
    fn close(&mut self) -> Result<(), BfError> {
        if self.current_cell() != 0 {
            if let Some(open_pos) = self.reverse_bracket_map.get(&(self.token_pos as i8)) {
                self.token_pos = *open_pos as usize;
            } else {
                return Err(BfError::NotFoundOpenBracket(self.token_pos as i8));
            }
        }
        Ok(())
    }
}

fn eval(src: &str, size: usize) -> Result<Memory, BfError> {
    let tokens = tokenize_code(src)?;
    let bracket_map = build_bracket_map(&tokens)?;
    let mut memory = Memory::new(size, tokens, bracket_map);
    let end_pos = memory.tokens.len() - 1;
    while memory.token_pos <= end_pos {
        match memory.current_token() {
            Token::Inc => memory.increment(),
            Token::Dec => memory.decrement(),
            Token::Next => memory.next_cell(),
            Token::Prev => memory.prev_cell(),
            Token::GetChar => memory.get_char()?,
            Token::PutChar => memory.put_char(),
            Token::Open => memory.open()?,
            Token::Close => memory.close()?,
        }
        memory.next_token();
    }
    Ok(memory)
}

fn main() {
    let src = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++.";
    let _ = eval(src, 100000);
}
