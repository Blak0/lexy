use num::Num;
use regex::Regex;
use std::fmt::{self, Debug};

/// Precedence of a concrete operation - operations with higher values execute first
pub type Precedence = u8;

/// Function or closure to use while executing this operator
pub type Callback<T> = Box<dyn Fn(T, T) -> T>;

#[derive(Debug, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug)]
pub enum ArgumentType {
    Number,
}

#[derive(Debug)]
pub struct TokenRule<T>
where
    T: Num,
{
    pub regex: Regex,
    pub kind: TokenType<T>,
}

pub enum TokenType<T>
where
    T: Num,
{
    Argument {
        kind: ArgumentType,
    },
    Operator {
        precedence: Precedence,
        associativity: Associativity,
        callback: Callback<T>,
    },
    LeftBracket,
    RightBracket,
}

impl<T> fmt::Debug for TokenType<T>
where
    T: Num + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Argument { kind } => f.debug_tuple("Argument").field(kind).finish(),
            Self::Operator {
                precedence,
                associativity,
                ..
            } => f
                .debug_tuple("Operator")
                .field(precedence)
                .field(associativity)
                .finish(),
            Self::LeftBracket => f.debug_tuple("LeftBracket").finish(),
            Self::RightBracket => f.debug_tuple("RightBracket").finish(),
        }
    }
}

/// Use this if you want to get a quite common rule set with:
/// Operators: +, -, *, / and round brackets ()
///
///
/// You can add more rules on returned builder:
/// ```
/// use lexer::{get_default_builder, Associativity, TokenType};
///
/// let rules = get_default_builder::<f32>().add(
///     r"\^",
///     TokenType::Operator {
///     precedence: 3,
///     associativity: Associativity::Right,
///     callback: Box::new(|x, y| f32::powf(x, y)),
/// }).compile();
/// ```
pub fn get_default_builder<T>() -> TokenRulesBuilder<T>
where
    T: num::Num,
{
    TokenRulesBuilder::<T>::new()
        .add_default(TokenType::Argument {
            kind: ArgumentType::Number,
        })
        .add(
            r"\+",
            TokenType::Operator {
                precedence: 1,
                associativity: Associativity::Left,
                callback: Box::new(|x, y| x + y),
            },
        )
        .add(
            r"\-",
            TokenType::Operator {
                precedence: 1,
                associativity: Associativity::Left,
                callback: Box::new(|x, y| x - y),
            },
        )
        .add(
            r"\*",
            TokenType::Operator {
                precedence: 2,
                associativity: Associativity::Left,
                callback: Box::new(|x, y| x * y),
            },
        )
        .add(
            r"/",
            TokenType::Operator {
                precedence: 2,
                associativity: Associativity::Left,
                callback: Box::new(|x, y| x / y),
            },
        )
        .add_default(TokenType::LeftBracket)
        .add_default(TokenType::RightBracket)
}

#[derive(Debug)]
pub struct TokenRulesBuilder<T: Num> {
    rules: Vec<TokenRule<T>>,
}

impl<T> TokenRulesBuilder<T>
where
    T: Num,
{
    pub fn new() -> TokenRulesBuilder<T> {
        TokenRulesBuilder { rules: Vec::new() }
    }

    pub fn add(mut self, match_pattern: &str, token_type: TokenType<T>) -> Self {
        let regex = compile_regex_for_token_rule(&match_pattern, &token_type);
        self.rules.push(TokenRule {
            regex,
            kind: token_type,
        });
        self
    }

    pub fn add_default(mut self, token_type: TokenType<T>) -> Self {
        let regex: Regex = match token_type {
            TokenType::LeftBracket => {
                Regex::new(r"\(").expect("Compilation of default left bracket has failed")
            }
            TokenType::RightBracket => {
                Regex::new(r"\)").expect("Compilation of default right bracket has failed")
            }
            TokenType::Operator { .. } => {
                panic!("Default operators aren't supported!")
            }
            TokenType::Argument { ref kind } => match kind {
                ArgumentType::Number => {
                    Regex::new(r"\d+\.?(\d?)+").expect("Compilation of numeric argument has failed")
                }
            },
        };

        self.rules.push(TokenRule {
            regex,
            kind: token_type,
        });

        self
    }

    pub fn compile(self) -> TokenRules<T> {
        TokenRules { rules: self.rules }
    }
}

fn compile_regex_for_token_rule<T: num::Num>(match_rule: &str, token: &TokenType<T>) -> Regex {
    match token {
        TokenType::Argument { .. } => {
            Regex::new(match_rule).expect("Compilation of number regex has failed")
        }
        TokenType::Operator { .. } => Regex::new(match_rule)
            .expect(format!("Compilation of regex with rule: {} has failed.", match_rule).as_str()),
        TokenType::LeftBracket | TokenType::RightBracket => Regex::new(match_rule).expect(
            format!(
                "Compilation of bracket regex with rule: {} has failed",
                match_rule
            )
            .as_str(),
        ),
    }
}
#[derive(Debug)]
pub struct TokenRules<T>
where
    T: Num,
{
    pub rules: Vec<TokenRule<T>>,
}

/// Token type ready to be evaluated
pub enum ExpressionToken<'rules, T>
where
    T: 'rules + Num,
{
    Argument(T),
    Operator(Precedence, &'rules Associativity, &'rules Callback<T>),
    LeftBracket,
    RightBracket,
}

impl<'rules, T> Debug for ExpressionToken<'rules, T>
where
    T: 'rules + Num + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Argument(value) => f.debug_tuple("Argument").field(value).finish(),
            Self::Operator(prec, assoc, _) => {
                f.debug_tuple("Operator").field(prec).field(assoc).finish()
            }
            Self::LeftBracket => write!(f, "LeftBracket"),
            Self::RightBracket => write!(f, "RightBracket"),
        }
    }
}

pub type ExpressionTokens<'rules, T> = Vec<ExpressionToken<'rules, T>>;

/// Single match containing start and stop byte index, corresponting slice and type of rule that detected it.
pub struct FoundMatch<'text, 'rules, T>
where
    T: num::Num,
{
    pub start: usize,
    pub stop: usize,
    pub value: &'text str,
    pub token_type: &'rules TokenType<T>,
}
