use crate::lexy::calculate;

mod lexy;
mod types;

fn main() {
    use types::{ArgumentType, Associativity, TokenRulesBuilder, TokenType};

    let token_rules = TokenRulesBuilder::<f32>::new()
        .add_default(TokenType::Argument(ArgumentType::Number))
        .add(
            r"\+",
            TokenType::Operator(1, Associativity::Left, Box::new(|x, y| x + y)),
        )
        .add(
            r"-",
            TokenType::Operator(1, Associativity::Left, Box::new(|x, y| x - y)),
        )
        .add(
            r"\*",
            TokenType::Operator(2, Associativity::Left, Box::new(|x, y| x * y)),
        )
        .add(
            r"/",
            TokenType::Operator(2, Associativity::Left, Box::new(|x, y| x / y)),
        )
        .add(
            r"\^",
            TokenType::Operator(3, Associativity::Right, Box::new(|x, y| f32::powf(x, y))),
        )
        .add_default(TokenType::LeftBracket)
        .add_default(TokenType::RightBracket)
        .compile();

    let x = calculate("420+68", &token_rules).unwrap();
    println!("{}", x);
}
