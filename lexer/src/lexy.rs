use itertools::Itertools;
use num::Num;
use std::{collections::VecDeque, str::FromStr};

use crate::types::{
    Associativity, ExpressionToken, ExpressionTokens, FoundMatch, TokenRules, TokenType,
};

/// Tokenize text based on build rules. All whitespace is removed in the text.
///
/// Tokens are considered correct if they are recognized by any rule in token_rules.
///
/// Returns Err if text contains invalid tokens or is empty.
/// Every not recognized token is included in error message.
pub fn tokenize<'text, 'rules, T>(
    text: &'text str,
    token_rules: &'rules TokenRules<T>,
) -> Result<ExpressionTokens<'rules, T>, String>
where
    T: Num + FromStr,
    <T as FromStr>::Err: std::fmt::Debug,
{
    let text = text.replace(" ", "");

    if text.len() == 0 {
        return Err("Provided text is empty".to_string());
    }

    let mut found: Vec<FoundMatch<T>> = find_all_matches_defined_by_rules(token_rules, &text);

    //sort found matches from leftmost to rightmost
    found.sort_by(|first, second| first.start.partial_cmp(&second.start).unwrap());
    verify_integrity_of_matches(&found, &text)?;

    return Ok(found
        .iter()
        .map(|m| token_type_to_expression_token(m.value, m.token_type))
        .collect());
}

/// Get every possible match described in every rule
fn find_all_matches_defined_by_rules<'text, 'rules, T>(
    token_rules: &'rules TokenRules<T>,
    text: &'text String,
) -> Vec<FoundMatch<'text, 'rules, T>>
where
    T: num::Num,
{
    let mut found = vec![];
    for rule in &token_rules.rules {
        for m in rule.regex.find_iter(text) {
            found.push(FoundMatch {
                start: m.start(),
                stop: m.end(),
                value: m.as_str(),
                token_type: &rule.kind,
            })
        }
    }
    return found;
}

/// Checks if matches cover every byte of text.
/// If not, it returns Err with string listing every token not included by matches.
/// This function assumes that matches are sorted in occurence order.
fn verify_integrity_of_matches<T>(found: &[FoundMatch<T>], text: &String) -> Result<(), String>
where
    T: num::Num,
{
    let text_byte_len = text.bytes().count();
    let mut lowest = usize::MAX;
    let mut highest = usize::MIN;

    let mut unknown_symbols = std::collections::HashSet::new();

    if found.len() == 0 {
        unknown_symbols.insert(format!("{}", &text));
    } else if found.len() == 1 {
        let single_element = found.get(0).unwrap();
        lowest = single_element.start;
        highest = single_element.stop;
    } else {
        for (prev, next) in found.iter().tuple_windows() {
            lowest = std::cmp::min(prev.start, lowest);
            highest = std::cmp::max(next.stop, highest);

            if prev.stop != next.start {
                unknown_symbols.insert(format!("{}", &text[prev.stop..next.start]));
            }
        }
    }

    if lowest != 0 && lowest < text_byte_len {
        unknown_symbols.insert(format!("{}", &text[0..lowest]));
    }

    if highest != text_byte_len {
        unknown_symbols.insert(format!("{}", &text[highest..text_byte_len]));
    }

    if unknown_symbols.len() == 0 {
        return Ok(());
    } else if unknown_symbols.len() == 1 {
        return Err(format!(
            "Found unknown symbol: {}",
            unknown_symbols.iter().nth(0).unwrap()
        ));
    } else {
        return Err(format!(
            "Found unknown symbols: {}",
            unknown_symbols.iter().join(", ")
        ));
    }
}

/// Translate token type to expression token for evaluation purposes.
fn token_type_to_expression_token<'text, 'rules, T>(
    slice: &'text str,
    token_type: &'rules TokenType<T>,
) -> ExpressionToken<'rules, T>
where
    T: num::Num + FromStr,
    <T as FromStr>::Err: std::fmt::Debug,
{
    match token_type {
        TokenType::Argument(_) => ExpressionToken::Argument(T::from_str(slice).unwrap()),
        TokenType::Operator(precedence, associativity, lambda) => {
            ExpressionToken::Operator(*precedence, associativity, lambda)
        }
        TokenType::LeftBracket => ExpressionToken::LeftBracket,
        TokenType::RightBracket => ExpressionToken::RightBracket,
    }
}

/// Convert tokens from infix to postfix using Dijkstra's shunting-yard algorithm.
pub fn infix_to_postfix<T: num::Num>(
    tokens: ExpressionTokens<T>,
) -> Result<Vec<ExpressionToken<T>>, String> {
    let mut output: Vec<ExpressionToken<T>> = Vec::new();
    let mut stack: VecDeque<ExpressionToken<T>> = VecDeque::new();

    for token in tokens {
        match token {
            ExpressionToken::Argument(_) => {
                output.push(token);
            }
            ExpressionToken::Operator(precedence, associativity, _) => {
                //while there is something on the operator stack
                while let Some(popped_token) = stack.back() {
                    if let ExpressionToken::LeftBracket = popped_token {
                        break;
                    }
                    if let ExpressionToken::Operator(popped_prec, _, _) = popped_token {
                        if (*popped_prec > precedence)
                            || (*popped_prec == precedence && associativity == &Associativity::Left)
                        {
                            let popped_op = stack.pop_back().unwrap();
                            output.push(popped_op);
                        } else {
                            break;
                        }
                    }
                }
                //finally, push current operator onto stack
                stack.push_back(token);
            }
            ExpressionToken::LeftBracket => stack.push_back(token),
            ExpressionToken::RightBracket => loop {
                //special case, because we need to pop off the stack until we reach the left bracket, then cancel out both brackets

                //check if stack non empty
                let current_token = stack.pop_back();
                //we have closing bracket, but we didnt have any left bracket, so thats invalid expression
                if let None = current_token {
                    return Err("Unmatched right parenthesis in an expression.".to_string());
                }

                let current_token = current_token.unwrap();
                //if current token is an left bracket, then stop popping of the stack
                if let ExpressionToken::LeftBracket = current_token {
                    break;
                } else {
                    //not a left bracket, so keep popping of the stack
                    output.push(current_token);
                }
            },
        }
    }

    while let Some(token) = stack.pop_back() {
        match token {
            ExpressionToken::LeftBracket => {
                return Err("Unmatched left parenthesis in an expression.".to_string())
            }
            ExpressionToken::Argument(_) | ExpressionToken::Operator(_, _, _) => output.push(token),
            ExpressionToken::RightBracket => {
                panic!("No right brackets should be in stack after that, algorithm error.")
            }
        }
    }

    return Ok(output);
}

pub fn postfix_to_result<'rules, T>(postfix: &'rules Vec<ExpressionToken<T>>) -> Result<T, String>
where
    T: Num + Clone,
{
    let mut stack: VecDeque<T> = VecDeque::new();

    for token in postfix.iter() {
        match token {
            ExpressionToken::Argument(value) => stack.push_back(value.clone()),
            ExpressionToken::Operator(_, _, lambda) => {
                let arg2 = stack.pop_back();
                let arg1 = stack.pop_back();

                match  (arg1, arg2) {
                    (None, None) => return Err("Operator expected 2 arguments but met none.".to_string()),
                    (None, Some(_)) => return Err("Operator expected 2 arguments but met only 1".to_string()),
                    (Some(_), None) => return Err("Operator expected 2 arguments but met only 1.".to_string()),
                (Some(x), Some(y)) => stack.push_back(lambda(x, y)),

                }


            },
            ExpressionToken::LeftBracket |
            ExpressionToken::RightBracket => panic!("Encountered a bracket in reverse polish notation, meaning that calculation of rpn has a bug."),
        }
    }

    if stack.len() != 1 {
        return Err("Too much operators in input.".to_string());
    }

    if let Some(result) = stack.pop_back() {
        return Ok(result);
    }

    return Err("Cos poszlo nie tak".to_string());
}

pub fn calculate<'text, 'rules, T>(
    expr: &'text str,
    token_rules: &'rules TokenRules<T>,
) -> Result<T, String>
where
    T: Num + std::str::FromStr + Clone,
    <T as FromStr>::Err: std::fmt::Debug,
{
    let tokens = tokenize(&expr, &token_rules)?;
    let rpn = infix_to_postfix(tokens)?;
    let res = postfix_to_result(&rpn)?;
    return Ok(res);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    fn get_rules() -> TokenRules<f32> {
        types::TokenRulesBuilder::new()
            .add_default(TokenType::Argument(types::ArgumentType::Number))
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
            .compile()
    }

    #[test]
    fn calculate_simple_add() {
        let result = calculate("2 + 3", &get_rules());
        assert_eq!(result, Ok(5.));
    }

    #[test]
    fn calculate_simple_mult() {
        let result = calculate("2 * 3", &get_rules());
        assert_eq!(result, Ok(6.));
    }

    #[test]
    fn calculate_simple_1_term() {
        let result = calculate("89", &get_rules());
        assert_eq!(result, Ok(89.));
    }

    #[test]
    fn calculate_err_when_empty_input() {
        let result = calculate("", &get_rules());
        assert!(result.is_err())
    }

    #[test]
    fn calculate_omit_spaces() {
        let result = calculate("   12        -  8   ", &get_rules());
        assert_eq!(result, Ok(4.));

        let result = calculate("142        -9   ", &get_rules());
        assert_eq!(result, Ok(133.));

        let result = calculate("72+  15", &get_rules());
        assert_eq!(result, Ok(87.));

        let result = calculate(" 12*  4", &get_rules());
        assert_eq!(result, Ok(48.));

        let result = calculate(" 50/10", &get_rules());
        assert_eq!(result, Ok(5.));
    }

    #[test]
    fn calculate_handle_expressions_with_no_spaces() {
        let result = calculate("67+2", &get_rules());
        assert_eq!(result, Ok(69.));

        let result = calculate("2-7", &get_rules());

        assert_eq!(result, Ok(-5.));

        let result = calculate("5*7 ", &get_rules());
        assert_eq!(result, Ok(35.));

        let result = calculate("8/4", &get_rules());
        assert_eq!(result, Ok(2.));
    }
    #[test]
    fn calculate_add_left_to_right() {
        let result = calculate("2 -4 +6 -1 -1- 0 +8", &get_rules());
        assert_eq!(result, Ok(10.));

        let result = calculate("1 -1   + 2   - 2   +  4 - 4 +    6", &get_rules());
        assert_eq!(result, Ok(6.));
    }
    #[test]
    fn calculate_add_and_mult_mixed() {
        let result = calculate(" 2*3 - 4*5 + 6/3 ", &get_rules());
        assert_eq!(result, Ok(-12.));

        let result = calculate("2*3*4/8 -   5/2*4 +  6 + 0   /3   ", &get_rules());
        assert_eq!(result, Ok(-1.));
    }
    #[test]
    fn calculate_result_not_integer() {
        let result = calculate("10/4", &get_rules());
        assert_eq!(result, Ok(2.5));

        let result = calculate("5/3", &get_rules()).unwrap();
        approx::assert_relative_eq!(result, 1.66, max_relative = 0.01);

        let result = calculate("3 + 8/5 -1 -2*5", &get_rules()).unwrap();
        approx::assert_relative_eq!(result, -6.4, max_relative = 0.01)
    }

    #[test]
    fn calculate_error_on_invalid_token() {
        let result = calculate("  6 + c", &get_rules());
        assert!(result.unwrap_err().contains("c"));

        let result = calculate("  7 & 2", &get_rules());
        assert!(result.unwrap_err().contains("&"));

        let result = calculate("  %", &get_rules());
        assert!(result.unwrap_err().contains("%"));

        let result = calculate("  32+12bajojajo32", &get_rules());
        assert!(result.unwrap_err().contains("bajojajo"));
    }

    #[test]
    fn calculate_multiple_errors_included_in_err() {
        let result = calculate("  32+12bajo jajo32", &get_rules());
        let error_msg = result.unwrap_err();
        assert!(error_msg.contains("bajo"));
        assert!(error_msg.contains("jajo"));
    }

    #[test]
    fn calculate_err_on_syntax_error() {
        let result = calculate("5 + + 6", &get_rules());
        result.unwrap_err();

        let result = calculate("-5 + 6", &get_rules());
        result.unwrap_err();
    }

    #[test]
    fn calculate_infinity_on_division_by_0() {
        let result = calculate("5/0", &get_rules()).unwrap();
        assert_eq!(result, f32::INFINITY);

        let result = calculate(" 2 - 1 + 14/0 + 7", &get_rules()).unwrap();
        assert_eq!(result, f32::INFINITY);
    }

    #[test]
    fn calculate_1_term_in_parens() {
        let result = calculate("(2)", &get_rules()).unwrap();
        assert_eq!(result, 2.0);
    }

    #[test]
    fn calculate_nested_expressions() {
        let result = calculate("(5 + 2*3 - 1 + 7 * 8)", &get_rules()).unwrap();
        assert_eq!(result, 66.0);

        let result = calculate("(67 + 2 * 3 - 67 + 2/1 - 7)", &get_rules()).unwrap();
        assert_eq!(result, 1.0);
    }

    #[test]
    fn calculate_multiple_nested_expressions() {
        let result = calculate("(2) + (17*2-30) * (5)+2 - (8/2)*4", &get_rules()).unwrap();
        assert_eq!(result, 8.0);

        let result = calculate("(5*7/5) + (23) - 5 * (98-4)/(6*7-42)", &get_rules()).unwrap();
        assert_eq!(result, -f32::INFINITY);
    }

    #[test]
    fn calculate_handle_multiple_nested_parens() {
        let result = calculate("(((((5)))))", &get_rules()).unwrap();
        assert_eq!(result, 5.0);

        let result = calculate("(( ((2)) + 4))*((5))", &get_rules()).unwrap();
        assert_eq!(result, 30.0);
    }

    #[test]
    fn calculate_err_on_unbalanced_parens() {
        let result = calculate("(()", &get_rules());
        assert!(result.unwrap_err().contains("paren"));

        let result = calculate("2 + (5 * 2", &get_rules());
        assert!(result.unwrap_err().contains("paren"));

        let result = calculate("(((((4))))", &get_rules());
        assert!(result.unwrap_err().contains("paren"));

        let result = calculate("((2)) * ((3", &get_rules());
        assert!(result.unwrap_err().contains("paren"));

        let result = calculate("(()", &get_rules());
        assert!(result.unwrap_err().contains("paren"));

        let result = calculate("((9)) * ((1)", &get_rules());
        assert!(result.unwrap_err().contains("paren"));
    }
}
