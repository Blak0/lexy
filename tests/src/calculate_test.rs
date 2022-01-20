#[cfg(test)]
mod tests {
    use lexer::{calculate, ArgumentType, Associativity, TokenRules, TokenRulesBuilder, TokenType};
    fn get_rules() -> TokenRules<f32> {
        TokenRulesBuilder::new()
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
