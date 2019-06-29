mod build_bracket_map {
    use crate::{build_bracket_map, BfError, Token};
    use std::collections::HashMap;

    #[test]
    fn test_no_nest() {
        let tokens = vec![Token::Open, Token::Close];
        let actual = build_bracket_map(&tokens);
        let mut expected = HashMap::new();
        expected.insert(0, 1);
        assert!(actual.is_ok());
        assert_eq!(actual, Ok(expected));
    }

    #[test]
    fn test_one_nest() {
        let tokens = vec![Token::Open, Token::Open, Token::Close, Token::Close];
        let actual = build_bracket_map(&tokens);
        let mut expected = HashMap::new();
        expected.insert(0, 3);
        expected.insert(1, 2);
        assert!(actual.is_ok());
        assert_eq!(actual, Ok(expected));
    }

    #[test]
    fn test_complex_nest() {
        let tokens = vec![
            Token::Open,
            Token::Inc,
            Token::Inc,
            Token::Open,
            Token::Inc,
            Token::Close,
            Token::Open,
            Token::Open,
            Token::Open,
            Token::Open,
            Token::Close,
            Token::Next,
            Token::Close,
            Token::Inc,
            Token::Close,
            Token::Next,
            Token::Inc,
            Token::Close,
            Token::Dec,
            Token::Close,
        ];
        let actual = build_bracket_map(&tokens);
        let mut expected = HashMap::new();
        expected.insert(0, 19);
        expected.insert(3, 5);
        expected.insert(6, 17);
        expected.insert(7, 14);
        expected.insert(8, 12);
        expected.insert(9, 10);
        assert!(actual.is_ok());
        assert_eq!(actual, Ok(expected));
    }

    #[test]
    fn test_start_not_bracket() {
        let tokens = vec![Token::Inc, Token::Open, Token::Close];
        let actual = build_bracket_map(&tokens);
        let mut expected = HashMap::new();
        expected.insert(1, 2);
        assert!(actual.is_ok());
        assert_eq!(actual, Ok(expected));
    }

    #[test]
    fn test_end_not_bracket() {
        let tokens = vec![Token::Open, Token::Close, Token::Inc];
        let actual = build_bracket_map(&tokens);
        let mut expected = HashMap::new();
        expected.insert(0, 1);
        assert!(actual.is_ok());
        assert_eq!(actual, Ok(expected));
    }

    #[test]
    fn test_not_close() {
        let tokens = vec![Token::Open, Token::Inc];
        let actual = build_bracket_map(&tokens);
        assert!(actual.is_err());
        assert_eq!(actual, Err(BfError::InvalidBracketPair(0)));
    }

    #[test]
    fn test_start_close() {
        let tokens = vec![Token::Close];
        let actual = build_bracket_map(&tokens);
        assert!(actual.is_err());
        assert_eq!(actual, Err(BfError::InvalidBracketPair(0)));
    }
}

mod tokenize_code {
    use crate::{tokenize_code, BfError, Token};

    #[test]
    fn test_ok() {
        let src = "+-,.<>[]";
        let actual = tokenize_code(src);
        assert!(actual.is_ok());
        assert_eq!(
            actual,
            Ok(vec![
                Token::Inc,
                Token::Dec,
                Token::GetChar,
                Token::PutChar,
                Token::Prev,
                Token::Next,
                Token::Open,
                Token::Close
            ]),
        );
    }

    #[test]
    fn test_tokenize_error() {
        let src = "a.";
        let actual = tokenize_code(src);
        assert!(actual.is_err());
        assert_eq!(actual, Err(BfError::TokenizeError));
    }
}

mod memory {
    use crate::{BfError, BracketMap, Memory, Token};
    use std::collections::HashMap;

    #[test]
    fn test_new() {
        let size: usize = 10;
        let tokens = vec![Token::Inc, Token::Dec];
        let mut bracket_map: BracketMap = HashMap::new();
        bracket_map.insert(1, 3);
        bracket_map.insert(4, 5);
        let mut reverse_bracket_map: BracketMap = HashMap::new();
        reverse_bracket_map.insert(3, 1);
        reverse_bracket_map.insert(5, 4);
        let memory = Memory::new(size, tokens.clone(), bracket_map.clone());
        assert_eq!(memory.cells, vec![0; size]);
        assert_eq!(memory.cell_pos, 0);
        assert_eq!(memory.tokens, tokens);
        assert_eq!(memory.bracket_map, bracket_map);
        assert_eq!(memory.reverse_bracket_map, reverse_bracket_map);
    }

    #[test]
    fn test_prev_cell() {
        let mut memory = Memory::new(3, vec![], HashMap::new());
        memory.cell_pos += 1;
        memory.prev_cell();
        assert_eq!(memory.cell_pos, 0);
    }

    #[test]
    fn test_next_cell() {
        let mut memory = Memory::new(3, vec![], HashMap::new());
        memory.next_cell();
        assert_eq!(memory.cell_pos, 1);
    }

    #[test]
    fn test_next_token() {
        let expected_token = Token::Dec;
        let tokens = vec![Token::Inc, expected_token];
        let mut memory = Memory::new(3, tokens.clone(), HashMap::new());
        memory.next_token();
        assert_eq!(memory.current_token(), expected_token);
    }

    #[test]
    fn test_increment() {
        let mut memory = Memory::new(3, vec![], HashMap::new());
        memory.cells = vec![0];
        memory.increment();
        assert_eq!(memory.cells, vec![1]);
    }

    #[test]
    fn test_decrement() {
        let mut memory = Memory::new(1, vec![], HashMap::new());
        memory.cells = vec![1];
        memory.decrement();
        assert_eq!(memory.cells, vec![0]);
    }

    #[test]
    fn test_open_jump_behind_of_correspond_close_bracket() {
        let mut bracket_map = HashMap::new();
        bracket_map.insert(0, 2);
        let mut memory = Memory::new(
            3,
            vec![Token::Open, Token::Inc, Token::Close, Token::Inc],
            bracket_map,
        );
        let result = memory.open();
        assert!(result.is_ok());
        assert_eq!(memory.token_pos, 2);
    }

    #[test]
    fn test_raise_not_found_close_bracket() {
        let tokens = vec![Token::Open, Token::Inc];
        let mut memory = Memory::new(2, tokens, HashMap::new());
        let result = memory.open();
        assert_eq!(result, Err(BfError::NotFoundCloseBracket(0)));
    }

    #[test]
    fn test_close_jump_behind_of_correspond_open_bracket() {
        let mut bracket_map = HashMap::new();
        bracket_map.insert(1, 3);
        let mut memory = Memory::new(
            3,
            vec![Token::Inc, Token::Open, Token::Inc, Token::Close],
            bracket_map,
        );
        memory.token_pos = 3;
        memory.cells = vec![1, 0, 0];
        let result = memory.close();
        assert!(result.is_ok());
    }

    #[test]
    fn test_raise_not_found_open_bracket() {
        let tokens = vec![Token::Close, Token::Inc];
        let mut memory = Memory::new(2, tokens, HashMap::new());
        memory.cells = vec![1, 0];
        let result = memory.close();
        assert_eq!(result, Err(BfError::NotFoundOpenBracket(0)));
    }
}

mod eval {
    use crate::eval;

    #[test]
    fn test_inc_dec() {
        let src = "+++++--";
        let actual = eval(src, 1);
        assert!(actual.is_ok());
        assert_eq!(actual.unwrap().cells, vec![3]);
    }

    #[test]
    fn test_move_ptr() {
        let src = "++++>+>++->+<<<-";
        let actual = eval(src, 4);
        assert!(actual.is_ok());
        assert_eq!(actual.unwrap().cells, vec![3, 1, 1, 1]);
    }

    #[test]
    fn test_loop() {
        let src = "+[-]";
        let actual = eval(src, 1);
        assert!(actual.is_ok());
        assert_eq!(actual.unwrap().cells, vec![0]);
    }

    #[test]
    fn test_nested_loop() {
        let src = "+++++[>+++[>+++<-]<-]";
        let actual = eval(src, 3);
        assert!(actual.is_ok());
        assert_eq!(actual.unwrap().cells, vec![0, 0, 45]);
    }

    #[test]
    fn test_many_symbol() {
        let src = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++";
        let actual = eval(src, 1);
        assert!(actual.is_ok());
    }

    #[test]
    fn test_hello_world() {
        let src = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++.";
        let actual = eval(src, 100000);
        assert!(actual.is_ok());
    }
}
