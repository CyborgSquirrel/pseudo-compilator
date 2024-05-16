use enumflags2::make_bitflags;
use indoc::indoc;

use crate::parse::ValueType;

use super::{
	parse, 
	LanguageSettings,
	ParserErrorKind::*,
	expression::Expecting,
};

macro_rules! test_parser_err {
	{ $function:ident, ($line:expr , $column:expr , $kind:expr $(,)?), $code:expr } => {
		#[test]
		fn $function() {
			let code = indoc!{ $code };
			print!("===\n{code}\n===\n");
			let language_settings = LanguageSettings {
				epsilon: LanguageSettings::DEFAULT_EPSILON,
				enable_list: true,
			};
			let result = parse(&language_settings, code);
			let err = result.unwrap_err();
			assert_eq!(
				($line, $column, $kind),
				(err.0.line_one(), err.0.column(), err.1),
			)
		}
	}
}

macro_rules! nop {
	( $val:tt ) => { }
}

macro_rules! test_parser_err_no_list {
	{ $function:ident, ($line:expr , $column:expr , $($kind:expr $(,)?)?), $code:expr } => {
		#[test]
		fn $function() {
			let code = indoc!{ $code };
			print!("===\n{code}\n===\n");
			let language_settings = LanguageSettings {
				epsilon: LanguageSettings::DEFAULT_EPSILON,
				enable_list: false,
			};
			let result = parse(&language_settings, code);
			let err = result.unwrap_err();
			assert_eq!(
				($line, $column $(, $kind)?),
				(err.0.line_one(), err.0.column() $(, { nop!($kind); err.1})?),
			)
		}
	}
}

macro_rules! test_parser_ok {
	{ $function:ident, $code:expr } => {
		#[test]
		fn $function() {
			let code = indoc! { $code };
			print!("===\n{code}\n===\n");
			let language_settings = LanguageSettings {
				epsilon: LanguageSettings::DEFAULT_EPSILON,
				enable_list: true,
			};
			let result = parse(&language_settings, code);
			result.unwrap();
		}
	}
}

// expressions
test_parser_err! {
	missing_operand_or_unop,
	(
		1, 15,
		ExpectedSomethingElse(make_bitflags!(Expecting::{PrefixUnop | Rvalue})),
	),
	r#"scrie   4141+  "#
}

test_parser_err! {
	missing_operand,
	(
		1, 16,
		ExpectedSomethingElse(make_bitflags!(Expecting::{Rvalue})),
	),
	r#"scrie   4141+-  "#
}

test_parser_err! {
	mismatched_parens,
	(
		1, 10,
		MismatchedParens,
	),
	r#"scrie [1+2)"#
}

test_parser_err! {
	too_many_unops,
	(
		1, 7,
		ExpectedSomethingElse(make_bitflags!(Expecting::{Rvalue})),
	),
	r#"scrie ++41+1"#
}

test_parser_err! {
	empty_parens,
	(
		1, 4,
		ExpectedSomethingElse(make_bitflags!(Expecting::{PrefixUnop | Rvalue})),
	),
	r#"a<-()"#
}

test_parser_err! {
	unclosed_lparen,
	(
		1, 3,
		UnclosedLParen,
	),
	r#"a<-(x+y"#
}

test_parser_err! {
	unclosed_rparen,
	(
		1, 6,
		UnclosedRParen,
	),
	r#"a<-x+y)"#
}

test_parser_err! {
	invalid_float_unop_operands,
	(
		1, 6,
		ExpectedValueType(make_bitflags!(ValueType::{Float})),
	),
	r#"
		daca +(1=2 sau 3<5 si 4=4) atunci
			scrie "ok"
	"#
}

test_parser_err! {
	invalid_float_literal,
	(
		1, 3,
		InvalidFloatLiteral,
	),
	r#"a<-41yeet41"#
}

// other stuff
test_parser_err! {
	invalid_indent,
	(
		2, 1,
		InvalidIndent,
	),
	r#"
		scrie "ok"
			scrie "nope"
	"#
}

test_parser_err! {
	altfel_without_daca,
	(
		3, 0,
		AltfelWithoutDaca,
	),
	r#"
		scrie 41
		
		altfel
	"#
}

test_parser_err! {
	daca_already_has_altfel,
	(
		7, 0,
		DacaAlreadyHasAltfel,
	),
	r#"
		scrie 41
		
		daca 41=41 atunci
			scrie "ok"
		altfel
			scrie "catastrofa"
		altfel
			scrie "wut"
	"#
}

test_parser_err! {
	empty_block_daca,
	(
		1, 0,
		EmptyBlock,
	),
	r#"
		daca 40+1=41 atunci
			
		scrie "bun"
	"#
}

test_parser_err! {
	empty_block_altfel,
	(
		3, 0,
		EmptyBlock,
	),
	r#"
		daca 40+1=41 atunci
			scrie "bun"
		altfel
		scrie "ne-bun"
	"#
}

test_parser_err! {
	empty_block_repeta_pana_cand,
	(
		7, 0,
		EmptyBlock,
	),
	r#"
		repeta
			
			
			
			
			
		pana cand 41=42-1
	"#
}

test_parser_err! {
	repeta_without_pana_cand,
	(
		1, 0,
		RepetaWithoutPanaCand,
	),
	r#"
		repeta
			
			
	"#
}

test_parser_err! {
	repeta_without_pana_cand_short,
	(
		1, 0,
		RepetaWithoutPanaCand,
	),
	r#"repeta"#
}

test_parser_err! {
	recursive_instruction_after_nonrecursive_instruction,
	(
		1, 17,
		ExpectedBlocklessInstruction,
	),
	r#"
		scrie 12+3 ; daca a=12 atunci
	"#
}

test_parser_err! {
	invalid_assignment_daca,
	(
		1, 7,
		InvalidIdent(String::from("daca")),
	),
	r#"
		daca <- 42
	"#
}

test_parser_err! {
	invalid_assignment_pentru,
	(
		1, 9,
		InvalidIdent(String::from("pentru")),
	),
	r#"
		pentru <- 42
	"#
}

test_parser_err! {
	invalid_assignment_daca_second,
	(
		1, 16,
		InvalidIdent(String::from("daca"))
	),
	r#"
		x <- 41; daca <- 42
	"#
}

test_parser_err! {
	assignment_type_error,
	(
		1, 5,
		ExpectedValueType(make_bitflags!(ValueType::{Float | List})),
	),
	r#"
		x <- 41 < 12
	"#
}

test_parser_err! {
	daca_type_error,
	(
		1, 5,
		ExpectedValueType(make_bitflags!(ValueType::{Bool})),
	),
	r#"
		daca 41 atunci
			scrie "da"
	"#
}

test_parser_err! {
	expected_lvalue,
	(
		1, 0,
		ExpectedLvalue,
	),
	r#"
		x+41 <- 42
	"#
}

test_parser_ok! {
	parse_list_literal,
	r#"
		a <- 1,2,3
	"#
}

test_parser_ok! {
	parse_citeste,
	r#"
		a <- 1,2,3
		b <- 41; c <- 42
		citeste a[0], b, c, a[1]
	"#
}

test_parser_ok! {
	parse_cat_ident,
	r#"
		cat <- 42
		a <- 41
		cat <-> a

		x <- (1+2+cat) * cat

		cât <- cat
	"#
}

test_parser_err_no_list! {
	no_list_parse_list_literal,
	(
		1, 6,
		ExpectedEnd,
	),
	r#"
		a <- 1,2,3
	"#
}

test_parser_err_no_list! {
	no_list_parse_list_empty_literal,
	(
		1, 5,
	),
	r#"
		a <- ,
	"#
}

test_parser_err_no_list! {
	no_list_parse_list_lvalue,
	(
		1, 1,
	),
	r#"
		a[0] <- 41
	"#
}

test_parser_err_no_list! {
	no_list_parse_list_rvalue,
	(
		1, 5,
	),
	r#"
		x <- (a[0]+41)*42
	"#
}

test_parser_err_no_list! {
	no_list_parse_list_length,
	(
		1, 14,
		InvalidFunction(String::from("lungime")),
	),
	r#"
		x <- lungime(a)
	"#
}
