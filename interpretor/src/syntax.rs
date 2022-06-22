use crate::runtime::EPSILON;

#[derive(Debug)]
pub enum Instructiune<'a> {
	Atribuire(Lvalue<'a>, FloatRvalue<'a>),
	Interschimbare(Lvalue<'a>, Lvalue<'a>),
	Scrie(Vec<ScrieParam<'a>>),
	Citeste(Vec<Lvalue<'a>>),
	DacaAtunciAltfel(BoolRvalue<'a>, Vec<Instructiune<'a>>, Option<Vec<Instructiune<'a>>>),
	CatTimpExecuta(BoolRvalue<'a>, Vec<Instructiune<'a>>),
	PentruExecuta(Lvalue<'a>, FloatRvalue<'a>, FloatRvalue<'a>, Option<FloatRvalue<'a>>, Vec<Instructiune<'a>>),
	RepetaPanaCand(Vec<Instructiune<'a>>, BoolRvalue<'a>),
}

#[derive(Debug)]
pub enum ScrieParam<'a> {
	Rvalue(FloatRvalue<'a>),
	StringLiteral(&'a str),
	CharacterLiteral(&'a str),
}

// float stuff
#[derive(Debug)]
pub struct Lvalue<'a> (pub &'a str);

#[derive(Debug)]
pub enum FloatRvalue<'a> {
	Literal(f32),
	Lvalue(Lvalue<'a>),
	UnaryOp(FloatUnaryOp, Box<FloatRvalue<'a>>),
	BinaryOp(FloatBinaryOp, Box<FloatRvalue<'a>>, Box<FloatRvalue<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatUnaryOp { Ident, Neg, Whole }
impl FloatUnaryOp {
	pub fn evaluate(&self, x: f32) -> f32 {
		match self {
			FloatUnaryOp::Ident => x,
			FloatUnaryOp::Neg   => -x,
			FloatUnaryOp::Whole => x.floor(),
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			FloatUnaryOp::Ident => "+",
			FloatUnaryOp::Neg   => "-",
			FloatUnaryOp::Whole => "[...]",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatBinaryOp { Add, Sub, Mul, Div, Rem }
impl FloatBinaryOp {
	pub fn evaluate(&self, x: f32, y: f32) -> f32 {
		match &self {
			FloatBinaryOp::Add => x + y,
			FloatBinaryOp::Sub => x - y,
			FloatBinaryOp::Mul => x * y,
			FloatBinaryOp::Div => x / y,
			FloatBinaryOp::Rem => ((x as i32) % (y as i32)) as f32,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match &self {
			FloatBinaryOp::Add => "+",
			FloatBinaryOp::Sub => "-",
			FloatBinaryOp::Mul => "*",
			FloatBinaryOp::Div => "/",
			FloatBinaryOp::Rem => "%",
		}
	}
}

// bool stuff
#[derive(Debug)]
pub enum BoolRvalue<'a> {
	BoolFloatBinaryOp(BoolFloatBinaryOp, FloatRvalue<'a>, FloatRvalue<'a>),
	BoolBoolBinaryOp(BoolBoolBinaryOp, Box<BoolRvalue<'a>>, Box<BoolRvalue<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolUnaryOp { Ident }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolFloatBinaryOp {
	Eq, Neq, Lt, Gt, Lte, Gte,
	Divides,
}
impl BoolFloatBinaryOp {
	pub fn evaluate(&self, x: f32, y: f32) -> bool {
		match self {
			BoolFloatBinaryOp::Eq      => (x - y).abs() <= EPSILON,
			BoolFloatBinaryOp::Neq     => (x - y).abs() > EPSILON,
			BoolFloatBinaryOp::Lt      => x < y,
			BoolFloatBinaryOp::Gt      => x > y,
			BoolFloatBinaryOp::Lte     => x <= y,
			BoolFloatBinaryOp::Gte     => x >= y,
			BoolFloatBinaryOp::Divides => (y as i32) % (x as i32) == 0,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			BoolFloatBinaryOp::Eq      => "=",
			BoolFloatBinaryOp::Neq     => "!=",
			BoolFloatBinaryOp::Lt      => "<",
			BoolFloatBinaryOp::Gt      => ">",
			BoolFloatBinaryOp::Lte     => "<=",
			BoolFloatBinaryOp::Gte     => ">=",
			BoolFloatBinaryOp::Divides => "|",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolBoolBinaryOp {
	And, Or,
}
impl BoolBoolBinaryOp{
	pub fn evaluate(&self, x: bool, y: bool) -> bool {
		match self {
			BoolBoolBinaryOp::And => x && y,
			BoolBoolBinaryOp::Or  => x || y,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			BoolBoolBinaryOp::And => "și",
			BoolBoolBinaryOp::Or  => "sau",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolBinaryOp {
	BoolFloatBinaryOp(BoolFloatBinaryOp),
	BoolBoolBinaryOp(BoolBoolBinaryOp),
}

