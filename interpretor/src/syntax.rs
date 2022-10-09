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
	Unop(FloatUnop, Box<FloatRvalue<'a>>),
	Binop(FloatBinop, Box<FloatRvalue<'a>>, Box<FloatRvalue<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatUnop { Ident, Neg, Whole }
impl FloatUnop {
	pub fn evaluate(&self, x: f32) -> f32 {
		match self {
			FloatUnop::Ident => x,
			FloatUnop::Neg   => -x,
			FloatUnop::Whole => x.floor(),
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			FloatUnop::Ident => "+",
			FloatUnop::Neg   => "-",
			FloatUnop::Whole => "[...]",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatBinop { Add, Sub, Mul, Div, Rem }
impl FloatBinop {
	pub fn evaluate(&self, x: f32, y: f32) -> f32 {
		match &self {
			FloatBinop::Add => x + y,
			FloatBinop::Sub => x - y,
			FloatBinop::Mul => x * y,
			FloatBinop::Div => x / y,
			FloatBinop::Rem => ((x as i32) % (y as i32)) as f32,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match &self {
			FloatBinop::Add => "+",
			FloatBinop::Sub => "-",
			FloatBinop::Mul => "*",
			FloatBinop::Div => "/",
			FloatBinop::Rem => "%",
		}
	}
}

// bool stuff
#[derive(Debug)]
pub enum BoolRvalue<'a> {
	BoolFloatBinop(BoolFloatBinop, FloatRvalue<'a>, FloatRvalue<'a>),
	BoolBoolBinop(BoolBoolBinop, Box<BoolRvalue<'a>>, Box<BoolRvalue<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolUnop { Ident }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolFloatBinop {
	Eq, Neq, Lt, Gt, Lte, Gte,
	Divides,
}
impl BoolFloatBinop {
	pub fn evaluate(&self, x: f32, y: f32) -> bool {
		match self {
			BoolFloatBinop::Eq      => (x - y).abs() <= EPSILON,
			BoolFloatBinop::Neq     => (x - y).abs() > EPSILON,
			BoolFloatBinop::Lt      => x < y,
			BoolFloatBinop::Gt      => x > y,
			BoolFloatBinop::Lte     => x <= y,
			BoolFloatBinop::Gte     => x >= y,
			BoolFloatBinop::Divides => (y as i32) % (x as i32) == 0,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			BoolFloatBinop::Eq      => "=",
			BoolFloatBinop::Neq     => "!=",
			BoolFloatBinop::Lt      => "<",
			BoolFloatBinop::Gt      => ">",
			BoolFloatBinop::Lte     => "<=",
			BoolFloatBinop::Gte     => ">=",
			BoolFloatBinop::Divides => "|",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolBoolBinop {
	And, Or,
}
impl BoolBoolBinop{
	pub fn evaluate(&self, x: bool, y: bool) -> bool {
		match self {
			BoolBoolBinop::And => x && y,
			BoolBoolBinop::Or  => x || y,
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			BoolBoolBinop::And => "și",
			BoolBoolBinop::Or  => "sau",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolBinop {
	BoolFloatBinop(BoolFloatBinop),
	BoolBoolBinop(BoolBoolBinop),
}

