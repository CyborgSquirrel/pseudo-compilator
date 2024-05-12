use crate::runtime::EPSILON;

#[derive(Debug)]
pub struct Location {
	line: u32,
	column: u32,
}

impl Location {
  pub fn new(line: u32, column: u32) -> Self {
  	Self {
  		line,
  		column,
  	}
  }
	pub fn line(&self) -> u32 { self.line }
	pub fn column(&self) -> u32 { self.column }
}

#[derive(Debug)]
pub struct Node<T> {
	pub location: Location,
	pub inner: T,
}

impl<T> Node<T> {
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.inner
	}

	pub fn inner(&self) -> &T {
		&self.inner
	}
}

pub type InstructiuneNode<'src> = Node<Instructiune<'src>>;

#[derive(Debug)]
pub enum Instructiune<'src> {
	Atribuire(Lvalue<'src>, AtribuireRvalue<'src>),
	Interschimbare(Lvalue<'src>, Lvalue<'src>),
	Scrie(Vec<ScrieParam<'src>>),
	Citeste(Vec<Ident<'src>>),
	DacaAtunciAltfel(BoolRvalue<'src>, Vec<InstructiuneNode<'src>>, Option<Vec<InstructiuneNode<'src>>>),
	CatTimpExecuta(BoolRvalue<'src>, Vec<InstructiuneNode<'src>>),
	PentruExecuta(Ident<'src>, FloatRvalue<'src>, FloatRvalue<'src>, Option<FloatRvalue<'src>>, Vec<InstructiuneNode<'src>>),
	RepetaPanaCand(Vec<InstructiuneNode<'src>>, BoolRvalue<'src>),
}

#[derive(Debug)]
pub enum ScrieParam<'src> {
	Rvalue(FloatRvalue<'src>),
	StringLiteral(&'src str),
	CharacterLiteral(&'src str),
}

#[derive(Debug)]
pub enum AtribuireRvalue<'src> {
	Float(FloatRvalue<'src>),
	List(ListRvalue<'src>),
	Unknown(Ident<'src>),
}

#[derive(Debug)]
pub enum Lvalue<'src> {
	Float(FloatLvalue<'src>),
	List(ListLvalue<'src>),
	Unknown(Ident<'src>),
}

// float stuff
#[derive(Debug)]
pub struct Ident<'src> (pub &'src str);

#[derive(Debug)]
pub enum FloatLvalue<'src> {
	Variable(Ident<'src>),
	ListElement(ListRvalue<'src>, Box<FloatRvalue<'src>>),
}

#[derive(Debug)]
pub enum FloatRvalue<'src> {
	Lvalue(FloatLvalue<'src>),
	Literal(f32),
	Unop(FloatUnop, Box<FloatRvalue<'src>>),
	ListLength(ListRvalue<'src>),
	Binop(FloatBinop, Box<FloatRvalue<'src>>, Box<FloatRvalue<'src>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatUnop { Ident, Neg, IntegralPart }
impl FloatUnop {
	pub fn evaluate(&self, x: f32) -> f32 {
		match self {
			FloatUnop::Ident => x,
			FloatUnop::Neg   => -x,
			FloatUnop::IntegralPart => x.floor(),
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			FloatUnop::Ident => "+",
			FloatUnop::Neg   => "-",
			FloatUnop::IntegralPart => "[...]",
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

#[derive(Debug)]
pub enum ListLvalue<'a> {
	Variable(Ident<'a>),
}

#[derive(Debug)]
pub enum ListRvalue<'a> {
	Literal(Vec<FloatRvalue<'a>>),
	Lvalue(ListLvalue<'a>),
}
