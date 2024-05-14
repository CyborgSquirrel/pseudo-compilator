use crate::{EPSILON, source::Node};

pub type InstructiuneNode<'src> = Node<Instructiune<'src>>;

#[derive(Debug)]
pub enum Instructiune<'src> {
	Atribuire(
		/// Lvalue
		Lvalue<'src>,
		/// Rvalue
		AtribuireRvalue<'src>,
	),
	Interschimbare(
		/// Left
		Lvalue<'src>,
		/// Right
		Lvalue<'src>,
	),
	Insereaza(
		// List
		IdentNode<'src>,
		// Index
		FloatRvalueNode<'src>,
		/// Value
		FloatRvalueNode<'src>,
	),
	Sterge(
		/// List
		IdentNode<'src>,
		/// Index
		FloatRvalueNode<'src>,
	),
	Scrie(
		Vec<ScrieParam<'src>>,
	),
	Citeste(
		Vec<Lvalue<'src>>,
	),
	DacaAtunciAltfel(
		/// Conditie
		BoolRvalueNode<'src>,
		/// Atunci branch
		Vec<InstructiuneNode<'src>>,
		/// Altfel branch
		Option<Vec<InstructiuneNode<'src>>>,
	),
	CatTimpExecuta(
		/// Conditie
		BoolRvalueNode<'src>,
		/// Repeated instructions
		Vec<InstructiuneNode<'src>>,
	),
	PentruExecuta(
		/// Contor
		IdentNode<'src>,
		/// Start
		FloatRvalueNode<'src>,
		/// Stop
		FloatRvalueNode<'src>,
		/// Increment
		Option<FloatRvalueNode<'src>>,
		/// Repeated instructions
		Vec<InstructiuneNode<'src>>
	),
	RepetaPanaCand(
		/// Repeated instructions
		Vec<InstructiuneNode<'src>>,
		/// Conditie
		BoolRvalueNode<'src>,
	),
}

// special params for particular instructions
#[derive(Debug)]
pub enum ScrieParam<'src> {
	Rvalue(FloatRvalueNode<'src>),
	StringLiteral(&'src str),
	CharacterLiteral(&'src str),
}

#[derive(Debug)]
pub enum AtribuireRvalue<'src> {
	Float(FloatRvalueNode<'src>),
	List(ListRvalueNode<'src>),
	Unknown(IdentNode<'src>),
}

// expressions
#[derive(Debug)]
pub struct Ident<'src>(pub &'src str);
pub type IdentNode<'src> = Node<Ident<'src>>;

#[derive(Debug)]
pub enum Lvalue<'src> {
	Float(FloatLvalueNode<'src>),
	List(ListLvalue<'src>),
	Unknown(IdentNode<'src>),
}

// float expressions
#[derive(Debug)]
pub enum FloatLvalue<'src> {
	Variable(IdentNode<'src>),
	ListElement(ListRvalueNode<'src>, Box<FloatRvalueNode<'src>>),
}
pub type FloatLvalueNode<'src> = Node<FloatLvalue<'src>>;

#[derive(Debug)]
pub enum FloatRvalue<'src> {
	Lvalue(FloatLvalueNode<'src>),
	Literal(f32),
	Unop(FloatUnop, Box<FloatRvalueNode<'src>>),
	ListLength(ListRvalueNode<'src>),
	Binop(FloatBinop, Box<FloatRvalueNode<'src>>, Box<FloatRvalueNode<'src>>),
	Unknown(UnknownRvalueNode<'src>),
}
pub type FloatRvalueNode<'src> = Node<FloatRvalue<'src>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FloatUnop { Identity, Neg, IntegralPart }
impl FloatUnop {
	pub fn evaluate(&self, x: f32) -> f32 {
		match self {
			FloatUnop::Identity => x,
			FloatUnop::Neg   => -x,
			FloatUnop::IntegralPart => x.floor(),
		}
	}
	pub fn get_str(&self) -> &'static str {
		match self {
			FloatUnop::Identity => "+",
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

// bool expressions
#[derive(Debug)]
pub enum BoolRvalue<'src> {
	BoolUnop(BoolUnop, Box<BoolRvalueNode<'src>>),
	BoolFloatBinop(BoolFloatBinop, FloatRvalueNode<'src>, FloatRvalueNode<'src>),
	BoolBoolBinop(BoolBoolBinop, Box<BoolRvalueNode<'src>>, Box<BoolRvalueNode<'src>>),
}
pub type BoolRvalueNode<'src> = Node<BoolRvalue<'src>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolUnop { Identity }

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

// list expressions
#[derive(Debug)]
pub enum ListLvalue<'src> {
	Variable(IdentNode<'src>),
}

#[derive(Debug)]
pub enum ListRvalue<'src> {
	Literal(Vec<FloatRvalueNode<'src>>),
	Lvalue(ListLvalue<'src>),
	Unknown(UnknownRvalueNode<'src>),
}
pub type ListRvalueNode<'src> = Node<ListRvalue<'src>>;

// unknown expressions
#[derive(Debug)]
pub enum UnknownRvalue<'src> {
	Ident(IdentNode<'src>),
	Identity(Box<UnknownRvalueNode<'src>>),
}
pub type UnknownRvalueNode<'src> = Node<UnknownRvalue<'src>>;
