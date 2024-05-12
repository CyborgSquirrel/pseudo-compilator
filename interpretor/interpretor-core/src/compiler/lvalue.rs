use inkwell::values::{FloatValue, StructValue, AnyValue};

use crate::{Compiler, CompilerError, ast::{Lvalue, FloatLvalue, ListLvalue}};

use super::{Compile, variable::SetVariable};

pub trait CompileLvalue<'src, 'ctx> {
	type Output;

	fn compile_store(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
		value: StructValue<'ctx>,
	) -> Result<(), CompilerError>;

	fn compile_load(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<Self::Output, CompilerError>;
}

impl<'src, 'ctx> CompileLvalue<'src, 'ctx> for FloatLvalue<'src> {
	type Output = FloatValue<'ctx>;

	fn compile_store(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
		value: StructValue<'ctx>,
	) -> Result<(), CompilerError> {
		Ok(match self {
			FloatLvalue::Variable(_) => unreachable!(),
			FloatLvalue::ListElement(list, index) => {
				let inner = list.compile(compiler)?;
				let index = index.compile(compiler)?;

				compiler.build_list_range_check(inner, index)?;

				let value_ptr = compiler.builder.build_alloca(compiler.external.variable, "value_ptr")?;
				compiler.builder.build_store(value_ptr, value)?;
				let value = SetVariable { value_ptr };
				let value = value.build_load_float(compiler)?;

				compiler.builder.build_call(
					compiler.external.pseudo_list_set_item,
					&[
						inner.into(),
						index.into(),
						value.into(),
					],
					"pseudo_list_set_item",
				)?;
			}
		})
  }

  fn compile_load(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<Self::Output, CompilerError> {
		Ok(match self {
			FloatLvalue::Variable(ident) => {
				let x = compiler.variable(ident)?;
				x.build_set_check(compiler)?.build_load_float(compiler)?
			}
			FloatLvalue::ListElement(list, index) => {
				let inner = list.compile(compiler)?;
				let index = index.compile(compiler)?;

				compiler.build_list_range_check(inner, index)?;

				let call = compiler.builder.build_call(
					compiler.external.pseudo_list_get_item,
					&[
						inner.into(),
						index.into(),
					],
					"pseudo_list_get_item",
				)?;
				let value = call.as_any_value_enum().into_float_value();
				value
			}
		})
  }
}

impl<'src, 'ctx> CompileLvalue<'src, 'ctx> for ListLvalue<'src> {
	type Output = StructValue<'ctx>;

	fn compile_store(
		&self,
		_compiler: &mut Compiler<'src, 'ctx>,
		_value: StructValue<'ctx>,
	) -> Result<(), CompilerError> {
		unreachable!()
  }

  fn compile_load(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<Self::Output, CompilerError> {
		Ok(match self {
			ListLvalue::Variable(ident) => {
				compiler.variable(ident)?.build_set_check(compiler)?.build_load(compiler)?
			}
		})
  }
}

impl<'src, 'ctx> CompileLvalue<'src, 'ctx> for Lvalue<'src> {
	type Output = StructValue<'ctx>;

	fn compile_store(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
		value: StructValue<'ctx>,
	) -> Result<(), CompilerError> {
		Ok(match self {
	    Lvalue::Float(inner) => inner.compile_store(compiler, value)?,
	    Lvalue::Unknown(ident) => {
				let x = compiler.variable(ident)?;
				x.build_store(compiler, value)?;
	    }
	    Lvalue::List(_inner) => unreachable!(),
		})
  }

  fn compile_load(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<Self::Output, CompilerError> {
		Ok(match self {
			Lvalue::Float(inner) => {
				let x = inner.compile_load(compiler)?;
				let x = compiler.build_float_struct(x)?;
				compiler.builder.build_load(compiler.external.variable, x, "load_float")?.into_struct_value()
			}
			Lvalue::List(inner) => inner.compile_load(compiler)?,
			Self::Unknown(ident) => {
				compiler.variable(ident)?.build_set_check(compiler)?.build_load(compiler)?
			}
		})
  }
}
