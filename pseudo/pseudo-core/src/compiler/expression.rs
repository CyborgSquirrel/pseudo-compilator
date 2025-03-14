use inkwell::{values::{FloatValue, PointerValue, IntValue, AnyValue}, FloatPredicate, IntPredicate};

use crate::{ast::{FloatRvalue, FloatUnop, FloatBinop, BoolRvalue, BoolFloatBinop, BoolBoolBinop, ListLvalue, ListRvalue, ListRvalueNode, BoolRvalueNode, FloatRvalueNode, UnknownRvalueNode, UnknownRvalue, BoolUnop}, Compiler, source::Node};

use super::{Compile, error::CompilerError, lvalue::CompileLvalue, variable::Variable};

impl<'src, 'ctx> Compile<'src, 'ctx> for UnknownRvalueNode<'src> {
	type Output = Node<Variable<'ctx>>;

	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
    Ok(
    	match self.inner() {
    		UnknownRvalue::Ident(ident) => compiler.variable(ident)?,
    		UnknownRvalue::Identity(value) => value.compile(compiler)?,
    	}
    )
  }
}

impl<'src, 'ctx> Compile<'src, 'ctx> for FloatRvalueNode<'src> {
	type Output = FloatValue<'ctx>;

	/// Returns the inner float.
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		Ok(
			match self.inner() {
				FloatRvalue::Lvalue(x) => x.compile_load(compiler)?,
				FloatRvalue::Unknown(x) => {
					let x = x.compile(compiler)?;
					x.build_set_check(compiler)?.build_load_float(compiler)?
				}
				FloatRvalue::Literal(x) => compiler.context.f64_type().const_float(*x as f64),
				FloatRvalue::ListLength(list) => {
					let list = list.compile(compiler)?;
					
	  			// call function to get list length
					let call = compiler.builder.build_call(
						compiler.external.pseudo_list_len,
						&[
							list.into(),
						],
						"pseudo_list_len",
					)?;
					let list_len = call.as_any_value_enum().into_float_value();

					list_len
				}
				FloatRvalue::Unop(unop, x) => {
					let x = x.compile(compiler)?;
					match unop {
						FloatUnop::Identity => x,
						FloatUnop::Neg => compiler.builder.build_float_neg(x, "tmp_neg")?,
						FloatUnop::IntegralPart => {
							let trunc_int = compiler.builder.build_float_to_signed_int(
								x,
								compiler.context.i64_type(),
								"tmp_trunc",
							)?;
							let trunc_float = compiler.builder.build_signed_int_to_float(
								trunc_int,
								compiler.context.f64_type(),
								"tmp_cast",
							)?;
							trunc_float
						},
					}
				}
				FloatRvalue::Binop(binop, x, y) => {
					let x = x.compile(compiler)?;
					let y = y.compile(compiler)?;
					match binop {
						FloatBinop::Add => compiler.builder.build_float_add(x, y, "tmp_add")?,
						FloatBinop::Sub => compiler.builder.build_float_sub(x, y, "tmp_sub")?,
						FloatBinop::Mul => compiler.builder.build_float_mul(x, y, "tmp_mul")?,
						FloatBinop::Div => compiler.builder.build_float_div(x, y, "tmp_div")?,
						FloatBinop::Rem => compiler.builder.build_float_rem(x, y, "tmp_rem")?,
					}
				}
			}
		)
  }
}

impl<'src, 'ctx> Compile<'src, 'ctx> for BoolRvalueNode<'src> {
	type Output = IntValue<'ctx>;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		Ok(
			match self.inner() {
				BoolRvalue::BoolUnop(op, x) => match op {
					BoolUnop::Identity => x.compile(compiler)?,
				}
				BoolRvalue::BoolFloatBinop(op, x, y) => {
					let x = x.compile(compiler)?;
					let y = y.compile(compiler)?;
					match op {
						BoolFloatBinop::Eq => {
							let delta = compiler.builder.build_float_sub(x, y, "")?;
							compiler.builder.build_and(
								compiler.builder.build_float_compare(
									FloatPredicate::OLT,
									delta,
									compiler.context.f64_type().const_float(compiler.language_settings.epsilon.into()),
									"",
								)?,
								compiler.builder.build_float_compare(
									FloatPredicate::OGT,
									delta,
									compiler.context.f64_type().const_float((-compiler.language_settings.epsilon).into()),
									"",
								)?,
								"tmp_eq",
							)?
						}
						BoolFloatBinop::Neq => {
							let delta = compiler.builder.build_float_sub(x, y, "")?;
							compiler.builder.build_or(
								compiler.builder.build_float_compare(
									FloatPredicate::OGE,
									delta,
									compiler.context.f64_type().const_float(compiler.language_settings.epsilon.into()),
									"",
								)?,
								compiler.builder.build_float_compare(
									FloatPredicate::OLE,
									delta,
									compiler.context.f64_type().const_float((-compiler.language_settings.epsilon).into()),
									"",
								)?,
								"tmp_neq",
							)?
						}
						BoolFloatBinop::Lt => compiler.builder.build_float_compare(FloatPredicate::OLT, x, y, "tmp_lt")?,
						BoolFloatBinop::Gt => compiler.builder.build_float_compare(FloatPredicate::OGT, x, y, "tmp_gt")?,
						BoolFloatBinop::Lte => compiler.builder.build_float_compare(FloatPredicate::OLE, x, y, "tmp_lte")?,
						BoolFloatBinop::Gte => compiler.builder.build_float_compare(FloatPredicate::OGE, x, y, "tmp_gte")?,
						BoolFloatBinop::Divides => compiler.builder.build_int_compare(
							IntPredicate::EQ,
							compiler.builder.build_int_signed_rem(
								compiler.builder.build_float_to_signed_int(
									y,
									compiler.context.i64_type(),
									"",
								)?,
								compiler.builder.build_float_to_signed_int(
									x,
									compiler.context.i64_type(),
									"",
								)?,
								"",
							)?,
							compiler.context.i64_type().const_zero(),
							"tmp_divides",
						)?,
					}
				}
				BoolRvalue::BoolBoolBinop(op, x, y) => {
					let x = x.compile(compiler)?;
					let y = y.compile(compiler)?;
					match op {
						BoolBoolBinop::And => compiler.builder.build_and(x, y, "tmp_and")?,
						BoolBoolBinop::Or => compiler.builder.build_or(x, y, "tmp_or")?,
					}
				}
			}
		)
  }
}

impl<'src, 'ctx> Compile<'src, 'ctx> for ListRvalueNode<'src> {
	type Output = PointerValue<'ctx>;

	/// Returns the inner list.
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		Ok(match self.inner() {
  		ListRvalue::Lvalue(x) => match x {
  			ListLvalue::Variable(ident) => {
  				let x = compiler.variable(ident)?;
  				x.build_set_check(compiler)?.build_load_list(compiler)?
  			}
  		}
  		ListRvalue::Unknown(x) => {
				let x = x.compile(compiler)?;
				x.build_set_check(compiler)?.build_load_list(compiler)?
  		}
  		ListRvalue::Literal(x) => {
  			// create array with values
  			let array_type = compiler.context.f64_type().array_type(x.len() as u32);
  			let array_ptr = compiler.allocas_builder.build_alloca(array_type, "array")?;
  			for (i, x) in x.iter().enumerate() {
  				let x = x.compile(compiler)?;
  				let x_ptr = unsafe {
  					compiler.builder.build_in_bounds_gep(
  						compiler.context.f64_type(),
  						array_ptr,
  						&[
  							compiler.context.i64_type().const_int(i as u64, false),
  						],
  						"",
  					)?
  				};
  				compiler.builder.build_store(x_ptr, x)?;
  			}

  			// call function to create list
				let call = compiler.builder.build_call(
					compiler.external.pseudo_list_new,
					&[
						array_ptr.into(),
						compiler.context.i64_type().const_int(x.len() as u64, false).into(),
					],
					"pseudo_list_new",
				)?;
				let inner = call.as_any_value_enum().into_pointer_value();

				inner
  		}
		})
  }
}

