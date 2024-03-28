use std::collections::{HashSet, HashMap};

use itertools::{intersperse, Itertools, izip};

use inkwell::{context::{Context, self}, values::{FloatValue, FunctionValue, BasicValue, PointerValue, self, IntValue}, builder::{Builder, BuilderError}, module::Module, AddressSpace, types::AnyType, OptimizationLevel, llvm_sys::LLVMCallConv, basic_block::BasicBlock, FloatPredicate};

use crate::{syntax::{Instructiune, FloatRvalue, FloatUnop, FloatBinop, ScrieParam, Lvalue, BoolRvalue, BoolFloatBinop, BoolBoolBinop}, runtime::EPSILON};

pub fn compile<'a>(
	instructions: &Vec<Instructiune<'a>>,
) {
	let context = Context::create();
	let builder = context.create_builder();
	let variables_builder = context.create_builder();
	let module = context.create_module("main");
	let main_fn = Compiler::compile(&context, &builder, &variables_builder, &module, instructions);
	main_fn.verify(true);

	let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
	unsafe {
		engine.run_function(main_fn, &[]);
	}
}

#[derive(Clone, Copy)]
struct Variable<'ctx> {
	value: PointerValue<'ctx>,
	is_set: PointerValue<'ctx>,
}

#[derive(Debug)]
enum CompilerErorr {
	BuilderError(BuilderError),
}

impl From<BuilderError> for CompilerErorr {
	fn from(err: BuilderError) -> Self {
		CompilerErorr::BuilderError(err)
  }
}

trait Compile<'a, 'from_ctx, 'ctx> {
	type Output;
	fn compile(&self, compiler: &mut Compiler<'a, 'from_ctx, 'ctx>) -> Result<Self::Output, CompilerErorr>;
}

impl<'a, 'from_ctx, 'ctx> Compile<'a, 'from_ctx, 'ctx> for FloatRvalue<'a> {
	type Output = FloatValue<'ctx>;
	fn compile(&self, compiler: &mut Compiler<'a, 'from_ctx, 'ctx>) -> Result<Self::Output, CompilerErorr> {
		Ok(
			match self {
				FloatRvalue::Literal(x) => compiler.context.f64_type().const_float(*x as f64),
				FloatRvalue::Unop(unop, x) => match unop {
					FloatUnop::Ident => x.compile(compiler)?,
					FloatUnop::Neg => compiler.builder.build_float_neg(
						x.compile(compiler)?,
						"tmpneg",
					)?,
					FloatUnop::Whole => todo!(), // this seems promising: self.builder.build_float_trunc(, , )
				}
				FloatRvalue::Binop(binop, x, y) => match binop {
					FloatBinop::Add => compiler.builder.build_float_add(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"tmpadd",
					).unwrap(),
					FloatBinop::Sub => compiler.builder.build_float_sub(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"tmpsub",
					).unwrap(),
					FloatBinop::Mul => compiler.builder.build_float_mul(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"tmpmul",
					).unwrap(),
					FloatBinop::Div => compiler.builder.build_float_div(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"tmpdiv",
					).unwrap(),
					FloatBinop::Rem => compiler.builder.build_float_rem(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"tmprem",
					).unwrap(),
				}
				FloatRvalue::Lvalue(x) => {
					let value = compiler.get_variable_value(x);
					let value = compiler.builder.build_load(compiler.context.f64_type(), value, "")?;
					value.into_float_value()
				}
			}
		)
  }
}

impl<'a, 'from_ctx, 'ctx> Compile<'a, 'from_ctx, 'ctx> for BoolRvalue<'a> {
	type Output = IntValue<'ctx>;
	fn compile(&self, compiler: &mut Compiler<'a, 'from_ctx, 'ctx>) -> Result<Self::Output, CompilerErorr> {
		Ok(
			match self {
				BoolRvalue::BoolFloatBinop(op, x, y) => match op {
					BoolFloatBinop::Eq => {
						let delta = compiler.builder.build_float_sub(
							x.compile(compiler)?,
							y.compile(compiler)?,
							"",
						)?;
						compiler.builder.build_and(
							compiler.builder.build_float_compare(
								FloatPredicate::OLT,
								delta,
								compiler.context.f64_type().const_float(EPSILON.into()),
								"",
							)?,
							compiler.builder.build_float_compare(
								FloatPredicate::OGT,
								delta,
								compiler.context.f64_type().const_float((-EPSILON).into()),
								"",
							)?,
							"",
						)?
					}
					BoolFloatBinop::Neq => {
						let delta = compiler.builder.build_float_sub(
							x.compile(compiler)?,
							y.compile(compiler)?,
							"",
						)?;
						compiler.builder.build_and(
							compiler.builder.build_float_compare(
								FloatPredicate::OGE,
								delta,
								compiler.context.f64_type().const_float(EPSILON.into()),
								"",
							)?,
							compiler.builder.build_float_compare(
								FloatPredicate::OLE,
								delta,
								compiler.context.f64_type().const_float((-EPSILON).into()),
								"",
							)?,
							"",
						)?
					}
					BoolFloatBinop::Lt => compiler.builder.build_float_compare(
						FloatPredicate::OLT,
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
					BoolFloatBinop::Gt => compiler.builder.build_float_compare(
						FloatPredicate::OGT,
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
					BoolFloatBinop::Lte => compiler.builder.build_float_compare(
						FloatPredicate::OLE,
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
					BoolFloatBinop::Gte => compiler.builder.build_float_compare(
						FloatPredicate::OGE,
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
					BoolFloatBinop::Divides => todo!(),
				}
				BoolRvalue::BoolBoolBinop(op, x, y) => match op {
					BoolBoolBinop::And => compiler.builder.build_and(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
					BoolBoolBinop::Or => compiler.builder.build_or(
						x.compile(compiler)?,
						y.compile(compiler)?,
						"",
					)?,
				}
			}
		)
  }
}

// impl<'a, 'from_ctx, 'ctx> Compile<'a, 'from_ctx, 'ctx> for [Instructiune<'a>] {
// 	type Output = ();
// 	fn compile(&self, compiler: &mut Compiler<'a, 'from_ctx, 'ctx>) -> Result<Self::Output, CompilerErorr> {
      
//   }
// }

pub struct Compiler<'a, 'from_ctx, 'ctx> {
	pub context: &'ctx Context,
	pub builder: &'from_ctx Builder<'ctx>,
	pub module: &'from_ctx Module<'ctx>,
	pub function: FunctionValue<'ctx>,

	variables_builder: &'from_ctx Builder<'ctx>,
	variables_block: BasicBlock<'ctx>,

	variables: HashMap<&'a str, Variable<'ctx>>,
	
	printf_fn: FunctionValue<'ctx>,
	scanf_fn: FunctionValue<'ctx>,

	fail_block: BasicBlock<'ctx>,
}

impl<'a, 'from_ctx, 'ctx> Compiler<'a, 'from_ctx, 'ctx> {
	pub fn compile(
		context: &'ctx Context,
		builder: &'from_ctx Builder<'ctx>,
		variables_builder: &'from_ctx Builder<'ctx>,
		module: &'from_ctx Module<'ctx>,
		instructions: &Vec<Instructiune<'a>>,
	) -> FunctionValue<'ctx> {
		let printf_fn = module.add_function(
			"printf",
			context.i32_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into()
				],
				true,
			),
			None,
		);
		printf_fn.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		let scanf_fn = module.add_function(
			"scanf",
			context.i32_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into()
				],
				true,
			),
			None,
		);
		scanf_fn.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		let function = module.add_function("main", context.i32_type().fn_type(&[], false), None);
		let variables_block = context.append_basic_block(function, "variables");
		variables_builder.position_at_end(variables_block);

		let fail_block = {
			let fail_block = context.append_basic_block(function, "fail");
			builder.position_at_end(fail_block);
			
			let args = [
				builder.build_global_string_ptr("Variabila nu are nici o valoare!\n", "").unwrap().as_pointer_value().into(),
			];
			builder.build_call(
				printf_fn,
				args.as_slice(),
				"error_printf"
			).unwrap();

			builder.build_return(Some(&context.i32_type().const_int(1, false))).unwrap();
			fail_block
		};

		let mut compiler = Self {
			context,
			builder,
			module,
			function,

			variables_builder,
			variables_block,

			variables: HashMap::new(),

			printf_fn,
			scanf_fn,

			fail_block,
		};

		let start_block = compiler.context.append_basic_block(compiler.function, "start");

		compiler.builder.position_at_end(start_block);
		compiler.compile_instructions(instructions);
		compiler.builder.build_return(Some(&compiler.context.i32_type().const_int(0, false))).unwrap();

		variables_builder.build_unconditional_branch(start_block).unwrap();

		compiler.module.print_to_stderr();

		compiler.function
	}

	// variable
	// - value
	// - is_assigned

	// in the beginning, is_assigned is 0

	// when assigning variable
	// - set is_assigned to 1

	// when accessing variable
	// - check if is_assigned is 1, otherwise blow up

	/*

	self.get_variable_unchecked("name")
	self.get_variable("name")
	self.set_variable("name")

	*/

	fn get_variable(&mut self, lvalue: &Lvalue<'a>) -> Variable<'ctx> {
		*self.variables.entry(lvalue.0).or_insert_with_key(|key| {
			let value = {
				let mut name = String::new();
				name.push_str("value_");
				name.push_str(key);
				self.variables_builder.build_alloca(self.context.f64_type(), name.as_str()).unwrap()
			};

			let is_set = {
				let mut name = String::new();
				name.push_str("is_set_");
				name.push_str(key);
				self.variables_builder.build_alloca(self.context.i64_type(), name.as_str()).unwrap()
			};

			Variable {
				value,
				is_set,
			}
		})
	}

	fn get_variable_value(
		&mut self,
		lvalue: &Lvalue<'a>,
	) -> PointerValue<'ctx> {
		let variable = self.get_variable(lvalue);

		let is_set = self.builder.build_load(
			self.context.i64_type(),
			variable.is_set,
			"load_is_set",
		).unwrap();

		let cmp = self.builder.build_int_compare(
			inkwell::IntPredicate::EQ,
			is_set.into_int_value(),
			self.context.i64_type().const_zero(),
			"check_is_set",
		).unwrap();

		let merge_block = self.context.append_basic_block(self.function, "merge");
		self.builder.build_conditional_branch(cmp, self.fail_block, merge_block).unwrap();

		self.builder.position_at_end(merge_block);
		variable.value
	}

	fn set_variable_value(&mut self, lvalue: &Lvalue<'a>, rvalue: FloatValue<'ctx>) {
		let variable = self.get_variable(lvalue);
		self.builder.build_store(
			variable.value,
			rvalue,
		).unwrap();
		self.builder.build_store(
			variable.is_set,
			self.context.i64_type().const_int(1, false),
		).unwrap();
	}

	pub fn compile_instructions(&mut self, instructions: &Vec<Instructiune<'a>>) {
		for instruction in instructions {
			match instruction {
				Instructiune::Scrie(params) => {
					let mut args = Vec::new();

					// format string
					{
						let mut format = (
							params.iter()
							.map(|param| match param {
								ScrieParam::Rvalue(_) => "%f",
								ScrieParam::CharacterLiteral(_) => "%s",
								ScrieParam::StringLiteral(_) => "%s",
							})
							.join("")
						);
						format.push_str("\n");
						
						let format = self.builder.build_global_string_ptr(format.as_str(), "printf_format").unwrap();
						let format = format.as_pointer_value();
						args.push(format.into());
					}
					
					// add params to args
					for param in params {
						match param {
							ScrieParam::Rvalue(x) => {
								let x = x.compile(self).unwrap();
								args.push(x.into());
							}
							ScrieParam::CharacterLiteral(chr) => {
								let chr = self.builder.build_global_string_ptr(chr, "").unwrap().as_pointer_value();
								args.push(chr.into());
							}
							ScrieParam::StringLiteral(string) => {
								let string = self.builder.build_global_string_ptr(string, "").unwrap().as_pointer_value();
								args.push(string.into());
							}
						}
					}

					self.builder.build_call(
						self.printf_fn,
						args.as_slice(),
						"scrie_printf"
					).unwrap();
				}
				Instructiune::Atribuire(lvalue, rvalue) => {
					let rvalue = rvalue.compile(self).unwrap();
					self.set_variable_value(lvalue, rvalue);
				}
				Instructiune::Citeste(lvalues) => {
					let mut args = Vec::new();

					// format string
					{
						let format = (
							lvalues.iter()
							.map(|_| "%lf")
							.join(" ")
						);

						let format = self.builder.build_global_string_ptr(format.as_str(), "scanf_format").unwrap();
						let format = format.as_pointer_value();
						args.push(format.into());
					}

					let allocas: Vec<_> = (
						lvalues.iter()
						.map(|lvalue| self.builder.build_alloca(self.context.f64_type(), lvalue.0).unwrap())
						.collect()
					);
					
					for alloca in allocas.iter() {
						args.push((*alloca).into());
					}

					self.builder.build_call(
						self.scanf_fn,
						args.as_slice(),
						"citeste_scanf"
					).unwrap();

					for (lvalue, alloca) in izip!(lvalues, allocas) {
						let value = self.builder.build_load(self.context.f64_type(), alloca, "").unwrap().into_float_value();
						self.set_variable_value(lvalue, value);
					}
				}
				Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
					let conditie = conditie.compile(self).unwrap();

					let atunci_block = self.context.append_basic_block(self.function, "atunci");
					let altfel_block = self.context.append_basic_block(self.function, "altfel");
					self.builder.build_conditional_branch(conditie, atunci_block, altfel_block).unwrap();

					self.builder.position_at_end(atunci_block);
					self.compile_instructions(atunci);
					let atunci_end = self.builder.get_insert_block().unwrap();

					self.builder.position_at_end(altfel_block);
					if let Some(altfel) = altfel {
						self.compile_instructions(altfel);
					}
					let altfel_end = self.builder.get_insert_block().unwrap();

					let merge_block = self.context.append_basic_block(self.function, "merge");
					self.builder.position_at_end(atunci_end);
					self.builder.build_unconditional_branch(merge_block).unwrap();
					self.builder.position_at_end(altfel_end);
					self.builder.build_unconditional_branch(merge_block).unwrap();
					self.builder.position_at_end(merge_block);
				}
				Instructiune::Interschimbare(x, y) => {
					let x_ptr_value = self.get_variable_value(x);
					let x_value = self.builder.build_load(self.context.f64_type(), x_ptr_value, "x_value").unwrap();

					let y_ptr_value = self.get_variable_value(y);
					let y_value = self.builder.build_load(self.context.f64_type(), y_ptr_value, "y_value").unwrap();

					self.set_variable_value(y, x_value.into_float_value());
					self.set_variable_value(x, y_value.into_float_value());
				}
				Instructiune::CatTimpExecuta(conditie, executa) => {
					let conditie_block = self.context.append_basic_block(self.function, "conditie");
					let executa_block = self.context.append_basic_block(self.function, "executa");
					let merge_block = self.context.append_basic_block(self.function, "merge");
					self.builder.build_unconditional_branch(conditie_block).unwrap();

					self.builder.position_at_end(conditie_block);
					let conditie = conditie.compile(self).unwrap();
					self.builder.build_conditional_branch(
						conditie,
						executa_block,
						merge_block,
					).unwrap();

					self.builder.position_at_end(executa_block);
					self.compile_instructions(executa);
					self.builder.build_unconditional_branch(conditie_block).unwrap();

					self.builder.position_at_end(merge_block);
				}
				Instructiune::RepetaPanaCand(repeta, conditie) => {
					let repeta_block = self.context.append_basic_block(self.function, "repeta");
					let merge_block = self.context.append_basic_block(self.function, "merge");

					self.builder.build_unconditional_branch(repeta_block).unwrap();
					self.builder.position_at_end(repeta_block);
					self.compile_instructions(repeta);
					let conditie = conditie.compile(self).unwrap();
					self.builder.build_conditional_branch(
						conditie,
						merge_block,
						repeta_block,
					).unwrap();

					self.builder.position_at_end(merge_block);
				}
				Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
					let cond_block = self.context.append_basic_block(self.function, "cond");
					let executa_block = self.context.append_basic_block(self.function, "executa");
					let merge_block = self.context.append_basic_block(self.function, "merge");

					let start = start.compile(self).unwrap();
					let stop = stop.compile(self).unwrap();
					let increment = if let Some(increment) = increment {
						increment.compile(self).unwrap()
					} else {
						self.context.f64_type().const_float(1.0)
					};
					let increment_is_positive = self.builder.build_float_compare(
						FloatPredicate::OGT,
						increment,
						self.context.f64_type().const_zero(),
						"increment_is_positive",
					).unwrap();
					self.set_variable_value(contor, start);

					self.builder.build_unconditional_branch(cond_block).unwrap();

					// cond block
					self.builder.position_at_end(cond_block);

					let conditie = {
						let contor_ptr = self.get_variable_value(contor);
						let contor = self.builder.build_load(self.context.f64_type(), contor_ptr, "contor").unwrap().into_float_value();

						// NOTE: Yeah, this is quite gnarly, I know.
						let delta = self.builder.build_float_sub(
							contor,
							stop,
							"delta",
						).unwrap();
						let conditie = self.builder.build_or(
							self.builder.build_or(
								self.builder.build_and(
									increment_is_positive,
									self.builder.build_float_compare(
										FloatPredicate::OLT,
										contor,
										stop,
										"stop_lt",
									).unwrap(),
									"stop_pozitiv",
								).unwrap(),
								self.builder.build_and(
									self.builder.build_not(
										increment_is_positive,
										"",
									).unwrap(),
									self.builder.build_float_compare(
										FloatPredicate::OGT,
										contor,
										stop,
										"stop_gt",
									).unwrap(),
									"stop_negativ",
								).unwrap(),
								"stop",
							).unwrap(),
							self.builder.build_and(
								self.builder.build_float_compare(
									FloatPredicate::OLT,
									delta,
									self.context.f64_type().const_float(EPSILON.into()),
									"",
								).unwrap(),
								self.builder.build_float_compare(
									FloatPredicate::OGT,
									delta,
									self.context.f64_type().const_float((-EPSILON).into()),
									"",
								).unwrap(),
								"egal_contor",
							).unwrap(),
							"pentru_conditie",
						).unwrap();
						conditie
					};

					self.builder.build_conditional_branch(conditie, executa_block, merge_block).unwrap();

					// executa block
					self.builder.position_at_end(executa_block);
					self.compile_instructions(executa);
					
					let contor_ptr = self.get_variable_value(contor);
					let contor_value = self.builder.build_load(self.context.f64_type(), contor_ptr, "contor").unwrap().into_float_value();
					let new_contor = self.builder.build_float_add(contor_value, increment, "").unwrap();
					self.set_variable_value(contor, new_contor);
					self.builder.build_unconditional_branch(cond_block).unwrap();

					// merge block
					self.builder.position_at_end(merge_block);
				}
			}
		}
	}
}

