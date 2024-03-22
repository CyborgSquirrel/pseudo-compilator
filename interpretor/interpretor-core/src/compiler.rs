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

enum CompilerErorr {
	BuilderError(BuilderError),
}

impl From<BuilderError> for CompilerErorr {
	fn from(err: BuilderError) -> Self {
		CompilerErorr::BuilderError(err)
  }
}

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
}

impl<'a, 'from_ctx, 'ctx> Compiler<'a, 'from_ctx, 'ctx> {
	fn string_ptr_type(&self) -> inkwell::types::PointerType<'_> {
		self.context.i8_type().ptr_type(AddressSpace::default())
	}

	fn void_ptr_type(&self) -> inkwell::types::PointerType<'_> {
		self.context.i8_type().ptr_type(AddressSpace::default())
	}
	
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
					// TODO: replace with void_type() somehow
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
					// TODO: replace with void_type() somehow
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
		};

		let start_block = compiler.compile_instructions(instructions);
		compiler.builder.build_return(Some(&compiler.context.i32_type().const_int(0, false))).unwrap();

		{
			variables_builder.build_unconditional_branch(start_block).unwrap();
		}

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

		let then_block = self.context.append_basic_block(self.function, "then");
		let else_block = self.context.append_basic_block(self.function, "else");
		let merge_block = self.context.append_basic_block(self.function, "merge");
		self.builder.build_conditional_branch(cmp, then_block, else_block).unwrap();

		// blow up if set check fails
		{
			self.builder.position_at_end(then_block);
			
			let args = [
				self.builder.build_global_string_ptr("Variabila nu are nici o valoare!\n", "").unwrap().as_pointer_value().into(),
			];
			self.builder.build_call(
				self.printf_fn,
				args.as_slice(),
				"error_printf"
			).unwrap();

			self.builder.build_return(Some(&self.context.i32_type().const_int(1, false))).unwrap();
		}

		{
			self.builder.position_at_end(else_block);
			self.builder.build_unconditional_branch(merge_block).unwrap();
		}

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

	pub fn compile_instructions(&mut self, instructions: &Vec<Instructiune<'a>>) -> BasicBlock<'ctx> {
		let basic_block = self.context.append_basic_block(self.function, "instr");
		self.builder.position_at_end(basic_block);

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
								let x = self.compile_float_rvalue(x);
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
					let rvalue = self.compile_float_rvalue(rvalue);
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
					let conditie = self.compile_bool_rvalue(conditie);

					let current_block = self.builder.get_insert_block().unwrap();

					let atunci_block = self.compile_instructions(atunci);
					let altfel_block = (
						altfel.as_ref()
						.map(|instructions| self.compile_instructions(&instructions))
						.unwrap_or_else(|| self.context.append_basic_block(self.function, "instr"))
					);

					self.builder.position_at_end(current_block);
					self.builder.build_conditional_branch(conditie, atunci_block, altfel_block).unwrap();

					let merge_block = self.context.append_basic_block(self.function, "merge");
					self.builder.position_at_end(atunci_block);
					self.builder.build_unconditional_branch(merge_block).unwrap();
					self.builder.position_at_end(altfel_block);
					self.builder.build_unconditional_branch(merge_block).unwrap();
					self.builder.position_at_end(merge_block);
				}
				// Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
				// 	let start = self.compile_float_rvalue(start);
				// 	self.set_variable_value(contor, start);
				// 	self.builder.
				// }
				_ => todo!(),
			}
		}
		basic_block
	}

	pub fn compile_bool_rvalue(&mut self, bool_rvalue: &BoolRvalue<'a>) -> IntValue<'ctx> {
		match bool_rvalue {
			BoolRvalue::BoolFloatBinop(op, x, y) => match op {
				BoolFloatBinop::Eq => {
					let delta = self.builder.build_float_sub(
						self.compile_float_rvalue(x),
						self.compile_float_rvalue(y),
						"",
					).unwrap();
					self.builder.build_or(
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
						"",
					).unwrap()
				}
				BoolFloatBinop::Neq => {
					let delta = self.builder.build_float_sub(
						self.compile_float_rvalue(x),
						self.compile_float_rvalue(y),
						"",
					).unwrap();
					self.builder.build_and(
						self.builder.build_float_compare(
							FloatPredicate::OGE,
							delta,
							self.context.f64_type().const_float(EPSILON.into()),
							"",
						).unwrap(),
						self.builder.build_float_compare(
							FloatPredicate::OLE,
							delta,
							self.context.f64_type().const_float((-EPSILON).into()),
							"",
						).unwrap(),
						"",
					).unwrap()
				}
				BoolFloatBinop::Lt => self.builder.build_float_compare(
					FloatPredicate::OLT,
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"",
				).unwrap(),
				BoolFloatBinop::Gt => self.builder.build_float_compare(
					FloatPredicate::OGT,
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"",
				).unwrap(),
				BoolFloatBinop::Lte => self.builder.build_float_compare(
					FloatPredicate::OLE,
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"",
				).unwrap(),
				BoolFloatBinop::Gte => self.builder.build_float_compare(
					FloatPredicate::OGE,
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"",
				).unwrap(),
				BoolFloatBinop::Divides => todo!(),
			}
			BoolRvalue::BoolBoolBinop(op, x, y) => match op {
				BoolBoolBinop::And => self.builder.build_and(
					self.compile_bool_rvalue(x),
					self.compile_bool_rvalue(y),
					"",
				).unwrap(),
				BoolBoolBinop::Or => self.builder.build_or(
					self.compile_bool_rvalue(x),
					self.compile_bool_rvalue(y),
					"",
				).unwrap(),
			}
		}
	}
	
	pub fn compile_float_rvalue(&mut self, float_rvalue: &FloatRvalue<'a>) -> FloatValue<'ctx> {
		match float_rvalue {
			FloatRvalue::Literal(x) => self.context.f64_type().const_float(*x as f64),
			FloatRvalue::Unop(unop, x) => match unop {
				FloatUnop::Ident => self.compile_float_rvalue(x),
				FloatUnop::Neg => self.builder.build_float_neg(
					self.compile_float_rvalue(x),
					"tmpneg",
				).unwrap(),
				FloatUnop::Whole => todo!(), // this seems promising: self.builder.build_float_trunc(, , )
			}
			FloatRvalue::Binop(binop, x, y) => match binop {
				FloatBinop::Add => self.builder.build_float_add(
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"tmpadd",
				).unwrap(),
				FloatBinop::Sub => self.builder.build_float_sub(
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"tmpsub",
				).unwrap(),
				FloatBinop::Mul => self.builder.build_float_mul(
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"tmpmul",
				).unwrap(),
				FloatBinop::Div => self.builder.build_float_div(
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"tmpdiv",
				).unwrap(),
				FloatBinop::Rem => self.builder.build_float_rem(
					self.compile_float_rvalue(x),
					self.compile_float_rvalue(y),
					"tmprem",
				).unwrap(),
			}
			FloatRvalue::Lvalue(x) => {
				let value = self.get_variable_value(x);
				let value = self.builder.build_load(self.context.f64_type(), value, "").unwrap();
				value.into_float_value()
			}
		}
	}
}

