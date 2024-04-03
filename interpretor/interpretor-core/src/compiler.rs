use std::{collections::HashMap, path::Path, process::Command};

use itertools::{Itertools, izip};

use inkwell::{context::Context, values::{FloatValue, FunctionValue, PointerValue, IntValue}, builder::{Builder, BuilderError}, module::Module, AddressSpace, OptimizationLevel, llvm_sys::LLVMCallConv, basic_block::BasicBlock, FloatPredicate, targets::{InitializationConfig, Target, RelocMode, CodeModel, FileType}, IntPredicate, debug_info::{DebugInfoBuilder, DICompileUnit, AsDIScope, DISubprogram, DIType}};

use crate::{ast::{Instructiune, FloatRvalue, FloatUnop, FloatBinop, ScrieParam, Lvalue, BoolRvalue, BoolFloatBinop, BoolBoolBinop, InstructiuneNode, Location}, runtime::EPSILON, parse};

#[derive(Clone, Copy)]
struct Variable<'ctx> {
	value: PointerValue<'ctx>,
	is_set: PointerValue<'ctx>,
}

// errors
#[derive(Debug)]
pub enum CompilerError {
	BuilderError(BuilderError),
	VerificationError(VerificationError),
	LLVMError(inkwell::support::LLVMString),
	ClangError(ClangError),
	ParserError(parse::ParsingError),
}

impl From<BuilderError> for CompilerError {
	fn from(err: BuilderError) -> Self {
		CompilerError::BuilderError(err)
  }
}

#[derive(Debug)]
pub struct VerificationError(inkwell::support::LLVMString);
impl From<VerificationError> for CompilerError {
	fn from(err: VerificationError) -> Self {
		CompilerError::VerificationError(err)
  }
}

impl From<inkwell::support::LLVMString> for VerificationError {
	fn from(err: inkwell::support::LLVMString) -> Self {
		VerificationError(err)
  }
}

impl From<inkwell::support::LLVMString> for CompilerError {
	fn from(err: inkwell::support::LLVMString) -> Self {
		CompilerError::LLVMError(err)
  }
}

#[derive(Debug)]
pub struct ClangError {
	pub stdout: String,
	pub stderr: String,
	pub status: std::process::ExitStatus,
}

impl From<std::process::Output> for ClangError {
	fn from(value: std::process::Output) -> Self {
    ClangError {
			stdout: String::from_utf8(value.stdout).unwrap(),
			stderr: String::from_utf8(value.stderr).unwrap(),
    	status: value.status,
	  }
  }
}

impl From<ClangError> for CompilerError {
	fn from(err: ClangError) -> Self {
		CompilerError::ClangError(err)
  }
}

impl From<parse::ParsingError> for CompilerError {
	fn from(err: parse::ParsingError) -> Self {
		CompilerError::ParserError(err)
  }
}

// compilation
trait Compile<'src, 'ctx> {
	type Output;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError>;
}

impl<'src, 'ctx> Compile<'src, 'ctx> for FloatRvalue<'src> {
	type Output = FloatValue<'ctx>;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		Ok(
			match self {
				FloatRvalue::Literal(x) => compiler.context.f64_type().const_float(*x as f64),
				FloatRvalue::Unop(unop, x) => {
					let x = x.compile(compiler)?;
					match unop {
						FloatUnop::Ident => x,
						FloatUnop::Neg => compiler.builder.build_float_neg(x, "tmp_neg")?,
						FloatUnop::Whole => {
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
				
				FloatRvalue::Lvalue(x) => {
					let value = compiler.get_variable_value(x)?;
					let value = compiler.builder.build_load(compiler.context.f64_type(), value, "")?;
					value.into_float_value()
				}
			}
		)
  }
}

impl<'src, 'ctx> Compile<'src, 'ctx> for BoolRvalue<'src> {
	type Output = IntValue<'ctx>;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		Ok(
			match self {
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
									compiler.context.f64_type().const_float(EPSILON.into()),
									"",
								)?,
								compiler.builder.build_float_compare(
									FloatPredicate::OGT,
									delta,
									compiler.context.f64_type().const_float((-EPSILON).into()),
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
									compiler.context.f64_type().const_float(EPSILON.into()),
									"",
								)?,
								compiler.builder.build_float_compare(
									FloatPredicate::OLE,
									delta,
									compiler.context.f64_type().const_float((-EPSILON).into()),
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

impl<'src, 'ctx> Compile<'src, 'ctx> for [InstructiuneNode<'src>] {
	type Output = ();
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
		for instruction in self.iter() {
			// NOTE: Set the debug location to the current instruction's location, except
			// for RepetaPanaCand. We're excluding RepetaPanaCand, because its location
			// points to the "pana cand <condition>" part, which shows up after all its
			// instructions.
			match &instruction.inner {
				Instructiune::RepetaPanaCand(_, _) => { },
				_ => {
					compiler.builder.set_current_debug_location(compiler.get_debug_location(&instruction.location));
				}
			}
			
			match &instruction.inner {
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
						
						let format = compiler.builder.build_global_string_ptr(format.as_str(), "printf_format")?;
						let format = format.as_pointer_value();
						args.push(format.into());
					}
					
					// add params to args
					for param in params {
						match param {
							ScrieParam::Rvalue(x) => {
								let x = x.compile(compiler)?;
								args.push(x.into());
							}
							ScrieParam::CharacterLiteral(chr) => {
								let chr = compiler.builder.build_global_string_ptr(chr, "")?.as_pointer_value();
								args.push(chr.into());
							}
							ScrieParam::StringLiteral(string) => {
								let string = compiler.builder.build_global_string_ptr(string, "")?.as_pointer_value();
								args.push(string.into());
							}
						}
					}

					compiler.builder.build_call(
						compiler.printf_fn,
						args.as_slice(),
						"scrie_printf"
					)?;
				}
				Instructiune::Atribuire(lvalue, rvalue) => {
					let rvalue = rvalue.compile(compiler)?;
					compiler.set_variable_value(lvalue, rvalue)?;
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

						let format = compiler.builder.build_global_string_ptr(format.as_str(), "scanf_format")?;
						let format = format.as_pointer_value();
						args.push(format.into());
					}

					let allocas: Vec<_> = (
						lvalues.iter()
						.map(|lvalue| compiler.builder.build_alloca(compiler.context.f64_type(), lvalue.0))
						.collect::<Result<_, _>>()
					)?;
					
					for alloca in allocas.iter() {
						args.push((*alloca).into());
					}

					compiler.builder.build_call(
						compiler.scanf_fn,
						args.as_slice(),
						"citeste_scanf"
					)?;

					for (lvalue, alloca) in izip!(lvalues, allocas) {
						let value = compiler.builder.build_load(compiler.context.f64_type(), alloca, "")?.into_float_value();
						compiler.set_variable_value(lvalue, value)?;
					}
				}
				Instructiune::Interschimbare(x, y) => {
					let x_ptr_value = compiler.get_variable_value(x)?;
					let x_value = compiler.builder.build_load(compiler.context.f64_type(), x_ptr_value, "x_value")?;

					let y_ptr_value = compiler.get_variable_value(y)?;
					let y_value = compiler.builder.build_load(compiler.context.f64_type(), y_ptr_value, "y_value")?;

					compiler.set_variable_value(y, x_value.into_float_value())?;
					compiler.set_variable_value(x, y_value.into_float_value())?;
				}
				Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
					let atunci_block = compiler.context.append_basic_block(compiler.main_fn, "atunci");
					let altfel_block = compiler.context.append_basic_block(compiler.main_fn, "altfel");
					let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");

					// current block
					let conditie = conditie.compile(compiler)?;
					compiler.builder.build_conditional_branch(conditie, atunci_block, altfel_block)?;

					// atunci_block
					compiler.builder.position_at_end(atunci_block);
					atunci.compile(compiler)?;
					compiler.builder.build_unconditional_branch(merge_block)?;

					// altfel_block
					compiler.builder.position_at_end(altfel_block);
					if let Some(altfel) = altfel {
						altfel.compile(compiler)?;
					}
					compiler.builder.build_unconditional_branch(merge_block)?;

					// merge_block
					compiler.builder.position_at_end(merge_block);
				}
				Instructiune::CatTimpExecuta(conditie, executa) => {
					let conditie_block = compiler.context.append_basic_block(compiler.main_fn, "conditie");
					let executa_block = compiler.context.append_basic_block(compiler.main_fn, "executa");
					let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");

					// current block
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// conditie_block
					compiler.builder.position_at_end(conditie_block);
					let conditie = conditie.compile(compiler)?;
					compiler.builder.build_conditional_branch(
						conditie,
						executa_block,
						merge_block,
					)?;

					// executa_block
					compiler.builder.position_at_end(executa_block);
					executa.compile(compiler)?;
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// merge_block
					compiler.builder.position_at_end(merge_block);
				}
				Instructiune::RepetaPanaCand(repeta, conditie) => {
					let repeta_block = compiler.context.append_basic_block(compiler.main_fn, "repeta");
					let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");

					// current block
					compiler.builder.build_unconditional_branch(repeta_block)?;

					// repeta_block
					compiler.builder.position_at_end(repeta_block);
					repeta.compile(compiler)?;

					compiler.builder.set_current_debug_location(compiler.get_debug_location(&instruction.location));
					let conditie = conditie.compile(compiler)?;
					compiler.builder.build_conditional_branch(
						conditie,
						merge_block,
						repeta_block,
					)?;

					// merge_block
					compiler.builder.position_at_end(merge_block);
				}
				Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
					let conditie_block = compiler.context.append_basic_block(compiler.main_fn, "conditie");
					let executa_block = compiler.context.append_basic_block(compiler.main_fn, "executa");
					let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");

					// current block
					let start = start.compile(compiler)?;
					let stop = stop.compile(compiler)?;
					let increment = if let Some(increment) = increment {
						increment.compile(compiler)?
					} else {
						compiler.context.f64_type().const_float(1.0)
					};
					let increment_is_positive = compiler.builder.build_float_compare(
						FloatPredicate::OGE,
						increment,
						compiler.context.f64_type().const_zero(),
						"increment_is_positive",
					)?;
					compiler.set_variable_value(contor, start)?;
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// conditie_block
					compiler.builder.position_at_end(conditie_block);

					let conditie = {
						let contor_ptr = compiler.get_variable_value(contor)?;
						let contor = compiler.builder.build_load(compiler.context.f64_type(), contor_ptr, "contor")?.into_float_value();

						// NOTE: Yeah, this is quite gnarly, I know.
						let delta = compiler.builder.build_float_sub(
							contor,
							stop,
							"delta",
						)?;
						let conditie = compiler.builder.build_or(
							compiler.builder.build_or(
								compiler.builder.build_and(
									increment_is_positive,
									compiler.builder.build_float_compare(
										FloatPredicate::OLT,
										contor,
										stop,
										"stop_lt",
									)?,
									"stop_pozitiv",
								)?,
								compiler.builder.build_and(
									compiler.builder.build_not(
										increment_is_positive,
										"increment_is_negative",
									)?,
									compiler.builder.build_float_compare(
										FloatPredicate::OGT,
										contor,
										stop,
										"stop_gt",
									)?,
									"stop_negativ",
								)?,
								"stop",
							)?,
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
								"egal_contor",
							)?,
							"pentru_conditie",
						)?;
						conditie
					};

					compiler.builder.build_conditional_branch(conditie, executa_block, merge_block)?;

					// executa_block
					compiler.builder.position_at_end(executa_block);
					executa.compile(compiler)?;
					
					let contor_ptr = compiler.get_variable_value(contor)?;
					let contor_value = compiler.builder.build_load(compiler.context.f64_type(), contor_ptr, "contor")?.into_float_value();
					let incremented_contor = compiler.builder.build_float_add(contor_value, increment, "")?;
					compiler.set_variable_value(contor, incremented_contor)?;
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// merge_block
					compiler.builder.position_at_end(merge_block);
				}
			}
		}
		Ok(())
  }
}

pub struct Compiler<'src, 'ctx> {
	context: &'ctx Context,
	module: Module<'ctx>,

	builder: Builder<'ctx>,

	variables_builder: Builder<'ctx>,
	variables: HashMap<&'src str, Variable<'ctx>>,
	
	main_fn: FunctionValue<'ctx>,

	printf_fn: FunctionValue<'ctx>,
	scanf_fn: FunctionValue<'ctx>,
	exit_fn: FunctionValue<'ctx>,

	fail_block: BasicBlock<'ctx>,

	debug_info_builder: DebugInfoBuilder<'ctx>,
	debug_compile_unit: DICompileUnit<'ctx>,
	debug_main_function: DISubprogram<'ctx>,
	debug_type: DIType<'ctx>,
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	pub fn compile<P: AsRef<Path>>(
		context: &'ctx Context,
		code: &'src str,
		path: P,
	) -> Result<Self, CompilerError> {
		Ok({
			let path = path.as_ref();

			let module = context.create_module("main");

			// debug boilerplate
			let debug_metadata_version = context.i32_type().const_int(3, false);
			module.add_basic_value_flag(
		    "Debug Info Version",
		    inkwell::module::FlagBehavior::Warning,
		    debug_metadata_version,
			);

			let (debug_info_builder, debug_compile_unit) = module.create_debug_info_builder(
				false,
				inkwell::debug_info::DWARFSourceLanguage::C,
				path.file_name().unwrap().to_str().unwrap(),
				path.parent().unwrap().to_str().unwrap(),
				"pseudo-compiler",
				false, // TODO: actually reflect whether it's optimized or not
				"",
				0,
				"",
				inkwell::debug_info::DWARFEmissionKind::Full,
				0,
				false,
				false,
				"",
				"",
			);

			let debug_main_function_type = debug_info_builder.create_subroutine_type(
				debug_compile_unit.get_file(),
				None,
				&[],
				0,
			);

			let debug_main_function = debug_info_builder.create_function(
				debug_compile_unit.as_debug_info_scope(),
				"main",
				None,
				debug_compile_unit.get_file(),
				0,
				debug_main_function_type,
				false,
				true,
				0,
				0,
				false,
			);

			// external libc functions
			let printf_fn = module.add_function(
				"printf",
				context.i64_type().fn_type(
					&[
						context.i8_type().ptr_type(AddressSpace::default()).into(),
					],
					true,
				),
				None,
			);
			printf_fn.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

			let scanf_fn = module.add_function(
				"scanf",
				context.i64_type().fn_type(
					&[
						context.i8_type().ptr_type(AddressSpace::default()).into(),
					],
					true,
				),
				None,
			);
			scanf_fn.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

			let exit_fn = module.add_function(
				"exit",
				context.void_type().fn_type(
					&[
						context.i64_type().into(),
					],
					false,
				),
				None,
			);
			exit_fn.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

			// main_fn
			let main_fn = module.add_function("main", context.i64_type().fn_type(&[], false), None);
			main_fn.set_subprogram(debug_main_function);

			// variables
			let variables_builder = context.create_builder();
			let variables_block = context.append_basic_block(main_fn, "variables");
			variables_builder.position_at_end(variables_block);

			let builder = context.create_builder();
			let fail_block = {
				let fail_block = context.append_basic_block(main_fn, "fail");
				builder.position_at_end(fail_block);
			
				let args = [
					builder.build_global_string_ptr("Variabila nu are nici o valoare!\n", "")?.as_pointer_value().into(),
				];
				builder.build_call(
					printf_fn,
					args.as_slice(),
					"error_printf"
				)?;

				builder.build_call(exit_fn, &[context.i64_type().const_int(1, false).into()], "")?;
				builder.build_return(Some(&context.i64_type().const_int(1, false)))?;
				fail_block
			};

			let debug_type = debug_info_builder.create_basic_type(
				"f64",
				64,
				0x04, // float type, source: https://dwarfstd.org/doc/DWARF5.pdf#section.7.8
				0,
			).unwrap().as_type();

			let mut compiler = Self {
				context,
				module,

				builder,

				variables_builder,
				variables: HashMap::new(),
	
				main_fn,

				printf_fn,
				scanf_fn,
				exit_fn,

				fail_block,

				debug_info_builder,
				debug_compile_unit,
				debug_main_function,
				debug_type,
			};

			let program = parse::parse(code)?;
			{
				let location = Location::new(0, 0);
				compiler.variables_builder.set_current_debug_location(compiler.get_debug_location(&location));
			}
			compiler.compile_parsed(&program)?;

			compiler
		})
	}

	fn compile_parsed(
		&mut self,
		instructions: &Vec<InstructiuneNode<'src>>,
	) -> Result<(), CompilerError> {
		let start_block = self.context.append_basic_block(self.main_fn, "start");

		self.builder.position_at_end(start_block);
		instructions.compile(self)?;
		self.builder.build_call(self.exit_fn, &[self.context.i64_type().const_int(0, false).into()], "")?;
		self.builder.build_return(Some(&self.context.i64_type().const_int(0, false)))?;
		self.variables_builder.build_unconditional_branch(start_block)?;

		self.debug_info_builder.finalize();
		self.module.verify().map_err(VerificationError::from)?;

		Ok(())
	}

	pub fn write_object<P: AsRef<Path>>(
		&self,
		path: P,
		optimization_level: OptimizationLevel,
	) -> Result<(), CompilerError> {
		let path = path.as_ref();
		
		let target_triple = inkwell::targets::TargetMachine::get_default_triple();
		Target::initialize_all(&InitializationConfig::default());
		let target = Target::from_triple(&target_triple)?;
		let target_machine = target.create_target_machine(
			&target_triple,
			"generic",
			"",
			optimization_level,
			RelocMode::PIC,
			CodeModel::Default,
		).unwrap();
		target_machine.write_to_file(&self.module, FileType::Object, path)?;

		Ok(())
	}

	pub fn write_executable<P: AsRef<Path>>(
		&self,
		path: P,
		optimization_level: OptimizationLevel,
	) -> Result<(), CompilerError> {
		let path = path.as_ref();

		let object_path = tempfile::NamedTempFile::new().unwrap().into_temp_path();
		let object_path = &object_path;
		self.write_object(object_path, optimization_level)?;

		let output = Command::new("clang")
			.arg("-lm")
			.arg(object_path)
			.arg("-o").arg(path)
			.output().unwrap();
		if !output.status.success() {
			return Err(CompilerError::from(ClangError::from(output)));
		}

		Ok(())
	}

	pub fn write_llvm_ir<P: AsRef<Path>>(
		&self,
		path: P,
	) -> Result<(), CompilerError> {
		self.module.print_to_file(path)?;
		Ok(())
	}
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	fn get_debug_location(&self, location: &Location) -> inkwell::debug_info::DILocation<'ctx> {
		let debug_location = self.debug_info_builder.create_debug_location(
			self.context,
			location.line(),
			location.column(),
			self.debug_main_function.as_debug_info_scope(),
			None,
		);
		debug_location
	}
	
	fn get_variable(&mut self, lvalue: &Lvalue<'src>) -> Result<Variable<'ctx>, CompilerError> {
		Ok({
			let key = lvalue.0;
			if !self.variables.contains_key(key) {
				let value = {
					let mut name = String::new();
					name.push_str("value_");
					name.push_str(key);
					self.variables_builder.build_alloca(self.context.f64_type(), name.as_str())?
				};

				let is_set = {
					let mut name = String::new();
					name.push_str("is_set_");
					name.push_str(key);
					self.variables_builder.build_alloca(self.context.i64_type(), name.as_str())?
				};

				let debug_variable = self.debug_info_builder.create_auto_variable(
					self.debug_main_function.as_debug_info_scope(),
					key,
					self.debug_compile_unit.get_file(),
					0,
					self.debug_type,
					true,
					0,
					0,
				);
				self.debug_info_builder.insert_declare_at_end(
					value,
					Some(debug_variable),
					None,
					self.get_debug_location(&Location::new(0, 0)),
					self.variables_builder.get_insert_block().unwrap(),
				);

				let value = Variable {
					value,
					is_set,
				};

				self.variables.insert(key, value);
			}			
			self.variables[key]
		})
	}

	fn get_variable_value(
		&mut self,
		lvalue: &Lvalue<'src>,
	) -> Result<PointerValue<'ctx>, CompilerError> {
		Ok({
			let variable = self.get_variable(lvalue)?;

			// TODO: Would be nice to dedup these checks.
			let is_set = self.builder.build_load(
				self.context.i64_type(),
				variable.is_set,
				"load_is_set",
			)?;

			let cmp = self.builder.build_int_compare(
				IntPredicate::EQ,
				is_set.into_int_value(),
				self.context.i64_type().const_zero(),
				"check_is_set",
			)?;

			let merge_block = self.context.append_basic_block(self.main_fn, "merge");
			self.builder.build_conditional_branch(cmp, self.fail_block, merge_block)?;

			self.builder.position_at_end(merge_block);
			variable.value
		})
	}

	fn set_variable_value(&mut self, lvalue: &Lvalue<'src>, rvalue: FloatValue<'ctx>) -> Result<(), CompilerError> {
		let variable = self.get_variable(lvalue)?;
		self.builder.build_store(
			variable.value,
			rvalue,
		)?;
		self.builder.build_store(
			variable.is_set,
			self.context.i64_type().const_int(1, false),
		)?;
		Ok(())
	}
}

