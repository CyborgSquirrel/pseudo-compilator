pub use crate::compiler::error::CompilerError;

use pseudo_sys::VariableKind;
use itertools::{Itertools, izip};

use inkwell::{values::{PointerValue, AnyValue}, FloatPredicate};

use crate::{ast::{Instructiune, ScrieParam, InstructiuneNode, AtribuireRvalue}, Compiler};

use super::{Compile, lvalue::CompileLvalue};

impl<'src, 'ctx> Compile<'src, 'ctx> for AtribuireRvalue<'src> {
	type Output = PointerValue<'ctx>;

	/// Returns the variable struct.
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError> {
    Ok(match self {
    	AtribuireRvalue::Float(x) => {
    		let inner = x.compile(compiler)?;
    		let value_ptr = compiler.build_float_struct(inner)?;
				value_ptr
    	}

    	// NOTE: Don't need to clone the list in this case, because it will always
    	// be a newly created literal, which will never accidentally get shared
    	// between multiple variables.
    	AtribuireRvalue::List(x) => {
    		let inner = x.compile(compiler)?;
    		let value_ptr = compiler.build_list_struct(inner)?;
    		value_ptr
    	}

    	AtribuireRvalue::Unknown(ident) => {
    		let x = compiler.variable(ident)?;
    		let x = x.build_set_check(compiler)?;
    		let kind = x.inner().build_load_kind(compiler)?;

    		let value_ptr = compiler.builder.build_alloca(compiler.external.variable, "value")?;

    		let float_block = compiler.context.append_basic_block(compiler.main_fn, "float");
    		let list_block = compiler.context.append_basic_block(compiler.main_fn, "list");
    		let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");

    		compiler.builder.build_switch(
    			kind,
    			merge_block,
	    		&[
    				(
		    			compiler.context.i64_type().const_int(VariableKind::Float as u64, false),
		    			float_block,
    				),
    				(
		    			compiler.context.i64_type().const_int(VariableKind::List as u64, false),
		    			list_block,
    				),
	    		],
	    	)?;

    		// float_block
    		{
    			compiler.builder.position_at_end(float_block);
    			let x_val = x.build_load(compiler)?;
    			compiler.builder.build_store(
    				value_ptr,
    				x_val,
    			)?;
	    		compiler.builder.build_unconditional_branch(merge_block)?;
    		}

    		// list_block
    		// NOTE: In case the rvalue is a list, clone it before performing
    		// assignment.
    		{
	    		compiler.builder.position_at_end(list_block);
	    		let list = x.inner().build_load_list_unchecked(compiler)?;

	    		// clone the list
					let call = compiler.builder.build_call(
						compiler.external.pseudo_list_clone,
						&[
							list.into(),
						],
						"pseudo_list_clone",
					)?;
					let list = call.as_any_value_enum().into_pointer_value();

					let cloned_list_ptr = compiler.build_list_struct(list)?;
					let cloned_list = compiler.builder.build_load(compiler.external.variable, cloned_list_ptr, "")?;
					compiler.builder.build_store(
						value_ptr,
						cloned_list,
					)?;

	    		compiler.builder.build_unconditional_branch(merge_block)?;
    		}

    		compiler.builder.position_at_end(merge_block);

    		value_ptr
    	}
    })
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
			match instruction.inner() {
				Instructiune::RepetaPanaCand(_, _) => { },
				_ => {
					compiler.builder.set_current_debug_location(compiler.get_debug_location(&instruction.span().0));
				}
			}
			
			match &instruction.inner() {
				Instructiune::Scrie(params) => {
					let mut args = Vec::new();

					// format string
					{
						let mut format = {
							params.iter()
							.map(|param| match param {
								ScrieParam::Rvalue(_) => "%f",
								ScrieParam::CharacterLiteral(_) => "%s",
								ScrieParam::StringLiteral(_) => "%s",
							})
							.join("")
						};
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
						compiler.external.printf,
						args.as_slice(),
						"scrie_printf"
					)?;
				}
				Instructiune::Citeste(lvalues) => {
					let mut args = Vec::new();

					// format string
					{
						let format = {
							lvalues.iter()
							.map(|_| "%lf")
							.join(" ")
						};

						let format = compiler.builder.build_global_string_ptr(format.as_str(), "scanf_format")?;
						let format = format.as_pointer_value();
						args.push(format.into());
					}

					let allocas: Vec<_> = (
						lvalues.iter()
						.map(|_| compiler.builder.build_alloca(compiler.context.f64_type(), ""))
						.collect::<Result<_, _>>()
					)?;
					
					for alloca in allocas.iter() {
						args.push((*alloca).into());
					}

					compiler.builder.build_call(
						compiler.external.scanf,
						args.as_slice(),
						"citeste_scanf"
					)?;

					for (lvalue, alloca) in izip!(lvalues, allocas) {
						let value = compiler.builder.build_load(compiler.context.f64_type(), alloca, "")?.into_float_value();
						let value_ptr = compiler.build_float_struct(value)?;
						let struct_ = compiler.builder.build_load(compiler.external.variable_float, value_ptr, "load_struct")?.into_struct_value();
						lvalue.compile_store(compiler, struct_)?;
					}
				}
				Instructiune::Atribuire(lvalue, rvalue) => {
					let rvalue_ptr = rvalue.compile(compiler)?;
					let rvalue = compiler.builder.build_load(compiler.external.variable, rvalue_ptr, "load_rvalue")?.into_struct_value();
					lvalue.compile_store(compiler, rvalue)?;
				}
				Instructiune::Interschimbare(x, y) => {
					let x_val = x.compile_load(compiler)?;
					let y_val = y.compile_load(compiler)?;
					x.compile_store(compiler, y_val)?;
					y.compile_store(compiler, x_val)?;
				}
				Instructiune::Insereaza(list, index, value) => {
					let index = index.compile(compiler)?;
					let value = value.compile(compiler)?;
					let list = compiler.variable(list)?.build_set_check(compiler)?.build_load_list(compiler)?;

					compiler.build_list_range_check(instruction.span(), list, index, compiler.context.f64_type().const_float(1.0))?;

					compiler.builder.build_call(
						compiler.external.pseudo_list_insert,
						&[
							list.into(),
							index.into(),
							value.into(),
						],
						"pseudo_list_insert",
					)?;
				}
				Instructiune::Sterge(list, index) => {
					let index = index.compile(compiler)?;
					let list = compiler.variable(list)?.build_set_check(compiler)?.build_load_list(compiler)?;

					compiler.build_list_range_check(instruction.span(), list, index, compiler.context.f64_type().const_zero())?;

					compiler.builder.build_call(
						compiler.external.pseudo_list_remove,
						&[
							list.into(),
							index.into(),
						],
						"pseudo_list_remove",
					)?;
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

					compiler.builder.set_current_debug_location(compiler.get_debug_location(&instruction.span().0));
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
					let start_ptr = compiler.build_float_struct(start)?;
					let struct_ = compiler.builder.build_load(compiler.external.variable_float, start_ptr, "load_struct")?.into_struct_value();
					let contor_var = compiler.variable(contor)?;
					contor_var.build_store(compiler, struct_)?;
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// conditie_block
					compiler.builder.position_at_end(conditie_block);

					let conditie = {
						let contor_value = contor_var.build_set_check(compiler)?.build_load_float(compiler)?;

						// NOTE: Yeah, this is quite gnarly, I know.
						let delta = compiler.builder.build_float_sub(
							contor_value,
							stop,
							"delta",
						)?;
						let conditie = compiler.builder.build_or(
							compiler.builder.build_or(
								compiler.builder.build_and(
									increment_is_positive,
									compiler.builder.build_float_compare(
										FloatPredicate::OLT,
										contor_value,
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
										contor_value,
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
									compiler.context.f64_type().const_float(compiler.language_settings.epsilon.into()),
									"",
								)?,
								compiler.builder.build_float_compare(
									FloatPredicate::OGT,
									delta,
									compiler.context.f64_type().const_float((-compiler.language_settings.epsilon).into()),
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
					
					let contor_value = contor_var.build_set_check(compiler)?.build_load_float(compiler)?;
					let incremented_contor = compiler.builder.build_float_add(contor_value, increment, "increment_contor")?;
					let incremented_contor_ptr = compiler.build_float_struct(incremented_contor)?;
					let struct_ = compiler.builder.build_load(compiler.external.variable_float, incremented_contor_ptr, "load_struct")?.into_struct_value();
					contor_var.build_store(compiler, struct_)?;
					compiler.builder.build_unconditional_branch(conditie_block)?;

					// merge_block
					compiler.builder.position_at_end(merge_block);
				}
			}
		}
		Ok(())
  }
}

