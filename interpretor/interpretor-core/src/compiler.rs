trace::init_depth_var!();

pub use crate::compiler::error::CompilerError;
use std::{collections::HashMap, path::Path};

use interpretor_sys::VariableKind;

use inkwell::{context::Context, values::{FloatValue, FunctionValue, PointerValue, AnyValue}, builder::Builder, module::Module, basic_block::BasicBlock, debug_info::{DebugInfoBuilder, DICompileUnit, AsDIScope, DISubprogram, DIType}, FloatPredicate};

use crate::{ast::{Ident, InstructiuneNode, Location, FloatRvalue}, parse};

use self::{other::External, error::{VerificationError, CompilerResult}, variable::Variable};

mod other;
mod expression;
mod error;
mod output;
mod variable;
mod lvalue;
mod instruction;

trait Compile<'src, 'ctx> {
	type Output;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError>;
}

#[derive(Debug)]
pub struct Compiler<'src, 'ctx> {
	context: &'ctx Context,
	module: Module<'ctx>,

	builder: Builder<'ctx>,

	variables_builder: Builder<'ctx>,
	variables: HashMap<&'src str, Variable<'ctx>>,
	
	main_fn: FunctionValue<'ctx>,

	external: External<'ctx>,

	fail_variable_unset: BasicBlock<'ctx>,
	fail_type_error: BasicBlock<'ctx>,
	fail_range_error: BasicBlock<'ctx>,

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

			let external = External::new(context, &module);

			// main_fn
			let main_fn = module.add_function("main", context.i64_type().fn_type(&[], false), None);
			main_fn.set_subprogram(debug_main_function);

			// variables
			let variables_builder = context.create_builder();
			let variables_block = context.append_basic_block(main_fn, "variables");
			variables_builder.position_at_end(variables_block);

			let builder = context.create_builder();

			let fail_variable_unset = {
				let block = context.append_basic_block(main_fn, "fail_variable_unset");
				builder.position_at_end(block);
			
				// TODO: Include name of variable, and line of code.
				let args = [
					builder.build_global_string_ptr("Variabila nu are nici o valoare!\n", "")?.as_pointer_value().into(),
				];
				builder.build_call(
					external.printf,
					args.as_slice(),
					"error_printf"
				)?;

				builder.build_call(external.exit, &[context.i64_type().const_int(1, false).into()], "")?;
				builder.build_return(Some(&context.i64_type().const_int(1, false)))?;
				block
			};

			let fail_type_error = {
				let block = context.append_basic_block(main_fn, "fail_type_error");
				builder.position_at_end(block);
			
				// TODO: Include name of variable, and line of code.
				let args = [
					builder.build_global_string_ptr("Variabila are tip greșit!\n", "")?.as_pointer_value().into(),
				];
				builder.build_call(
					external.printf,
					args.as_slice(),
					"error_printf"
				)?;

				builder.build_call(external.exit, &[context.i64_type().const_int(1, false).into()], "")?;
				builder.build_return(Some(&context.i64_type().const_int(1, false)))?;
				block
			};

			let fail_range_error = {
				let block = context.append_basic_block(main_fn, "fail_range_error");
				builder.position_at_end(block);

				// TODO: Include name of variable, and line of code.
				let args = [
					builder.build_global_string_ptr("Indicele iese din listă!\n", "")?.as_pointer_value().into(),
				];
				builder.build_call(
					external.printf,
					args.as_slice(),
					"error_printf"
				)?;

				builder.build_call(external.exit, &[context.i64_type().const_int(1, false).into()], "")?;
				builder.build_return(Some(&context.i64_type().const_int(1, false)))?;
				block
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
				external,

				fail_variable_unset,
				fail_type_error,
				fail_range_error,

				debug_info_builder,
				debug_compile_unit,
				debug_main_function,
				debug_type,
			};

			let program = parse::parse(code)?;
			dbg!(&program);
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
		self.builder.build_call(self.external.exit, &[self.context.i64_type().const_int(0, false).into()], "")?;
		self.builder.build_return(Some(&self.context.i64_type().const_int(0, false)))?;
		self.variables_builder.build_unconditional_branch(start_block)?;

		self.module.print_to_stderr();

		self.debug_info_builder.finalize();
		self.module.verify().map_err(VerificationError::from)?;

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
	
	fn variable(&mut self, ident: &Ident<'src>) -> Result<Variable<'ctx>, CompilerError> {
		Ok({
			let key = ident.0;
			if !self.variables.contains_key(key) {
				let value = {
					let mut name = String::new();
					name.push_str("value_");
					name.push_str(key);
					self.variables_builder.build_alloca(self.external.variable, name.as_str())?
				};

				let is_set = {
					let mut name = String::new();
					name.push_str("is_set_");
					name.push_str(key);
					let alloca = self.variables_builder.build_alloca(self.context.i64_type(), name.as_str())?;
					self.variables_builder.build_store(alloca, self.context.i64_type().const_zero())?;
					alloca
				};

				// TODO: This is broken with new variables system.
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
					value_ptr: value,
					is_set_ptr: is_set,
				};

				self.variables.insert(key, value);
			}			
			self.variables[key]
		})
	}

	fn build_float_struct(&self, inner: FloatValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
		Ok({
			let struct_ptr = self.builder.build_alloca(self.external.variable_float, "struct")?;

			let kind_ptr = self.builder.build_struct_gep(
				self.external.variable_float,
				struct_ptr,
				0,
				"gep_kind",
			)?;
			self.builder.build_store(
				kind_ptr,
				self.context.i64_type().const_int(VariableKind::Float as u64, false),
			)?;

			let inner_ptr = self.builder.build_struct_gep(
				self.external.variable_float,
				struct_ptr,
				1,
				"gep_inner",
			)?;
			self.builder.build_store(
				inner_ptr,
				inner,
			)?;

			struct_ptr
		})
	}

	fn build_list_struct(&self, inner: PointerValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
		Ok({
			let struct_ptr = self.builder.build_alloca(self.external.variable_list, "struct")?;

			let kind_ptr = self.builder.build_struct_gep(
				self.external.variable_list,
				struct_ptr,
				0,
				"gep_kind",
			)?;
			self.builder.build_store(
				kind_ptr,
				self.context.i64_type().const_int(VariableKind::List as u64, false),
			)?;

			let inner_ptr = self.builder.build_struct_gep(
				self.external.variable_list,
				struct_ptr,
				1,
				"gep_inner",
			)?;
			self.builder.build_store(
				inner_ptr,
				inner,
			)?;

			struct_ptr
		})
	}

	fn build_list_range_check(
		&self,
		list: PointerValue<'ctx>,
		index: FloatValue<'ctx>,
	) -> CompilerResult<()> {
		Ok({
			// index < 0
			{
				let merge_block = self.context.append_basic_block(self.main_fn, "merge");

				let index_lt_zero = self.builder.build_float_compare(
					FloatPredicate::OLT,
					index,
					self.context.f64_type().const_zero(),
					"index_lt_zero",
				)?;

				self.builder.build_conditional_branch(
					index_lt_zero,
					self.fail_range_error,
					merge_block,
				)?;

				self.builder.position_at_end(merge_block);
			}

			// index >= len
			{
				let merge_block = self.context.append_basic_block(self.main_fn, "merge");

				let call = self.builder.build_call(
					self.external.pseudo_list_len,
					&[
						list.into(),
					],
					"pseudo_list_len",
				)?;
				let list_len = call.as_any_value_enum().into_float_value();

				let index_gt_len = self.builder.build_float_compare(
					FloatPredicate::OGE,
					index,
					list_len,
					"index_gt_len",
				)?;

				self.builder.build_conditional_branch(
					index_gt_len,
					self.fail_range_error,
					merge_block,
				)?;

				self.builder.position_at_end(merge_block);
			}
			
			()
		})
	}
}
