trace::init_depth_var!();

pub use crate::compiler::error::CompilerError;
use std::{collections::HashMap, path::Path};

use pseudo_sys::VariableKind;

use inkwell::{context::Context, values::{FloatValue, FunctionValue, PointerValue, AnyValue}, builder::Builder, module::Module, debug_info::AsDIScope, FloatPredicate, basic_block::BasicBlock};

use crate::{ast::{InstructiuneNode, IdentNode}, parse, source::{Offset, Node, Span}, LanguageSettings};

use self::{other::External, error::{VerificationError, CompilerResult}, variable::Variable, debug::DebugInfo};

mod other;
mod expression;
mod error;
mod output;
mod variable;
mod lvalue;
mod instruction;
mod fail;
mod debug;

trait Compile<'src, 'ctx> {
	type Output;
	fn compile(&self, compiler: &mut Compiler<'src, 'ctx>) -> Result<Self::Output, CompilerError>;
}

#[derive(Debug)]
pub struct Compiler<'src, 'ctx> {
	language_settings: &'src LanguageSettings,
	
	context: &'ctx Context,
	module: Module<'ctx>,

	builder: Builder<'ctx>,

	allocas_builder: Builder<'ctx>,
	variables: HashMap<&'src str, Variable<'ctx>>,

	misc_builder: Builder<'ctx>,
	misc_block: BasicBlock<'ctx>,
	
	main_fn: FunctionValue<'ctx>,

	external: External<'ctx>,
	debug_info: DebugInfo<'ctx>,

	float_type_name_ptr: PointerValue<'ctx>,
	list_type_name_ptr: PointerValue<'ctx>,

	fail_variable_unset_format_ptr: PointerValue<'ctx>,
	fail_type_error_format_ptr: PointerValue<'ctx>,
	fail_range_error_format_ptr: PointerValue<'ctx>,
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	pub fn compile<P: AsRef<Path>>(
		language_settings: &'src LanguageSettings,
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

			let external = External::new(context, &module);
			let debug_info = DebugInfo::new(&module, path);

			// main_fn
			let main_fn = module.add_function("main", context.i64_type().fn_type(&[], false), None);
			main_fn.set_subprogram(debug_info.main_function);

			// allocas
			//
			// NOTE: Apparently LLVM expects for allocas to be declared at the beginning
			// of functions, otherwise the mem2reg optimization does not function very
			// well, and this leads to the program constantly spilling variables to the
			// stack, and eventually blowing the stack up.
			let allocas_builder = context.create_builder();
			let allocas_block = context.append_basic_block(main_fn, "allocas");
			allocas_builder.position_at_end(allocas_block);

			// misc
			let misc_builder = context.create_builder();
			let misc_block = context.append_basic_block(main_fn, "misc");
			misc_builder.position_at_end(misc_block);

			let float_type_name_ptr = misc_builder.build_global_string_ptr("număr", "float_type_name")?.as_pointer_value();
			let list_type_name_ptr = misc_builder.build_global_string_ptr("listă", "list_type_name")?.as_pointer_value();

			let fail_variable_unset_format_ptr = misc_builder.build_global_string_ptr("[%d:%d] Eroare: variabila „%s” nu are nici o valoare.\n", "format")?.as_pointer_value();
			let fail_type_error_format_ptr = misc_builder.build_global_string_ptr("[%d:%d] Eroare: valoarea are tipul „%s”, însă ar fi trebuit să fi avut tipul „%s”.\n", "format")?.as_pointer_value();
			let fail_range_error_format_ptr = misc_builder.build_global_string_ptr("[%d:%d] Eroare: indicele %lf iese din intervalul [%lf; %lf).\n", "format")?.as_pointer_value();

			let builder = context.create_builder();

			let mut compiler = Self {
				language_settings,

				context,
				module,

				builder,

				allocas_builder,
				variables: HashMap::new(),

				misc_builder,
				misc_block,
	
				main_fn,
				external,
				debug_info,

				float_type_name_ptr,
				list_type_name_ptr,

				fail_variable_unset_format_ptr,
				fail_type_error_format_ptr,
				fail_range_error_format_ptr,
			};

			let program = parse::parse(&language_settings, code)?;
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

		self.allocas_builder.build_unconditional_branch(self.misc_block)?;
		self.misc_builder.build_unconditional_branch(start_block)?;

		self.debug_info.builder.finalize();
		self.module.verify().map_err(VerificationError::from)?;

		Ok(())
	}
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	fn get_debug_location(&self, location: &Offset) -> inkwell::debug_info::DILocation<'ctx> {
		let debug_location = self.debug_info.builder.create_debug_location(
			self.context,
			location.line_one(),
			location.column(),
			self.debug_info.main_function.as_debug_info_scope(),
			None,
		);
		debug_location
	}
	
	fn variable(&mut self, ident: &IdentNode<'src>) -> Result<Node<Variable<'ctx>>, CompilerError> {
		Ok({
			let key = ident.inner().0;
			if !self.variables.contains_key(key) {
				let value_ptr = {
					let name = format!("value_{key}");
					self.allocas_builder.build_alloca(self.external.variable, name.as_str())?
				};

				let is_set_ptr = {
					let name = format!("is_set_{key}");
					let alloca = self.allocas_builder.build_alloca(self.context.i64_type(), name.as_str())?;
					self.misc_builder.build_store(alloca, self.context.i64_type().const_zero())?;
					alloca
				};

				let name_ptr = {
					let name = format!("name_{key}");
					let name_ptr = self.builder.build_global_string_ptr(key, name.as_str())?.as_pointer_value();
					name_ptr
				};

				let debug_variable = self.debug_info.builder.create_auto_variable(
					self.debug_info.main_function.as_debug_info_scope(),
					key,
					self.debug_info.compile_unit.get_file(),
					0,
					self.debug_info.type_,
					true,
					0,
					0,
				);
				self.debug_info.builder.insert_declare_at_end(
					value_ptr,
					Some(debug_variable),
					None,
					self.get_debug_location(&ident.span().0),
					self.allocas_builder.get_insert_block().unwrap(),
				);

				let value = Variable {
					name_ptr,
					value_ptr,
					is_set_ptr,
				};

				self.variables.insert(key, value);
			}			
			ident.span().node(self.variables[key])
		})
	}

	fn build_float_struct(&self, inner: FloatValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
		Ok({
			let struct_ptr = self.allocas_builder.build_alloca(self.external.variable_float, "struct")?;

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
			let struct_ptr = self.allocas_builder.build_alloca(self.external.variable_list, "struct")?;

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
		span: &Span,
		list: PointerValue<'ctx>,
		index: FloatValue<'ctx>,
		upper_bound_add: FloatValue<'ctx>,
	) -> CompilerResult<()> {
		Ok({
			let merge_block = self.context.append_basic_block(self.main_fn, "merge");
			let fail_block = self.context.append_basic_block(self.main_fn, "fial");

			let lower_bound = self.context.f64_type().const_zero();

			let call = self.builder.build_call(
				self.external.pseudo_list_len,
				&[
					list.into(),
				],
				"pseudo_list_len",
			)?;
			let upper_bound = call.as_any_value_enum().into_float_value();
			let upper_bound = self.builder.build_float_add(upper_bound, upper_bound_add, "tmp_add")?;

			let fail = self.builder.build_or(
				self.builder.build_float_compare(
					FloatPredicate::OLT,
					index,
					lower_bound,
					"lower_bound",
				)?,
				self.builder.build_float_compare(
					FloatPredicate::OGE,
					index,
					upper_bound,
					"upper_bound",
				)?,
				"bounds_check",
			)?;

			self.builder.build_conditional_branch(
				fail,
				fail_block,
				merge_block,
			)?;

			{
				self.builder.position_at_end(fail_block);
				self.build_fail_range_error(
					span,
					index,
					lower_bound,
					upper_bound,
				)?;
			}
			
			self.builder.position_at_end(merge_block);
		})
	}
}
