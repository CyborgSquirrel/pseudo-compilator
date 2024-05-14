use inkwell::{values::{PointerValue, IntValue, FloatValue}, AddressSpace};
use interpretor_sys::VariableKind;

use crate::source::Span;

use super::{Compiler, error::CompilerResult};

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	pub fn build_fail_variable_unset(
		&self,
		span: &Span,
		name_ptr: PointerValue<'ctx>,
	) -> CompilerResult<()> {
		Ok({
			self.builder.build_call(
				self.external.printf,
				&[
					self.fail_variable_unset_format_ptr.into(),
					self.context.i64_type().const_int(span.0.line_one() as u64, false).into(),
					self.context.i64_type().const_int(span.0.column() as u64, false).into(),
					name_ptr.into(),
				],
				"fail_variable_unset"
			)?;

			self.builder.build_call(self.external.exit, &[self.context.i64_type().const_int(1, false).into()], "")?;
			self.builder.build_return(Some(&self.context.i64_type().const_int(1, false)))?;
		})
	}

	pub fn build_fail_type_error(
		&self,
		span: &Span,
		type_: IntValue<'ctx>,
		expected_type: VariableKind,
	) -> CompilerResult<()> {
		Ok({
			let expected_type_str_ptr = match expected_type {
				VariableKind::Null => panic!(),
				VariableKind::Float => self.float_type_name_ptr,
				VariableKind::List => self.list_type_name_ptr,
			};

			let merge_block = self.context.append_basic_block(self.main_fn, "merge");
			let float_block = self.context.append_basic_block(self.main_fn, "float");
			let list_block = self.context.append_basic_block(self.main_fn, "list");

			let type_str_ptr_ptr = self.builder.build_alloca(self.context.i8_type().ptr_type(AddressSpace::default()), "type_str_ptr")?;

			self.builder.build_switch(
				type_,
  			merge_block,
    		&[
  				(
	    			self.context.i64_type().const_int(VariableKind::Float as u64, false),
	    			float_block,
  				),
  				(
	    			self.context.i64_type().const_int(VariableKind::List as u64, false),
	    			list_block,
  				),
    		],
			)?;

			{
				self.builder.position_at_end(float_block);
				self.builder.build_store(type_str_ptr_ptr, self.float_type_name_ptr)?;
				self.builder.build_unconditional_branch(merge_block)?;
			}

			{
				self.builder.position_at_end(list_block);
				self.builder.build_store(type_str_ptr_ptr, self.list_type_name_ptr)?;
				self.builder.build_unconditional_branch(merge_block)?;
			}

			self.builder.position_at_end(merge_block);
			let type_str_ptr = self.builder.build_load(
				self.context.i8_type().ptr_type(AddressSpace::default()),
				type_str_ptr_ptr,
				"type_str_ptr_ptr",
			)?;
			
			self.builder.build_call(
				self.external.printf,
				&[
					self.fail_type_error_format_ptr.into(),
					self.context.i64_type().const_int(span.0.line_one() as u64, false).into(),
					self.context.i64_type().const_int(span.0.column() as u64, false).into(),
					expected_type_str_ptr.into(),
					type_str_ptr.into(),
				],
				"fail_type_error"
			)?;

			self.builder.build_call(self.external.exit, &[self.context.i64_type().const_int(1, false).into()], "")?;
			self.builder.build_return(Some(&self.context.i64_type().const_int(1, false)))?;
		})
	}

	pub fn build_fail_range_error(
		&self,
		span: &Span,
		index: FloatValue<'ctx>,
		lower_bound: FloatValue<'ctx>,
		upper_bound: FloatValue<'ctx>,
	) -> CompilerResult<()> {
		Ok({
			self.builder.build_call(
				self.external.printf,
				&[
					self.fail_range_error_format_ptr.into(),
					self.context.i64_type().const_int(span.0.line_one() as u64, false).into(),
					self.context.i64_type().const_int(span.0.column() as u64, false).into(),
					index.into(),
					lower_bound.into(),
					upper_bound.into(),
				],
				"fail_range_error"
			)?;

			self.builder.build_call(self.external.exit, &[self.context.i64_type().const_int(1, false).into()], "")?;
			self.builder.build_return(Some(&self.context.i64_type().const_int(1, false)))?;
		})
	}
}
