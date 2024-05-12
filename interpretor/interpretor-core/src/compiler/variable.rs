use inkwell::{values::{PointerValue, FloatValue, StructValue, IntValue}, IntPredicate, AddressSpace};
use interpretor_sys::VariableKind;

use crate::{Compiler, CompilerError};

#[derive(Debug, Clone, Copy)]
pub struct Variable<'ctx> {
	pub value_ptr: PointerValue<'ctx>,
	pub is_set_ptr: PointerValue<'ctx>,
}

impl<'src, 'ctx> Variable<'ctx> {
	pub fn build_set_check<'a>(
		&'a self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<SetVariable<'ctx>, CompilerError> {
		Ok({
			// TODO: Would be nice to dedup these checks.
			let is_set = compiler.builder.build_load(
				compiler.context.i64_type(),
				self.is_set_ptr,
				"load_is_set",
			)?;

			let cmp = compiler.builder.build_int_compare(
				IntPredicate::EQ,
				is_set.into_int_value(),
				compiler.context.i64_type().const_zero(),
				"check_is_set",
			)?;

			let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");
			compiler.builder.build_conditional_branch(cmp, compiler.fail_variable_unset, merge_block)?;

			compiler.builder.position_at_end(merge_block);

			SetVariable {
				value_ptr: self.value_ptr,
			}
		})
	}

	pub fn build_store(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
		rvalue: StructValue<'ctx>,
	) -> Result<(), CompilerError> {
		Ok({
			// save previous value
			let prev_value = compiler.builder.build_load(compiler.external.variable, self.value_ptr, "prev_value")?;
			let prev_value_ptr = compiler.builder.build_alloca(compiler.external.variable, "prev_value_ptr")?;
			compiler.builder.build_store(prev_value_ptr, prev_value)?;
		
			// perform store
			compiler.builder.build_store(self.value_ptr, rvalue)?;

			compiler.builder.build_store(
				self.is_set_ptr,
				compiler.context.i64_type().const_int(1, false),
			)?;

			// drop previous value, if it's a list
			{
				let x = SetVariable { value_ptr: prev_value_ptr, };
				let kind = x.build_load_kind(compiler)?;

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
    			compiler.builder.build_unconditional_branch(merge_block)?;
    		}

    		// list_block
    		{
	    		compiler.builder.position_at_end(list_block);
	    		let list = x.build_load_list_unchecked(compiler)?;
					compiler.builder.build_call(
						compiler.external.pseudo_list_drop,
						&[
							list.into(),
						],
						"pseudo_list_drop",
					)?;
					compiler.builder.build_unconditional_branch(merge_block)?;
    		}

	    	compiler.builder.position_at_end(merge_block);
			}
		})
	}
}

#[derive(Debug, Clone, Copy)]
pub struct SetVariable<'ctx> {
	pub value_ptr: PointerValue<'ctx>,
}

impl<'src, 'ctx> SetVariable<'ctx> {
	pub fn build_load_kind(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<IntValue<'ctx>, CompilerError> {
		Ok({
			let kind_ptr = compiler.builder.build_struct_gep(
				compiler.external.variable,
				self.value_ptr,
				0,
				"gep_kind",
			)?;
			let kind = compiler.builder.build_load(
				compiler.context.i64_type(),
				kind_ptr,
				"load_kind",
			)?.into_int_value();
			kind
		})
	}

	pub fn build_check_kind(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
		expected_kind: VariableKind,
	) -> Result<(), CompilerError> {
		Ok({
			let kind = self.build_load_kind(compiler)?;
			
			let cmp = compiler.builder.build_int_compare(
				IntPredicate::EQ,
				kind,
				compiler.context.i64_type().const_int(expected_kind as u64, false),
				"type_check",
			)?;

			let merge_block = compiler.context.append_basic_block(compiler.main_fn, "merge");
			compiler.builder.build_conditional_branch(cmp, merge_block, compiler.fail_type_error)?;

			compiler.builder.position_at_end(merge_block);
		})
	}

	pub fn build_load(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<StructValue<'ctx>, CompilerError> {
		Ok({
			compiler.builder.build_load(
				compiler.external.variable,
				self.value_ptr,
				"load",
			)?.into_struct_value()
		})
	}

	pub fn build_load_float(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<FloatValue<'ctx>, CompilerError> {
		Ok({
			self.build_check_kind(compiler, VariableKind::Float)?;
			
			let inner_ptr = compiler.builder.build_struct_gep(
				compiler.external.variable_float,
				self.value_ptr,
				1,
				"gep_inner",
			)?;
			let inner = compiler.builder.build_load(
				compiler.context.f64_type(),
				inner_ptr,
				"load_inner",
			)?.into_float_value();

			inner
		})
	}

	pub fn build_load_list_unchecked(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<PointerValue<'ctx>, CompilerError> {
		Ok({
			let inner_ptr = compiler.builder.build_struct_gep(
				compiler.external.variable_list,
				self.value_ptr,
				1,
				"gep_inner",
			)?;
			let inner = compiler.builder.build_load(
				compiler.context.i8_type().ptr_type(AddressSpace::default()),
				inner_ptr,
				"load_inner",
			)?.into_pointer_value();

			inner
		})
	}

	pub fn build_load_list(
		&self,
		compiler: &mut Compiler<'src, 'ctx>,
	) -> Result<PointerValue<'ctx>, CompilerError> {
		Ok({
			self.build_check_kind(compiler, VariableKind::List)?;
			self.build_load_list_unchecked(compiler)?
		})
	}
}
