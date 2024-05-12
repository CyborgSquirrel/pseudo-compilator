use inkwell::{values::FunctionValue, module::Module, context::Context, AddressSpace, llvm_sys::LLVMCallConv, types::{StructType, BasicType}};

pub use interpretor_sys::VariableKind;

#[derive(Debug)]
pub struct External<'ctx> {
	pub printf: FunctionValue<'ctx>,
	pub scanf: FunctionValue<'ctx>,
	pub exit: FunctionValue<'ctx>,

	pub pseudo_list_clone: FunctionValue<'ctx>,
	pub pseudo_list_drop: FunctionValue<'ctx>,
	pub pseudo_list_new: FunctionValue<'ctx>,
	pub pseudo_list_get_item: FunctionValue<'ctx>,
	pub pseudo_list_set_item: FunctionValue<'ctx>,
	pub pseudo_list_len: FunctionValue<'ctx>,

	pub variable: StructType<'ctx>,
	pub variable_float: StructType<'ctx>,
	pub variable_list: StructType<'ctx>,
}

impl<'ctx> External<'ctx> {
	pub fn new(
		context: &'ctx Context,
		module: &Module<'ctx>,
	) -> Self {
		// libc
		
		// printf
		let printf = module.add_function(
			"printf",
			context.i64_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into(),
				],
				true,
			),
			None,
		);
		printf.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// scanf
		let scanf = module.add_function(
			"scanf",
			context.i64_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into(),
				],
				true,
			),
			None,
		);
		scanf.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// exit
		let exit = module.add_function(
			"exit",
			context.void_type().fn_type(
				&[
					context.i64_type().into(),
				],
				false,
			),
			None,
		);
		exit.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// interpretor-sys

		// pseudo_list_clone
		let pseudo_list_clone = module.add_function(
			"pseudo_list_clone",
			context.i8_type().ptr_type(AddressSpace::default()).fn_type(
				&[
					context.f64_type().ptr_type(AddressSpace::default()).into(),
				],
				false,
			),
			None,
		);
		pseudo_list_clone.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// pseudo_list_drop
		let pseudo_list_drop = module.add_function(
			"pseudo_list_drop",
			context.i8_type().ptr_type(AddressSpace::default()).fn_type(
				&[
					context.f64_type().ptr_type(AddressSpace::default()).into(),
				],
				false,
			),
			None,
		);
		pseudo_list_drop.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// pseudo_list_new
		let pseudo_list_new = module.add_function(
			"pseudo_list_new",
			context.i8_type().ptr_type(AddressSpace::default()).fn_type(
				&[
					context.f64_type().ptr_type(AddressSpace::default()).into(),
					context.i64_type().into(),
				],
				false,
			),
			None,
		);
		pseudo_list_new.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// pseudo_list_get_item
		let pseudo_list_get_item = module.add_function(
			"pseudo_list_get_item",
			context.f64_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into(),
					context.f64_type().into(),
				],
				false,
			),
			None,
		);
		pseudo_list_get_item.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// pseudo_list_set_item
		let pseudo_list_set_item = module.add_function(
			"pseudo_list_set_item",
			context.f64_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into(),
					context.f64_type().into(),
					context.f64_type().into(),
				],
				false,
			),
			None,
		);
		pseudo_list_set_item.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// pseudo_list_len
		let pseudo_list_len = module.add_function(
			"pseudo_list_len",
			context.f64_type().fn_type(
				&[
					context.i8_type().ptr_type(AddressSpace::default()).into(),
				],
				false,
			),
			None,
		);
		pseudo_list_len.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

		// Variable
		let variable = context.struct_type(
			&[
				context.i64_type().into(),
				context.i8_type().array_type(8).into(), // padding
			],
			false,
		);

		let variable_float = context.struct_type(
			&[
				context.i64_type().into(),
				context.f64_type().into(),
			],
			false,
		);

		let variable_list = context.struct_type(
			&[
				context.i64_type().into(),
				context.i8_type().ptr_type(AddressSpace::default()).into(),
			],
			false,
		);

		Self {
			printf,
			scanf,
			exit,

			pseudo_list_clone,
			pseudo_list_drop,
			pseudo_list_new,
			pseudo_list_get_item,
			pseudo_list_set_item,
			pseudo_list_len,

			variable,
			variable_float,
			variable_list,
		}
	}
}
