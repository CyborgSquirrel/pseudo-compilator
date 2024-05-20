use std::path::Path;

use inkwell::{debug_info::{DebugInfoBuilder, DICompileUnit, DISubprogram, DIType, AsDIScope}, module::Module};

/// Debug info boilerplate.
#[derive(Debug)]
pub struct DebugInfo<'ctx> {
	pub builder: DebugInfoBuilder<'ctx>,
	pub compile_unit: DICompileUnit<'ctx>,
	pub main_function: DISubprogram<'ctx>,
	pub type_: DIType<'ctx>,
}

impl<'ctx> DebugInfo<'ctx> {
	pub fn new<P: AsRef<Path>>(
		module: &Module<'ctx>,
		path: P,
	) -> Self {
		let path = path.as_ref();
		
		let (info_builder, compile_unit) = module.create_debug_info_builder(
			false,
			inkwell::debug_info::DWARFSourceLanguage::C,
			path.file_name().unwrap().to_str().unwrap(),
			path.parent().unwrap().canonicalize().unwrap().to_str().unwrap(),
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

		let main_function_type = info_builder.create_subroutine_type(
			compile_unit.get_file(),
			None,
			&[],
			0,
		);

		let main_function = info_builder.create_function(
			compile_unit.as_debug_info_scope(),
			"main",
			None,
			compile_unit.get_file(),
			0,
			main_function_type,
			false,
			true,
			0,
			0,
			false,
		);

		let type_ = {
			// Source for LLVMDWARFTypeEncoding values:
			// https://dwarfstd.org/doc/DWARF5.pdf#section.7.8

			let int_type = info_builder.create_basic_type(
				"int",
				64,
				0x07, // unsigned int
				0,
			).unwrap().as_type();

			let float_type = info_builder.create_basic_type(
				"float",
				64,
				0x04, // float
				0,
			).unwrap().as_type();

			let ptr_type = info_builder.create_basic_type(
				"ptr",
				64,
				0x01, // address
				0,
			).unwrap().as_type();

			let discriminant_type = info_builder.create_member_type(
				compile_unit.as_debug_info_scope(),
				"discriminant",
				compile_unit.get_file(),
				0,
				64,
				0,
				0,
				0,
				int_type,
			).as_type();

			let float_inner_type = info_builder.create_member_type(
				compile_unit.as_debug_info_scope(),
				"float_inner",
				compile_unit.get_file(),
				0,
				64,
				0,
				64,
				0,
				float_type,
			).as_type();

			let float_variant_type = info_builder.create_struct_type(
				compile_unit.as_debug_info_scope(),
				"float_variant",
				compile_unit.get_file(),
				0,
				128,
				0,
				0,
				None,
				&[
					discriminant_type,
					float_inner_type,
				],
				0,
				None,
				"float_variant",
			).as_type();

			let float_variant_member_type = info_builder.create_member_type(
				compile_unit.as_debug_info_scope(),
				"float_variant_member",
				compile_unit.get_file(),
				0,
				128,
				0,
				0,
				0,
				float_variant_type,
			).as_type();

			let list_inner_type = info_builder.create_member_type(
				compile_unit.as_debug_info_scope(),
				"list_inner",
				compile_unit.get_file(),
				0,
				64,
				0,
				64,
				0,
				ptr_type,
			).as_type();

			let list_variant_type = info_builder.create_struct_type(
				compile_unit.as_debug_info_scope(),
				"list_variant",
				compile_unit.get_file(),
				0,
				128,
				0,
				0,
				None,
				&[
					discriminant_type,
					list_inner_type,
				],
				0,
				None,
				"list_variant",
			).as_type();

			let list_variant_member_type = info_builder.create_member_type(
				compile_unit.as_debug_info_scope(),
				"list_variant_member",
				compile_unit.get_file(),
				0,
				128,
				0,
				0,
				0,
				list_variant_type,
			).as_type();

			let variable_type = info_builder.create_union_type(
				compile_unit.as_debug_info_scope(),
				"variable",
				compile_unit.get_file(),
				0,
				128,
				0,
				0,
				&[
					float_variant_member_type,
					list_variant_member_type,
				],
				0,
				"variable",
			).as_type();

			variable_type
		};

		DebugInfo {
			builder: info_builder,
			compile_unit,
			main_function,
			type_,
		}
	}
}
