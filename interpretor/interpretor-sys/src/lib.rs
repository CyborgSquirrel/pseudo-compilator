use std::{mem::ManuallyDrop, ptr::slice_from_raw_parts};

type List = ManuallyDrop<*mut Vec<f64>>;

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_clone(list: List) -> List {
	let elements: Vec<f64> = (**list).iter().map(|a| *a).collect();
	let elements = Box::into_raw(Box::new(elements));
	ManuallyDrop::new(elements)
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_drop(mut list: List) {
	ManuallyDrop::drop(&mut list);
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_new(elements: *const f64, len: usize) -> List {
	let elements = slice_from_raw_parts(elements, len);
	let elements: Vec<f64> = elements.as_ref().unwrap().iter().map(|a| *a).collect();
	let elements = Box::into_raw(Box::new(elements));
	ManuallyDrop::new(elements)
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_get_item(list: List, index: f64) -> f64 {
	(**list)[index as usize]
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_set_item(mut list: List, index: f64, value: f64) {
	(**list)[index as usize] = value;
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_len(list: List) -> f64 {
	(**list).len() as f64
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_insert(mut list: List, index: f64, value: f64) {
	(**list).insert(index as usize, value);
}

#[no_mangle]
pub unsafe extern "C" fn pseudo_list_remove(mut list: List, index: f64) {
	(**list).remove(index as usize);
}

#[repr(C)]
pub union VariableInner {
	float: f64,
	list: List,
}

#[repr(i64)]
pub enum VariableKind {
	Null = 0,
	Float = 1,
	List = 2,
}

#[repr(C)]
pub struct Variable {
	kind: VariableKind,
	inner: VariableInner,
}
