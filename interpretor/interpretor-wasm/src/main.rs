use wasm_bindgen::prelude::wasm_bindgen;
use ouroboros::self_referencing;

impl Into<wasm_bindgen::JsValue> for runtime::RuntimeError {
	fn into(self) -> wasm_bindgen::JsValue {
		wasm_bindgen::JsValue::from_str(self.make_string().as_str())
	}
}

impl Into<wasm_bindgen::JsValue> for parse::ParsingError {
	fn into(self) -> wasm_bindgen::JsValue {
		wasm_bindgen::JsValue::from_str(self.make_string().as_str())
	}
}

#[wasm_bindgen]
#[self_referencing]
pub struct OwningRunner {
	code: Box<str>,
	
	#[not_covariant]
	#[borrows(code)]
	program: Vec<syntax::Instructiune<'this>>,
	
	#[not_covariant]
	#[borrows(program)]
	runner: runtime::Runner<'this, 'this>,
	
	#[not_covariant]
	#[borrows(code)]
	variables: runtime::Variables<'this>,
	
	input: String,
	output: String,
}

#[wasm_bindgen]
impl OwningRunner {
	#[wasm_bindgen(constructor)]
	pub fn constructor(code: String) -> parse::ParsingResult<OwningRunner> {
		let code = code.into_boxed_str();
		OwningRunnerTryBuilder {
			code: code,
			program_builder: |code: &Box<str>| parse::parse(code.as_ref()),
			runner_builder: |program: &Vec<syntax::Instructiune>| Ok(runtime::Runner::new(program)),
			variables_builder: |_| Ok(runtime::Variables::new()),
			input: String::new(),
			output: String::new(),
		}.try_build()
	}
	
	#[wasm_bindgen(js_name = runOnce)]
	pub fn run_once(&mut self) -> runtime::RuntimeResult<runtime::RuntimeState> {
		self.with_mut(|borrow| {
			borrow.runner.run_once(borrow.variables, borrow.input, borrow.output)
		})
	}
	
	#[wasm_bindgen(js_name = run)]
	pub fn run(&mut self) -> runtime::RuntimeResult<runtime::RuntimeState> {
		self.with_mut(|borrow| {
			borrow.runner.run(borrow.variables, borrow.input, borrow.output)
		})
	}
	
	#[wasm_bindgen(js_name = appendInput)]
	pub fn append_input(&mut self, append: String) {
		self.with_input_mut(|input| input.push_str(append.as_str()))
	}
	
	#[wasm_bindgen(js_name = flushOutput)]
	pub fn flush_output(&mut self) -> String {
		self.with_output_mut(|output| {
			let mut old_output = String::new();
			std::mem::swap(output, &mut old_output);
			old_output
		})
	}
}

fn main() {
    println!("Hello, world!");
}
