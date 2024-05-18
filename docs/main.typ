// TODO: Referinte pentru cod

#import "@preview/lovelace:0.2.0": *
#import "@preview/fletcher:0.4.4" as fletcher
#import "@preview/cetz:0.2.2"

#show: setup-lovelace

#set page(
  margin: (left: 80pt, right: 80pt),
)
#set heading(numbering: "1.1")
#set text(size: 12pt)
#set block(spacing: 1.5em)

#show figure.where(
  kind: "code",
): set figure(supplement: "Code listing")

#show heading.where(level: 1): it => [
  #pagebreak(weak: true)
  #block(height: 60pt)
  #set text(size: 24pt)

  #let count = counter(heading)
  #if count.get().at(0) != 0 [
    Chapter #count.display()
    #linebreak()
    #linebreak()
    #it.body
  ] else [
    #it.body
  ]
  #linebreak()
  #linebreak()
]

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[(TODO: #msg)]]
}

#show figure: set block(breakable: true)

#show raw: it => {
  if it.block {
    set align(left)
    block(
      width: 100%,
      fill: luma(230),
      inset: 8pt,
      radius: 4pt,
      it,
    )
  } else {
    // it
    highlight(fill: luma(210), radius: 0pt, extent: 1pt, it)
  }
}

/*

state of the art: Antlr
- ce mai exista in domeniul limbajelor educationale

alte limbaje similare:
- https://www.99-bottles-of-beer.net/lyrics.html

educational programming languages
- https://en.wikipedia.org/wiki/List_of_educational_programming_languages
- https://www.computerscience.org/resources/best-programming-languages-for-kids/
- https://www.eduporium.com/blog/eduporium-weekly-5-programming-languages-students-could-be-learning/

*/

#include "cover.typ"

// leave a blank page
#pagebreak()
#pagebreak()

Abstract

Pseudocode compiler #todo[bla bla bla]

#pagebreak(weak: true)

#outline(indent: 32pt)

#pagebreak(weak: true)

// reset page numbering
#set page(
  numbering: "1",
)
#counter(page).update(1)

= Introduction

In this work, we implement a compiler for a language that is akin to the pseudocode that is used in the Romanian Baccalaureate exam. From here onwards, whenever we use the term "Pseudocode", it will be in reference to this language, unless specified otherwise.

The Romanian Baccalaureate is an exam that is taken as part of finishing high school in Romania. The exam typically comprises 3-4 written exams. For one of these exams, there are a few subjects from which pupils may choose, including computer science. We analyzed data available from the 2023 Baccalaureate exam, and came to the conclusion that in that year, out of the 42689 pupils taking the exam who were eligible to take the computer science thing, 8708, or around 20.39%, had chosen it for the "subject to be chosen" (@can_informatics_pie_chart).

#{
  let data = (
    ([Biology]    , 2795 ),
    ([Anatomy]    , 22601),
    ([Physics]    , 5103 ),
    ([Informatics], 8708 ),
    ([Chemistry]  , 3482 ),
  )
  let sum = data.map(value => value.at(1)).sum()

  [
    #figure(
      caption: [subjects taken by students eligible to take informatics],
      cetz.canvas({
        cetz.chart.piechart(
          data,
          value-key: 1,
          label-key: 0,
          radius: 3,
          outset: 3,
          slice-style: gradient.linear(red, blue, green, yellow),
          outer-label: (radius: 130%),
          inner-label: (content: (value, label) => [
            #(calc.floor((value/sum)*10000) / 100)%
            ], radius: 140%
          ),
        )
      })
    ) <can_informatics_pie_chart>
  ]
}

We believe that this work could potentially decrease the barrier to entry for pupils wanting to take the computer science exam. Because it would let them double check whether their solutions output the correct result, they would be able to debug them, etc.. It would also help teachers, and people grading the Baccalaureate exams, check the solutions faster, and with greater thoroughness.

In @sota we elucidate the state of the art. In @theory we go over a few theoretical concepts. In @app we describe the pseudocode language, its implementation, and integration into an approachable web application.

= State of the art <sota>

// - Prezentare sisteme, algoritmi, abordări asemănătoare sau înrudite cu subiectul lucrării
// - Detalii despre sistemele existente, cu discutarea avantajelor/dezavantajelor în general şi în relaţie cu abordarea lucrării

Educational programming languages aim to assist and encourage people to learn programming and computing concepts. Frequently, though not always, the languages are designed with a particular audience in mind. The concepts that they introduce can be narrow (e.g. the fundamentals of functional programming), as well as broad (e.g. game development with 2D sprites). We will enumerate a few such languages that we consider to be relevant to this work.

Scratch@maloney2010scratch is a programming environment that is primarily aimed at introducing children aged 8-16 to coding. Its appeal and success comes from its approachable visual editor (@scratch_online_editor), and also because it allows even people unfamiliar with programming to develop interactive, media-rich projects. It's an excellent introduction to programming, however it is not an optimal environment to learn to learn more complex aspects of computer science, such as various algorithms.

#figure(image("res/scratch.png"), caption: [scratch online editor]) <scratch_online_editor>

// MIX and MMIX are hypothetical computers, with their own respective assembly languages, used in Donald Knuth's "The Art of Computer Programming", with the purpose of educating people about what goes on inside a computer.

Python is a programming language, whose educational potential was recognized as early as 2012@kruglyk2012choosing. The syntax is easy to learn, the type system is forgiving, as the language is dynamically typed, and there is also a wealth of easily installable packages, which allow users to do develop complex projects, ranging from GUI applications to web servers.

However, we posit that Python is not an optimal language for use cases such as standardized testing. Despite the language's outward simplicity, in reality it possesses a large set of features, which is constantly being expanded with updates. It would be unreasonable to ask of all test graders to learn the entirety of Python, and to regularly learn the newest features. But it would also be inconvenient to limit students to writing only a particular version of Python, or to only use a particular set of features.

/*

points in favor of Pseudocode:
- governments are slow to adopt new tech, for instance Python
  => we'll be stuck with this Pseudocode for a while longer
  => it's worthwile improving the UX of the current Pseudocode
- existing industry-standard languages are complex, are being constantly updated, and can usually be enhanced with an ever-expanding number of downloadable packages
  => it may lead to confusion for pupils during tests (why implement a sorting algorithm manually, when you can just do list.sort())
  => or it could even confuse teachers or test graders, if a pupil happens to use a feature from a newer version of the language, which they haven't acquainted themselves with
  => it's useful to have a barebones language that doesn't receive constant updates, for the purposes of testing

*/

The Pseudocode programming language that is part of the Romanian Baccalaureate does not appear to possess an official specification, nor an implementation. It is targeted at a highly specific target audience -- high-school level pupils in Romania, and it accomplishes a specific goal -- teaching the basics of imperative computer programming, together with some algorithms.

/*

1. Pseudocode has advantages for examination, being a fixed and very simple language
2. governments adopt new technology slowly

*/

We believe that it is unlikely that the languages we have enumerated prior would be able to replace Pseudocode in the Romanian Baccalaureate. Our reasoning is twofold. First, Pseudocode possesses properties which are advantageous for its purpose of being used in examinations: it is a simple language, which does not receive updates, and which can be very quickly learned in its entirety. Second, governments generally tend to change and take up new technologies slowly #todo[cite], which means that even if an alternative language which happens to be better than Pseudocode is developed, it is not guaranteed when or if it will be adopted.

With all this in mind, we believe it's worthwhile to improve the user experience of Pseudocode, by developing editors, compilers, debuggers, etc.. It has the potential of encouraging and helping high-school students in Romania, on their path to learning informatics, while also assisting teachers and exam graders.

= Theory <theory>

// - Prezintare aspectelor teoretice (algoritmi, arhitecturi, etc) folosite în lucrare sau din care e inspirată abordarea lucrării
// - Se pot prezenta şi tehnologii/framework-uri mai interesante folosite în teză, dar în niciun caz tehnologii comune, cum ar fi php/javascript/java/.net/etc

== Shunting yard algorithm <theory_shunting_yard>

#todo[re-explain with parens]

The shunting yard algorithm is a method for parsing expressions specified in infix notation, and producing as result an AST. It was first invented by Edsger Dijkstra@dijkstrashunting.

The algorithm consists of an output queue and an operator stack. An expression is read left to right, token by token. Whenever an operand is encountered, it is added to the output queue. Whenever an operator is encountered, as long as there are operators of lesser precedence in the operator stack, the following process takes place: the top operator is popped from the stack, two operands are extracted from the output queue, then the result of applying the operator on the operands is added back to the queue.

After the tokens run out, the output stack is collapsed according to the following procedure: as long as is possible, two operands are extracted from the output queue, one operator is extracted from the operator stack, and the result of applying the operator to the operands is added back to the queue.

At the end, the operator stack must be empty, and the output queue must contain a single element, representing the output. Otherwise, the algorithm throws an error.

A pseudocode version of the algorithm is offered in @shunting_yard_code.

#todo[explain apply(), out, ops]

#algorithm(
  caption: [Shunting yard algorithm],
  pseudocode(
    [*for* token *in* tokens *do*], ind,
      [*if* token.type() $=$ "operand" *then*], ind,
        [out.enqueue(operand)],
      ded,
      [*if* token.type() $=$ "operator" *then*], ind,
        [*while* ops.len() $>$ 0 *and* ops.top().type() $!=$ "lparen" *and* token.precedence() $<$ ops.top().precedence() *do*], ind,
          [operator $<-$ ops.pop()],
          [operand#sub()[1] $<-$ out.dequeue()],
          [operand#sub()[0] $<-$ out.dequeue()],
          [out.enqueue(apply(operator, operand#sub()[0], operand#sub()[1]))],
        ded,
        [ops.push(token)],
      ded,
      [*if* token.type() $=$ "lparen" *then*], ind,
        [ops.push(token)],
      ded,
      [*if* token.type() $=$ "rparen" *then*], ind,
        [*while* ops.len() $>$ 0 *and* ops.top().type() $!=$ "lparen" *do*], ind,
          [operator $<-$ ops.pop()],
          [operand#sub()[1] $<-$ out.dequeue()],
          [operand#sub()[0] $<-$ out.dequeue()],
          [out.enqueue(apply(operator, operand#sub()[0], operand#sub()[1]))],
        ded,
        [ops.pop()],
      ded,
    ded,
    [*while* ops.len() $>=$ 1 *and* out.len() $>=$ 2], ind,
      [operator $<-$ ops.pop()],
      [*assert* operator.type() $!=$ "lparen"],
      [operand#sub()[1] $<-$ out.dequeue()],
      [operand#sub()[0] $<-$ out.dequeue()],
      [out.enqueue(apply(operator, operand#sub()[0], operand#sub()[1]))],
    ded,
    [*assert* ops.len() $=$ 0],
    [*assert* out.len() $=$ 1],
    [result $<-$ out.dequeue()],
    [*return* result],
  )
) <shunting_yard_code>

== LLVM <theory_llvm>

The LLVM Project is a collection of modular and reusable compiler and toolchain technologies@llvminfrastructure.

In this work, we use a Rust wrapper over the LLVM C library, called Inkwell#footnote[https://github.com/TheDan64/inkwell].

LLVM can be used as the backend to a programming language. It can help turn unoptimized code into optimized code. Your code writes LLVM Intermediate Representation, commonly referred to as LLVM IR. LLVM IR sort of looks like higher-level assembly language. After your code is finished outputting the IR, you hand it off to LLVM to perform optimization, and to turn it into an object file.

For example, compiling the C program in @simple_c_program, results in the LLVM IR at @simple_c_program_ll#footnote[`clang` version #read("llvm_example/target/clang_version.txt").trim() was used].

#figure(kind: "code", caption: [Simple C program])[
  #raw(read("llvm_example/main.c"), lang: "c", block: true)
] <simple_c_program>

#[
  #show raw: set text(size: 8pt)
  #figure(kind: "code", caption: [LLVM IR generated for @simple_c_program])[
    #raw(read("llvm_example/target/main.ll"), lang: "ll", block: true)
  ] <simple_c_program_ll>
]

== Recursive descent

#todo[explain recursive descent parsing, or maybe don't?]

= Application <app>

== Pseudocode language description

We aimed to make our implementation of the Pseudocode language as close to the version that is used in the Baccalaureate exam as possible, while keeping it easy to write. We will refer to the Pseudocode used in the Baccalaureate exam as "Baccalaureate Pseudocode", and to our implementation as "Pseudocode" or "our Pseudocode". Some of the main differences in syntax between our Pseudocode, and Baccalaureate Pseudocode, can be observed in @baccalaureate_vs_ours.

While Baccalaureate Pseudocode uses box-drawing characters to delimit code blocks, we chose to simply use indentation, akin to what is done in the Python programming language. Additionally, we approximate characters such as `←` and `≤` (among others), with versions which are easier to type: `<-` and `<=`.

#figure(
  kind: "code",
  caption: [structurally equivalent Pseudocode, Baccalaureate (left) and ours (right)],
)[
  #columns(2)[
    ```
    citește x
    i ← 2
    ┌cât timp i*i ≤ x execută
    │	┌dacă x % i = 0 atunci
    │	│	scrie i
    │	│	┌dacă i ≠ x/i atunci
    │	│	│	scrie x/i
    │	│	└■
    │	└■
    │	i ← i+1
    └■
    ```
    #colbreak()
    ```
    citește x
    i <- 2
    cât timp i*i <= x execută
    	dacă x % i = 0 atunci
    		scrie i
    		dacă i != x/i atunci
    			scrie x/i
    	i <- i+1
    ```
  ]
] <baccalaureate_vs_ours>

The Pseudocode language accomodates an imperative programming style. In Baccalaureate Pseudocode, variables may only be of floating point type. In our implementation, variables may also be lists of floating point numbers (further described in @lists_section). This functionality may be disabled, so that the compiler's behavior will match the Baccalaureate more closely.

One issue that arises when the only numeric type is floating-point numbers, is verifying whether two numbers are equal. An experienced programmer will know to use an $epsilon$ threshold value, and to perform a comparison between $x$ and $y$ using a formula such as $abs(x-y)<epsilon$, however Pseudocode is not aimed at experienced programmers.

On the contrary, its supposed to assist high school students, who have just started to learn to program. Taking this into consideration, we decided to make the compiler automatically generate comparisons this way.

=== Supported statements

In the following examples, an indented block of statements will be denoted with `...`.

#todo[expressions]

The language supports console input and output, with `citește` and `scrie` respectively (@citeste_scrie), variable assignment with `{variable} <- {value}`, and swapping of variables with `{left} <-> {right}` (@assignment_swap).

#figure(kind: "code", caption: [citește and scrie statements])[
```
citește x
scrie "x=", x
```] <citeste_scrie>

#figure(kind: "code", caption: [variable assignment and swapping])[
```
x <- 1
y <- 2
x <-> y
```] <assignment_swap>

It supports various control-flow statements, such as if-else statements (@if_else), while loops (@while), repeat ... until loops (@repeat_until), and for loops (@for_increment, @for).

#figure(kind: "code", caption: [if-else statement: `dacă {condition} atunci ... altfel ...`])[
```
x <- 10
dacă x < 5 atunci
  scrie "x<5"
altfel
  scrie "x>=5"
```] <if_else>

#figure(kind: "code", caption: [while loop: `cât timp {condition} execută ...`])[
```
x <- 0
cât timp x < 10 execută
  x <- x+1
```] <while>

#figure(kind: "code", caption: [repeat ... until loop: `repetă ... până când {condition}`])[
```
x <- 10
repetă
  x <- x-1
până când x <= 0
```] <repeat_until>

#figure(kind: "code", caption: [for loop: `pentru {index} <- {start}, {stop}, {increment} execută ...`])[
```
pentru i <- 1,10,2 execută
  scrie i
```] <for_increment>

#figure(kind: "code", caption: [specifying an `increment` for a for loop is optional])[
```
pentru i <- 1,100 execută
  scrie i
```] <for>

=== Lists <lists_section>

We extended the language with lists, so that pupils may use Pseudocode to study more complex algorithms involving lists.

#figure(kind: "code", caption: [#todo[]])[
```
list <- 1,2,3,4,5
pentru i<-0,lungime(list)-1 execută
  list[i] <- list[i]+1
  scrie list[i]
```] <lists_example>

#figure(kind: "code", caption: [#todo[]])[
```
list <- 1,2,3,4,5
pentru i<-0,lungime(list)-1 execută
  list[i] <- list[i]+1
  scrie list[i]
```] <lists_example_two>

=== Sample programs

Despite being a simple language, it posesses enough complexity so as to be used for educational purposes, for instance teaching students an algorithm.

The following sample program performs bubble sort on an unordered list of numbers:
#figure(kind: "code", caption: [#todo[write smth]])[
```
list <- 53, 34, 12, 665, 34, 23, 54, 65, 123, 65

pentru i<-0,lungime(list)-1-1 executa
	pentru j<-0,lungime(list)-i-1-1 executa
		daca list[j] > list[j+1] atunci
			list[j] <-> list[j+1]

pentru i<-0,lungime(list)-1 executa
	scrie list[i]
``` ]

The following program approximates the value of $sin(x)$, by way of Taylor polynomial, $x$ being read from the command line:
#figure(kind: "code", caption: [#todo[write smth]])[
```
citește x
gata <- 0
r <- 0
t <- x
n <- 1
cât timp gata = 0 execută
	r <- r + t
	
	s <- -1 * ((x*x) / ((2*n)*(2*n+1))) * t
	dacă s = t atunci
		gata <- 1
	t <- s
	n <- n + 1
scrie r
```]

=== EBNF Grammar

Grammar of pseudocode language in EBNF.

#todo[add lists and clear up writing and variables]

`IDENT_GRAPHEME` is any unicode grapheme, with the exception of: `+-*/%|=!<>()[]`.

`INDENT` and `DEDENT` are special symbols representing the increase of the indentation level by one, and the decrease of the indentation level by one, respectively.

```
Digit = "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9".

IdentRest = IDENT_GRAPHEME | Digit.
Ident = IDENT_GRAPHEME { IdentRest }.

FloatBinop = "+" | "-" | "*" | "/" | "%".
FloatUnop = "+" | "-".
FloatLit = Digit { Digit } [ "." { Digit } ].
FloatExpr =
  FloatLit
  | Ident
  | FloatUnop FloatExpr
  | FloatExpr FloatBinop FloatExpr
  | "[" FloatExpr "]"
  | "(" FloatExpr ")".

BoolFloatBinop = "=" | "!=" | "<" | ">" | "<=" | ">=" | "|".
BoolBoolBinop = "sau" | "și".
BoolExpr = FloatExpr BoolFloatBinop FloatExpr | BoolExpr BoolBoolBinop BoolExpr.

InstrAtribuire = Ident "<-" FloatExpr.
InstrInterschimbare = Ident "<->" Ident.
ScrieParam =
  FloatExpr
  | D_QUOTE UNICODE_GRAPHEME_EXCEPT_D_QUOTE D_QUOTE
  | QUOTE UNICODE_GRAPHEME_EXCEPT_QUOTE QUOTE.
InstrScrie = "scrie" ScrieParam { "," ScrieParam }.
InstrCiteste = "citește" Ident { "," Ident }.

Bloc = NEWLINE INDENT { InstrLine } DEDENT.
InstrDaca = "dacă" BoolExpr "atunci" Bloc [ "altfel" Bloc ].
InstrCatTimp = "cât timp" BoolExpr "execută" Bloc.
InstrPentru =
  "pentru" Ident "<-" FloatExpr "," FloatExpr [ "," FloatExpr ] "execută" Bloc.
InstrRepeta = "repetă" Bloc "până când" BoolExpr.

InstrRepeatable =
  InstrAtribuire
  | InstrInterschimbare
  | InstrScrie
  | InstrCiteste.
Instr =
  InstrRepeatable { ";" InstrRepeatable }
  | InstrDaca | InstrCatTimp | InstrPentru | InstrRepeta.
InstrLine = Instr NEWLINE.
```

== Compiler implementation

Rust was chosen as the implementation language, for its performance and memory-safety characteristics. Rust's safety guarantees are especially useful in the implementation of the parser, because they enable the developer to safely process strings, without excessive copying, which would harm performance.

Also, Rust's strong type system made it easier to catch mistakes while writing the code.

Additionally, in the implementation, all diacritics which are part of keywords are considered to be optional, so as to ease the process of writing pseudocode programs. In the following example, both lines will be parsed as the `citește` statement.
```
citește x
citeste x
```

=== Parsing

#todo[explain that no tokenizing is done, and the whole thing is parsed in one pass; that is in part because of `<-`, which can either show up in an assignment (e.g. `a <- 42`), or in a boolean expression, signifying "less than a negative of something" `daca x<-42 atunci`]

Use recursive descent because of the particularities of this programming languages, such as having spaces inside keywords, like `cât timp` and `până când`. The code consists of small functions, each function having a very specific purpose, such as parsing a boolean operator, or an if statement.

For parsing expressions, a modified version of the shunting yard algorithm was employed, as described in @theory_shunting_yard.

The result of the parsing process is an AST (Abstract Syntax Tree) of the program that is being parsed.

#todo[figure with graph of AST for some code]

=== Compilation

The LLVM (@theory_llvm) library is used to assist with compilation. The AST is used to generate LLVM IR, which is subsequently compiled by LLVM into an object file. Then the object file is linked into the final executable using the `clang` command, which also takes care of linking other libraries into the executable, such as `libc`.

During the LLVM IR generation process, we also make sure to include debugging information, which allows the compiled executable to be debugged at source-level, using any debugger that supports the DWARF debugging information format, such as `gdb`.

#todo[add picture of code in debugger]

== Accessible online editor

#{
  let fe = (0,1);
  let be_job = (2,0);
  let be_job_status = (2,1);
  let be = (4,1);
  let cmp = (3,3);
  let exe = (4,3);
  figure(
    fletcher.diagram(
      spacing: 2em,
  
      fletcher.node(fe, [Frontend], stroke: 1pt),
      fletcher.node(be_job, [HTTP POST `/job`], stroke: 1pt),
      fletcher.node(be_job_status, [WebSocket `/job/{job_id}/status`], stroke: 1pt),
      fletcher.node(be, [Backend], stroke: 1pt),
      fletcher.node(cmp, [Compiler], stroke: 1pt),
      fletcher.node(exe, [Executable<asd>], stroke: 1pt),

      fletcher.edge(fe, "r,u,r", "->", shift: (-5pt, 0)),
      fletcher.edge(fe, "rr", "<->", shift: (0, 0pt)),

      fletcher.edge(be_job, "r,d,r", shift: (0, -5pt)),
      fletcher.edge(be_job_status, "rr", shift: (0, 0pt)),

      fletcher.edge(be, "d,l,d", "<->", shift: (5pt, 0)),
      fletcher.edge(be, "dd", "<->", shift: (0pt, 0)),
      fletcher.edge(cmp, exe, "=>"),
    ),
    caption: [online editor architecture],
  )
}

We have implemented an online editor that should be accessible for non-technical people, such as high school pupils learning informatics.

The frontend of the editor was implemented using React, along with CodeMirror providing the actual text editor implementation. The backend consists of a Python HTTP server, which receives the code from the frontend, compiles it, and runs the resulting executable.

After the executable is run, the backend proxies all communication between the frontend and the running executable, through a WebSocket. Everything the executable writes to standard output is sent through the socket, and every string a user writes and submits into the frontend console is forwarded to the process' standard input.

#figure(caption: [online editor frontend])[#image("res/editor.png")]

// If the code happ

= Future work

== Syntax highlighting

Syntax highlighting could be added to the editor. It would not even be that difficult: as it's parsing, the parser basically extracts all the information necessary for syntax highlighting. It would have to be slightly modified for this task, but it would not be a very significant modification.

Then, this modified parser could be compiled to WebAssembly, and included into the final editor website. If you ignore indentation, then if you parse the entire document, then change one line, only that line will have to be re-parsed. Therefore, syntax highlighting could be done quite efficiently, by re-parsing and re-highlighting only lines of code that have been changed.

== Improved error message source location

Currently, when a parsing error happens, the parser only outputs the line and column where the error happened. Due to this, all the editor can do, is highlight the line where the error happened. It would be better if the parser returned a starting and ending pair of (line, column). This way, the exact location where the error occured would be easier to find, and errors would be easier to diagnose and fix.

== Execution of source code from a photo

#let ocr_footnote = footnote[
  For example, Google's Cloud Vision API (https://cloud.google.com/vision/docs/handwriting) can extract handwriting from an image.
]

A potentially useful feature for pupils, teachers, and exam graders alike, would be the possiblity to easily execute Pseudocode source code, that has been written on a piece of paper. This could be achieved by integrating our compiler with a handwriting detection OCR system#ocr_footnote.

As Pseudocode is very commonly written on paper, this would make it easier for everyone to execute and check it.

= Conclusions

a

#pagebreak(weak: true)
#bibliography("works.bib")
