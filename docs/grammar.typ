#import "style.typ": code_block

#show: rest => code_block(rest)
#show raw: it => block(it, breakable: false)

```
FloatBinop = "+" | "-" | "*" | "/" | "%".
FloatUnop = "+" | "-".
FloatExpr =
  FLOAT_LIT
  | ID
  | ID "[" FloatExpr "]"
  | FloatUnop FloatExpr
  | FloatExpr FloatBinop FloatExpr
  | "[" FloatExpr "]"
  | "(" FloatExpr ")"
  | ID "(" FloatExpr ")".
```

```
BoolFloatBinop = "=" | "!=" | "<" | ">" | "<=" | ">=" | "|".
BoolBoolBinop = "sau" | "și".
BoolExpr =
    FloatExpr BoolFloatBinop FloatExpr
  | BoolExpr BoolBoolBinop BoolExpr.

ListLitRest = FloatExpr [ "," [ ListLitRest ] ].
ListLit = "," | FloatExpr "," ListLitRest.
```

```
InstrAtribuireParam = FloatExpr | ListLit.
InstrAtribuire = ID "<-" InstrAtribuireParam.

ScrieParam =
    FloatExpr
  | STRING_LIT
  | CHAR_LIT.
InstrScrie = "scrie" ScrieParam { "," ScrieParam }.
```

```
Lvalue = ID | ID "[" FloatExpr "]"
InstrInterschimbare = Lvalue "<->" Lvalue.
InstrCiteste = "citește" Lvalue { "," Lvalue }.
```

```
InstrInsereaza = "inserează" ID "," FloatExpr "," FloatExpr.
InstrSterge = "șterge" ID "," FloatExpr.
```

```
Bloc = NEWLINE INDENT { InstrLine } DEDENT.
InstrDaca = "dacă" BoolExpr "atunci" Bloc [ "altfel" Bloc ].
InstrCatTimp = "cât timp" BoolExpr "execută" Bloc.
InstrPentru =
	"pentru" ID "<-" FloatExpr "," FloatExpr
	[ "," FloatExpr ] "execută" Bloc.
InstrRepeta = "repetă" Bloc "până când" BoolExpr.
```

```
InstrRepeatable =
    InstrAtribuire
  | InstrInterschimbare
  | InstrScrie
  | InstrCiteste
  | InstrCiteste
  | InstrSterge.
Instr =
  InstrRepeatable { ";" InstrRepeatable }
  | InstrDaca | InstrCatTimp | InstrPentru | InstrRepeta.
InstrLine = Instr NEWLINE.
```
