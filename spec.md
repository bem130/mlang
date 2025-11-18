mylang 言語仕様書 (Language Specification)
Version: 1.2 (Comprehensive)
Paradigm: Expression-Oriented, Stack-Based (Implicit), Statically Typed

## 1. 概要 (Overview)

mylang は、式指向 (Expression-Oriented) の静的型付け言語です。以下の主要な特徴を持ちます：

*   **全てが式**: `if`, `match`, `block`, `loop` など、ほぼすべての構文が値を返します。
*   **柔軟な呼び出しスタイル**: ポーランド記法 (P-style) を基本としつつ、C言語風 (C-style)、数式 (Math-style)、パイプライン (`>`) を適材適所で使い分けることができます。
*   **多値返却 (MRV)**: スタック指向の特性を活かし、複数の値をネイティブに返し、文脈に応じて展開またはタプル化します。
*   **省略記法**: `let` や `if` などで冗長な記号（`=` や `{}`）を省略し、簡潔な記述が可能です。

## 2. 構文定義 (Syntax / EBNF)

### 2.1. プログラム構造

プログラムはトップレベル定義の集合です。

```
Program = { TopLevelDefinition } ;

TopLevelDefinition = LetStatement
                   | LetFunctionStatement
                   | StructDefinition
                   | EnumDefinition
                   ;
```

### 2.2. 定義文 (Definitions)

注意: `let` 文において `=` は使用しません。

```
(* 変数定義: hoist を指定するとスコープ先頭に巻き上げられる *)
(* 例: let x 10; *)
LetStatement = "let" [ "mut" | "hoist" ] Identifier [ ":" Type ] Expression ";" ;

(* 関数定義: let hoist ... の糖衣構文 *)
(* 例: fn add |a:i32, b:i32|->i32 $a+b$; *)
LetFunctionStatement = "fn" Identifier FunctionExpression ";" ;

(* データ構造定義 *)
StructDefinition = "struct" Identifier "{" { Identifier ":" Type [ "," ] } "}" ;
EnumDefinition   = "enum" Identifier [ Generics ] "{" { Identifier [ "(" [ TypeList ] ")" ] [ "," ] } "}" ;
Generics         = "[" Identifier { "," Identifier } "]" ;
```

### 2.3. 式 (Expression)

式は結合強度（優先順位）に従って階層化されています。

```
(* 最上位の式 *)
Expression = PipeExpression
           | IfExpression
           | MatchExpression
           | BlockExpression
           | LoopExpression
           | WhileLoop
           ;

(* パイプ式: 左結合。BaseExpression をパイプで繋ぐ *)
(* 例: 10 > add 5 > double *)
PipeExpression = BaseExpression { ">" BaseExpression } ;

(* 基本式: パイプや制御構文の構成要素 *)
BaseExpression = PStyleExpression
               | PrimaryExpression
               ;

(* P-style: ポーランド記法。1つ以上の PrimaryExpression の列 *)
(* パーサーはこれをフラットなリストとして読み込み、意味解析で構造化する *)
(* 例: add 1 2 *)
PStyleExpression = { PrimaryExpression }+ ;

(* PrimaryExpression: 単一の式要素 *)
PrimaryExpression = Atom
                  | ParenthesizedExpression
                  | CStyleExpression
                  | MathExpression
                  | FunctionExpression
                  | ArrayLiteral
                  ;

Atom = Identifier | Literal ;
```

### 2.4. 詳細な式構文

```
(* グルーピング: 任意の式を PrimaryExpression 化する *)
ParenthesizedExpression = "(" Expression ")" ;

(* 関数式: 多値返却型をサポート *)
FunctionExpression = "|" [ ParamList ] "|" [ "->" TypeList ] Expression ;
ParamList = Identifier ":" Type { "," Identifier ":" Type } [ "," ] ;
TypeList  = Type { "," Type } ;

(* C-style呼び出し: タプル化コンテキスト *)
(* 例: add(1, 2) *)
CStyleExpression = PrimaryExpression "(" [ ExpressionList ] ")" ;
ExpressionList   = SpreadExpression | Expression { "," Expression } [ "," ] ;
SpreadExpression = "..." Expression ; (* タプルを展開 *)

(* Math-style: $ で囲まれた中置記法 *)
MathExpression = "$" InfixExpression "$" ;

(* 配列リテラル: タプル化コンテキスト *)
ArrayLiteral = "[" [ ExpressionList ] "]" ;

(* 制御構文 *)
IfExpression    = "if" Expression Expression { "else" "if" Expression Expression } [ "else" Expression ] ;
MatchExpression = "match" Expression "{" { Pattern "=>" Expression [ "," ] } "}" ;
WhileLoop       = "while" Expression Expression ;
LoopExpression  = "loop" Expression ;

(* ブロック式 *)
BlockExpression = "{" { Statement } [ Expression ] "}" ;
Statement       = LetStatement | ( Expression ";" ) ;
```

### 2.5. 型システム

```
Type = Identifier             (* i32, string, MyStruct など *)
     | "[" Type ";" SExpression "]" (* 固定長配列 [i32; 3] *)
     ;
```

## 3. 意味論 (Semantics)

### 3.1. スコープルール (Scoping)

mylang はレキシカルスコープ（静的スコープ）を採用します。

*   **ブロック `{ ... }`**:
    新たなスコープを導入します。内部で `let` された変数はブロック外へリークしません。`if`, `while`, `fn` などの本体として `{}` が使われた場合も同様です。
*   **関数引数**:
    `|a: i32| ...` の引数は、その関数本体 (Expression) 全体で有効です。
*   **Matchアーム**:
    パターンマッチで束縛された変数は、そのアーム式内でのみ有効です。
*   **条件式内のブロック**:
    `if { let a=1; a } ...` のように条件部でブロックを使った場合、そのスコープは条件判定直後に破棄されます（Then/Else節からは見えません）。

### 3.2. P-style の解決アルゴリズム (R-to-L Maximal Munch)

`PStyleExpression`（フラットな式のリスト、例: `[f, a, b]`）は以下の手順で構文解析・型解決されます。

1.  **スキャン (Right-to-Left)**: リストを右端から左へスキャンします。
2.  **関数同定**: `Identifier` (関数) を発見すると、その右側にある解決済みの式（アトム）を引数候補とします。
3.  **Maximal Munch (最大引数優先)**:
    その関数のオーバーロード定義の中から、「型エラーにならず、かつ最大の引数を取るもの」 を貪欲に選択します。
    例: `f(i32)` と `f(i32, i32)` があり、右側に `i32`, `i32` がある場合、`f(i32, i32)` が選択されます。
4.  **畳み込み (Collapse)**:
    マッチした関数と引数を消費し、その返り値の型を持つ単一（または多値）のアトムに置換します。
5.  **反復**: リスト全体が解決されるまでスキャンを繰り返します。

### 3.3. パイプ演算子 (`>`) の挙動

構文 `LHS > RHS` は、「LHS の結果を、RHS の関数の『第一引数』として注入する」 糖衣構文です。

*   **結合性**: 左結合です。`A > B > C` は `(A > B) > C` と解釈されます。
*   **左辺の抽出 (LHS Extraction)**:
    P-style の連鎖の中に `>` がある場合、`>` の左側のトークン列から、**直近の完結した式** だけを LHS として切り出します。
    例: `add 1 add 2 3 > add 4`
    `>` の左側を後ろから解析し、`add 2 3` が完結した式 (5) となる時点でストップします。
    式は `add 1 ( (add 2 3) > add 4 )` と等価になります。

### 3.4. 多値返却とコンテキスト (Multiple Return Values)

関数が `-> T1, T2` のように複数の値を返す場合、呼び出し元のコンテキストによって扱いが変化します。

#### コンテキスト定義

| コンテキスト種類 | 該当箇所 | 挙動 |
| :--- | :--- | :--- |
| **[1] 展開コンテキスト** | P-style の引数位置<br>S-style の `(...)` 内部 | 多値は **スタック上に展開されたまま** (Flattened) 引数リストに並びます。<br>`fn ret2()->1,2` に対し `add ret2` は `add 1 2` と解釈されます。 |
| **[2] タプル化コンテキスト** | C-style の `func(...)` 内部<br>配列リテラル `[...]` 内部 | 多値は自動的に **1つのタプル `(T1, T2)` にパック**されます。<br>`add(ret2)` は `add((1, 2))` となり、型不一致でエラーになる可能性があります。 |

#### スプレッド構文 (`...`)

*   **用途**: コンテキスト [2] において、タプル化された値を強制的に展開します。
*   **例**: `add(...ret2())` → `ret2` はタプル化されるが、`...` で展開され、`add(1, 2)` として成功します。

## 4. 実例集 (Examples)

### 4.1. 基本的な関数呼び出しスタイル

```mylang
fn add |a: i32, b: i32|->i32 $a + b$;

// 1. P-style (基本)
let a add 1 2;

// 2. S-style (グルーピング)
let b add 1 (add 2 3);

// 3. C-style (従来の呼び出し)
let c add(1, 2);
```

### 4.2. 高度な関数定義 (再帰・相互再帰・式指向)

```mylang
// 再帰と式指向のif (ブロックなし)
fn factorial |n: i32|->i32
    if $n <= 1$ 1
    else $n * factorial($n - 1$)$;

// 相互再帰 (fn は hoist されるため定義順序不問)
fn is_even |n: i32|->bool
    if $n == 0$ true else is_odd($n - 1$);

fn is_odd |n: i32|->bool
    if $n == 0$ false else is_even($n - 1$);

let fact5 factorial(5); // 120
let even10 is_even(10); // true
```

### 4.3. パイプ演算子 (>)

```mylang
fn double |n: i32|->i32 $n * 2$;
fn sub |a: i32, b: i32|->i32 $a - b$;

// 単純なパイプ
// 10 -> sub(10, 2) = 8 -> double(8) = 16
let res 10 > sub 2 > double;

// 左辺抽出と優先順位
// add 2 3 が先に評価され 5 になる -> add(5, 4) = 9 -> add(1, 9) = 10
let res2 add 1 add 2 3 > add 4;
```

### 4.4. 多値返却 (MRV) と混合スタイル

```mylang
fn pair ||->i32,i32 { 10 20 }
fn add3 |x: i32, y: i32, z: i32|->i32 $x + y + z$;

// P-style (展開コンテキスト)
// pair は 10, 20 に展開され、add3(5, 10, 20) となる
let v1 add3 5 pair;

// C-style (タプル化コンテキスト)
// pair() はタプルになるため、スプレッド ... で展開する
let v2 add3(5, ...pair());

// 混合スタイル
// add 1 [Result]
// [Result] = add(...[(10, 20)]) = add(10, 20) = 30
// add 1 30 = 31
let v3 add 1 add(...[pair()]);
```

