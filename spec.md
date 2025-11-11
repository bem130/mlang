### `mylang`コンパイラの設計：パーサーとコンパイラの協調プロセス

`mylang`のコンパイル処理は、大きく分けて**パーサー**と**コンパイラ**という2つの主要コンポーネントが連携して行います。それぞれの役割は明確に分離されており、これにより複雑な言語機能を堅牢に実装できます。

*   **パーサーの責務**: ソースコードを読み込み、その「**構文的な構造**」を解析する。意味的な正しさ（変数が定義されているか、型が合うか等）は一切問わない。
*   **コンパイラの責務**: パーサーが作った構造を元に、「**意味的な正しさ**」を検証し、最終的に実行可能なWasmコードを生成する。

処理は以下のパイプラインで進みます。

1.  **宣言収集 (Compiler Pre-pass)**: 全ての関数宣言を事前に収集。
2.  **構文解析 (Parser - 1st Pass)**: ソースコードを「未解決の構文木 (Raw AST)」に変換。
3.  **意味解析 (Compiler - 2nd Pass)**: Raw ASTを「検証済みの型付き構文木 (Typed AST)」に変換。
4.  **コード生成 (Compiler - Code Generation)**: Typed ASTをWATコードに変換。

---

### 1. パーサーの設計：構文の柔軟性を支えるルール

パーサーは、ソースコードの構造を解析し、意味解釈は後続のコンパイラに委ねることで、自身の役割を単純に保ちます。mylangの構文の柔軟性は、特に `()` の解釈ルールに集約されます。

#### 2種類の `()` とその区別

mylangには2種類の `()` が存在し、字句解析段階で隣接関係を判定したうえで専用のトークンを発行し、パーサー側はその違いを受け取って処理を分岐します。

> 実装では、レキサーが直前に読み取ったトークンが識別子やリテラルの一部だったかどうかを `last_char_was_ident_part` フラグで記録しています。直前が識別子相当で、かつ空白を挟まずに `(` が現れた場合は `Token::CallLParen` を生成し、それ以外は `Token::LParen` を生成します。これによりパーサーは Span を比較せずに `()` の種類を区別できます。

1.  **S式グループ `( ... )`**:
    *   **役割**: 式の評価順序を制御するためのグループ化。
    *   **ルール**: `(` の前に識別子がある場合でも、**間に空白が存在する**。
    *   **例**: `add (sub 5 3) 2`。これは `add sub 5 3 2` と意味的に等価。

2.  **C-style呼び出し `f(...)`**:
    *   **役割**: C言語風の明確な関数呼び出し。引数は `,` で区切られる。
    *   **ルール**: 関数名となる識別子と `(` が**ソースコード上で隣接している（間に空白がない）**。
    *   **例**: `add(5, 3)`。

このルールにより、`multiply (add 2 3) 4` のようなS式と `multiply(5, 4)` のようなC-style呼び出しを、パーサーは明確に区別して解析できます。

#### 構成

*   **LL(1)パーサー (メインパーサー)**: S式、C-style呼び出し、`fn`, `let`, `if` などの言語の基本構造を担当します。
*   **数式パーサー (Pratt Parser)**: `$$` ブロック内の中置記法 (`1 + 2 * 3`) のみを担当します。

---

### 2. ステップ1：構文解析 (1パス目)

この段階では、ソースコードを「**Raw Abstract Syntax Tree (Raw AST)**」に変換します。これは、まだ変数と関数呼び出しの区別がついていない、純粋な構造的データです。

**サンプルコード (`sample.mlang`)**
```mylang
fn main() {
    let result1 = add (sub 5 3) $1+2$;
    let result2 = add(sub 5 3, $1+2$);
}
```

#### `let` と可変変数

`let` 束縛は Rust に倣って **可変 (mutable)** か **不変 (immutable)** かを明示できます。既定は不変で、`let mut name = ...` のように `mut`
キーワードを挿入すると可変変数になります。意味解析では以下を保証します。

* 不変変数に対して `name = expr` 形式の代入文を記述するとコンパイルエラーになる。
* 代入文の右辺は変数の宣言時と同じ型でなければならない。
* 代入文自体は `()` 型（Unit）として扱われるため、ブロック内の文として使用する。

代入は式ではなく文としてのみ許可され、パーサーは `Identifier '='` の並びを文レベルで検出して `RawAstNode::Assignment` を生成します。意味解析段階で変数テーブルを引き当て、可変性と型を検証した上で `TypedExprKind::Assignment` へ変換します。

**Raw ASTのデータ構造 (Rustのイメージ)**

```rust
// file_ast.rs

pub enum RawAstNode {
    Expr(Vec<RawExprPart>),
    // ... FnDef, LetDef etc.
}

// 「式の構成要素」。まだ意味が確定していない。
pub enum RawExprPart {
    Token(Token, Span),                 // トークンそのもの
    Group(Vec<RawExprPart>, Span),      // S式グループ `( ... )`
    CStyleArgs(Vec<RawAstNode>, Span),  // C-style呼び出しの引数リスト `(...)`
    MathBlock(MathAstNode, Span),       // $$で囲まれた数式ブロック
    TypeAnnotation(String, Span),       // 型注釈 ": i32"
}
```

**`result1` のパース結果 (`add (sub 5 3) $1+2$`)**

`()` の前に空白があるため、`Group` として解釈されます。

```rust
// RawAstNode::Expr([
//   RawExprPart::Token(Ident("add"), Span{...}),
//   RawExprPart::Group(vec![ // S式グループ
//       RawExprPart::Token(Ident("sub"), Span{...}),
//       RawExprPart::Token(Number(5), Span{...}),
//       RawExprPart::Token(Number(3), Span{...}),
//   ], Span{...}),
//   RawExprPart::MathBlock( ... , Span{...}),
// ])
```

**`result2` のパース結果 (`add(sub 5 3, $1+2$)`)**

`add`と`(`が隣接しているため、`CStyleArgs` として解釈されます。

```rust
// RawAstNode::Expr([
//   RawExprPart::Token(Ident("add"), Span{...}),
//   RawExprPart::CStyleArgs(vec![ // C-style引数リスト
//       RawAstNode::Expr(vec![/* "sub 5 3" のパーツ */]),
//       RawAstNode::Expr(vec![/* "$1+2$" のパーツ */]),
//   ], Span{...}),
// ])
```
この時点では、`add` や `sub` が関数なのか、引数の数が正しいのかは一切関知しません。単に構文ルールに従って構造化するだけです。

---

### 3. ステップ2：意味解析と型チェック (2パス目)

ここからが**コンパイラ**の仕事です。1パス目で生成された `Raw AST` を入力として受け取り、意味を解釈して検証します。

#### 事前準備：宣言収集 (Pre-pass)

意味解析を始める前に、ソースコード全体を一度スキャンし、すべての関数 (`fn`) の名前、引数の型、戻り値の型、そして**定義された位置(Span)**を**シンボルテーブル**に登録します。

#### 意味解析とAST変換

コンパイラは `Raw AST` を辿りながら、それを「**Typed Abstract Syntax Tree (Typed AST)**」に変換します。このASTは、すべてのノードの意味が確定し、型情報が付与されたものです。

**Typed ASTのデータ構造 (Rustのイメージ)**

```rust
// file_compiler.rs
pub enum DataType { I32, F64, /* ... */ }

// 意味が確定し、型が付与されたノード
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub data_type: DataType,
    pub span: Span,
}

pub enum TypedExprKind {
    Literal(LiteralValue),
    VariableRef { name: String, unique_name: String },
    FunctionCall {
        name: String,
        args: Vec<TypedExpr>,
    },
    // ... IfExpr, LetBinding など
}
```

**解析プロセスの例**

1.  **名前解決**: `Raw AST` の先頭にある `Token(Ident("add"))` をシンボルテーブルで検索します。
    *   ルール: ローカル変数がグローバル関数に優先する。
    *   今回は `main` スコープに `add` 変数はないので、グローバル関数 `add` を見つけます。

2.  **引数解析と数チェック**: `add` トークンに続く `RawExprPart` の種類によって、引数の解釈方法が変わります。
    *   **S式の場合 (`Group` や `MathBlock` が続く）**:
        シンボルテーブルから関数 `add` が2つの引数を取ることが分かります。`Raw AST` のリストから、続く2つの要素 (`Group(...)` と `MathBlock(...)`) を引数として取り出し、再帰的に解析します。
    *   **C-styleの場合 (`CStyleArgs` が続く)**:
        `CStyleArgs` ノードが持つ引数のリスト (`Vec<RawAstNode>`) の要素数と、シンボルテーブルにある `add` 関数の引数の数を比較します。数が合わなければエラーを報告します。リストの各要素を再帰的に解析します。

3.  **型チェック**: 解析済みの各引数の型が、シンボルテーブルにある関数のシグネチャと一致するかを検証します。

4.  **Typed ASTの構築**: すべての検証が通れば、最終的な `TypedExprKind::FunctionCall` ノードを構築します。

---

### 4. 親切なエラーメッセージ

各ノードとシンボルが持つ `Span` (位置情報) を利用することで、どこで何が問題だったのかを正確にユーザーに伝えることができます。

**エラー例1: 未定義の識別子**
*   **コード**: `let x = foo 1 2`
*   **メッセージ**:
    ```
    error: sample.mlang:3:13: Undefined function or variable 'foo'
      |
    3 |     let x = foo 1 2
      |             ^^^
    ```

**エラー例2: 引数の数が違う**
*   **コード**: `let result = multiply (add 2 3) 4;` （もしパーサーにバグがあった場合）
*   **分析**: `multiply` の実引数が1つしか与えられていない。
*   **メッセージ**: エラー箇所に加え、`note:` によって関連情報（関数の定義場所）を提示する。
    ```
    error: sample.mlang:39:19: Function 'multiply' expects 2 arguments, but 1 were provided
      |
    39|     let result3 = multiply (add 2 3) 4;
      |                   ^^^^^^^^
      note: 'multiply' is defined here at line 9, column 1
    ```