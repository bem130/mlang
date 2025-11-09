### `mylang`コンパイラの設計：パーサーとコンパイラの協調プロセス

`mylang`のコンパイル処理は、大きく分けて**パーサー**と**コンパイラ**という2つの主要コンポーネントが連携して行います。それぞれの役割は明確に分離されており、これにより複雑な言語機能を堅牢に実装できます。

*   **パーサーの責務**: ソースコードを読み込み、その「**構文的な構造**」を解析する。意味的な正しさ（変数が定義されているか、型が合うか等）は一切問わない。
*   **コンパイラの責務**: パーサーが作った構造を元に、「**意味的な正しさ**」を検証し、最終的に実行可能なWasmコードを生成する。

処理は以下のパイプラインで進みます。

1.  **宣言収集 (Compiler Pre-pass)**: 全ての関数・変数宣言を事前に収集。
2.  **構文解析 (Parser - 1st Pass)**: ソースコードを「未解決の構文木 (Raw AST)」に変換。
3.  **意味解析 (Compiler - 2nd Pass)**: Raw ASTを「検証済みの型付き構文木 (Typed AST)」に変換。
4.  **コード生成 (Compiler - Code Generation)**: Typed ASTをWATコードに変換。

---

### 1. パーサーの設計：2つのパーサーによる協調動作

パーサーは、ソースコードの構造を解析し、意味解釈は後続のコンパイラに委ねることで、自身の役割を単純に保ちます。

#### 構成

*   **LL(1)パーサー (メインパーサー)**: S式 (`add 1 2`)、`fn`, `let`, `if` などの言語の基本構造を担当します。
*   **数式パーサー (Pratt Parser)**: `$$` ブロック内の中置記法 (`1 + 2 * 3`) のみを担当します。

この2つは、ソースコードの構造に応じて互いを再帰的に呼び出し合います。

#### 位置情報の保持

親切なエラーメッセージを表示するため、**すべてのトークンと生成されるASTノードは、ソースコード上の行番号と列番号を保持します。** これを`Span`という構造体で管理します。

```rust
// file.rs
// ソースコード上の位置を示す構造体
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}
```

### 2. ステップ1：構文解析 (1パス目)

この段階では、ソースコードを「**Raw Abstract Syntax Tree (Raw AST)**」に変換します。これは、まだ変数と関数呼び出しの区別がついていない、構造的なデータです。

**サンプルコード (`sample.mylang`)**
```mylang
fn main() -> () {
    let result1 = add (sub 5 3): i32 $1+2$;
    let result2 = add(sub 5 3, $1+2$);
}
```

**Raw ASTのデータ構造 (Rustのイメージ)**

```rust
// file_parser.rs

// 「式の構成要素」。まだ意味が確定していない。
pub enum RawExprPart {
    Token(Token, Span), // トークンそのもの
    Group(Vec<RawExprPart>, Span), // ()で囲まれたグループ
    MathBlock(MathAstNode, Span), // $$で囲まれた数式ブロック
    TypeAnnotation(String, Span), // 型注釈 ": i32"
}

// 数式パーサーが生成する、より構造化されたAST
pub enum MathAstNode {
    // ... InfixOp, Number, など
    // 数式内での関数呼び出しは()が必須なので、この段階で区別できる
    Call {
        name: (String, Span),
        args: Vec<Vec<RawExprPart>>, // 引数部分は再びLL(1)パーサーが担当
        span: Span,
    },
}
```

**`let result = ...`行のパース結果**

LL(1)パーサーは `add (sub 5 3): i32 $1+2$` を見て、以下のようなフラットなリスト (`Vec<RawExprPart>`) を生成します。

```rust
// [
//   RawExprPart::Token(Ident("add"), Span{...}),
//   RawExprPart::Group(vec![
//       RawExprPart::Token(Ident("sub"), Span{...}),
//       RawExprPart::Token(Number(5.0), Span{...}),
//       RawExprPart::Token(Number(3.0), Span{...}),
//   ], Span{...}),
//   RawExprPart::TypeAnnotation("i32", Span{...}),
//   RawExprPart::MathBlock(
//       MathAstNode::InfixOp { /* 1 + 2 */ },
//       Span{...}
//   ),
// ]
```
この時点では、`add` や `sub` が関数なのか、引数の数が正しいのかは一切関知しません。単に「トークン」と「グループ」の並びとして構造化するだけです。

---

### 3. ステップ2：意味解析と型チェック (2パス目)

ここからが**コンパイラ**の仕事です。1パス目で生成された `Raw AST` を入力として受け取り、意味を解釈して検証します。

#### 事前準備：宣言収集 (Pre-pass)

意味解析を始める前に、ソースコード全体を一度スキャンし、すべての関数 (`fn`) と変数 (`let`) の名前を**シンボルテーブル**に登録します。これにより、2パス目の解析中に名前の存在を確認できます。

#### 意味解析とAST変換

コンパイラは `Raw AST` を辿りながら、それを「**Typed Abstract Syntax Tree (Typed AST)**」に変換します。このASTは、すべてのノードの意味が確定し、型情報が付与されたものです。

**Typed ASTのデータ構造 (Rustのイメージ)**

```rust
// file_compiler.rs
pub enum Type { I32, F64, /* ... */ }

// 意味が確定し、型が付与されたノード
pub struct TypedNode {
    pub kind: TypedNodeKind,
    pub return_type: Type,
    pub span: Span,
}

pub enum TypedNodeKind {
    Literal(f64),
    VariableRef(String),
    FunctionCall {
        name: String,
        args: Vec<TypedNode>,
    },
    // ... IfExpr, LetBinding など
}
```

**`[Token(Ident("add")), Group(...)]` の解析プロセス**

1.  **名前解決**: 先頭のトークン `add` をシンボルテーブルで検索します。
    *   ルール: ローカル変数がグローバル関数に優先する。
    *   今回は `main` スコープに `add` 変数はないので、グローバル関数 `add` を見つけます。

2.  **引数の数チェック**: シンボルテーブルから、関数 `add` が2つの引数を取ることが分かります。
    *   `Raw AST` のリストから、続く2つの要素 `Group(...)` と `MathBlock(...)` を引数として取り出します。
    *   もし要素の数が合わなければ、ここでエラーを報告します。

3.  **引数の再帰的解析**:
    *   1つ目の引数 `Group(...)` を再帰的に解析し、`sub` の関数呼び出しとして `TypedNode` を構築します。
    *   2つ目の引数 `MathBlock(...)` を解析し、`1+2` の結果の型を持つ `TypedNode` を構築します。

4.  **型チェック**:
    *   `add` 関数のシグネチャ（例: `(i32, i32) -> i32`）と、解析した各引数の `return_type` を比較します。
    *   最後に、式全体の結果の型と、型注釈 `: i32` が一致するかを検証します。

5.  **Typed ASTの構築**: すべての検証が通れば、最終的な `TypedNode` を構築します。

### 4. 親切なエラーメッセージ

各ノードが持つ `Span` (位置情報) を利用することで、どこで何が問題だったのかを正確にユーザーに伝えることができます。

**エラー例1: 未定義の識別子**
*   **コード**: `let x = foo 1 2`
*   **分析**: `foo` がシンボルテーブルに見つからない。
*   **メッセージ**:
    ```
    error: sample.mylang:3:13: 未定義の識別子 'foo' が見つかりました。
      |
    3 |     let x = foo 1 2
      |             ^^^
    ```

**エラー例2: 変数を関数として呼び出し**
*   **コード**: `let add = 10; let y = add 1 2`
*   **分析**: `add` は変数として解決されるが、後ろに引数が続いている。
*   **メッセージ**:
    ```
    error: sample.mylang:4:13: 変数 'add' は関数として呼び出すことはできません。
      |
    2 |     let add = 10;
      |         --- 'add' はここで変数として定義されています。
    ...
    4 |     let y = add 1 2
      |             ^^^
    ```

この設計により、柔軟で直感的な構文と、静的型付き言語の安全性を両立させることができます。パーサーは「形」、コンパイラは「意味」という明確な責務分離が、開発を容易にし、将来の拡張性を高めます。