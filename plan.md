# mylang – Refinement Types, Overloading, Traits, and Mini SMT Solver

## Implementation Plan Document

---

## 1. Goals and Scope

### 1.1 High-level goals

We want to extend **mylang** with:

1. **Function overloading**

   * Multiple `fn` with the same name, differentiated by parameter and/or return types.

2. **Generics and Rust-like traits**

   * Type variables `<T>` and trait bounds `T: Trait`.
   * Trait-based overloading (e.g. `Cast`-style conversions).

3. **Refinement Types**

   * Types of the form `<p: T | predicate>` where `predicate` is a boolean expression.
   * Used for:

     * Range-bounded integers.
     * Non-empty `Vec`, etc.
     * Eventually more complex properties.

4. **Simple self-written SMT-style solver**

   * Entirely implemented in Rust initially, but simple enough to port to mylang.
   * Used to check refinement implications and subtyping.

5. **Type inference + narrowing**

   * Overload resolution guided by inference.
   * Control-flow-based narrowing (especially with refinements and pattern matching).

6. **Standard-library style `cast`**

   * Trait-based `Cast<Target>` and a generic function `cast`:

     ```mylang
     (cast 5): string
     (cast 5): f64
     ```

---

## 2. Specification

### 2.1 Core type system

Existing `DataType` will be extended.

#### 2.1.1 DataType

Current variants (simplified):

```rust
enum DataType {
    I32,
    F64,
    Bool,
    String,
    Unit,
    Vector(Box<DataType>),
    Tuple(Vec<DataType>),
    Struct(String),
    Enum(String),
    Function {
        params: Vec<DataType>,
        return_type: Box<DataType>,
    },
    // NEW:
    TypeVar(String), // T, U, ...
    Refined {
        base: Box<DataType>,      // T
        // syntactic pattern; for type-level we can store just name or structured pattern
        binder: String,           
        // pointer to refinement predicate (typed AST or internal ID)
        predicate_id: RefinementId,
    },
}
```

Notes:

* `Refined` is the type-level representation of `<p: T | predicate>`.
* At type-checking time, we will carry an environment mapping `RefinementId` to predicate ASTs.

---

### 2.2 Function overloading

#### 2.2.1 Function signatures

```rust
struct FunctionSignature {
    name: String,
    type_params: Vec<String>,          // ["T", "U", ...]
    param_types: Vec<DataType>,        // after parsing and type resolution
    return_type: DataType,
    trait_bounds: Vec<TraitBound>,     // e.g. T: Numeric
    definition_span: Span,
}
```

The **function table**:

```rust
type FunctionTable = BTreeMap<String, Vec<FunctionSignature>>;
```

* One function name → **multiple** signatures (overloads).
* Overloads are allowed when `(param_types, return_type)` differ.

#### 2.2.2 Overload resolution

For a call `f arg1 arg2 ...`:

1. Collect `candidates = function_table["f"]` (or error if none).
2. Filter by **arity**:

   * Keep only `candidate` where `candidate.param_types.len() == args.len()`.
3. For each candidate:

   * Instantiate type parameters with fresh type variables.
   * Attempt to **unify** argument types with parameter types using subtyping rules (see 2.5).
   * If there is an expected return type (from context), also unify result.
   * If unification fails, record the failure reason for this candidate and discard it.
4. After filtering:

   * `0` candidates → **no matching overload** error (with reasons).
   * `1` candidate → success.
   * `>= 2` candidates → **ambiguous overload** error.

---

### 2.3 Generics and traits

#### 2.3.1 Traits

Trait syntax:

```mylang
trait Numeric {
    fn zero | |->Self;
    fn add  |lhs: Self, rhs: Self|->Self;
    fn lt   |lhs: Self, rhs: Self|->bool;
}

trait Cast<Target> {
    fn cast |x: Self|->Target;
}
```

* `Self` refers to the implementing type.
* Traits can have type parameters (like `Cast<Target>`).

Internal representation (rough sketch):

```rust
struct TraitDef {
    name: String,
    type_params: Vec<String>,           // e.g. ["Target"]
    methods: Vec<FunctionSignature>,    // trait method signatures
}

struct ImplDef {
    trait_name: String,
    trait_params: Vec<DataType>,        // e.g. [DataType::String] for Cast<string>
    self_type: DataType,                // e.g. DataType::I32
    methods: Vec<FunctionSignature>,    // concrete implementations
}
```

#### 2.3.2 Trait bounds on generics

Function definition example:

```mylang
fn bounded<T: Numeric> |x: T|->T { ... }
```

Type-level:

```rust
struct TraitBound {
    type_param: String,      // "T"
    trait_name: String,      // "Numeric"
    trait_args: Vec<DataType>, // usually empty for simple traits
}
```

At instantiation `T = ConcreteType`, we must verify:

* There exists an `ImplDef` for `(Numeric, [], ConcreteType)`.

---

### 2.4 Refinement types

#### 2.4.1 Syntax

We adopt:

```mylang
<p: T | predicate_expr>
```

where:

* `p` is a **pattern** (minimal version: identifier; later, tuples and enum patterns).
* `T` is any type (including type variables).
* `predicate_expr` is a boolean expression in mylang:

  * Usually a `$...$` math block
  * Or more generally a pure expression of type `bool`.

Examples:

```mylang
// Simple integer refinement
type Positive = <x: i32 | $x > 0$>;

// Non-negative index
type Index<N> = <i: i32 | $0 <= i && i < N$>;

// Non-empty Vec
type NonEmptyVec<T> = <v: Vec<T> | $vec_len(v) > 0$>;
```

#### 2.4.2 Semantics (informal)

* `<p: T | φ(p)>` is the set of values of type `T` that satisfy boolean predicate `φ`.
* Refinement subtyping:

  * `<x: T | φ1>` is a subtype of `<x: T | φ2>` iff `φ1 ⇒ φ2` (for all `x`).
  * `<x: T | φ>` is always a subtype of `T` (we can drop constraints).
  * `T` is **not** automatically a subtype of `<x: T | φ>`.

These implications are decided by the SMT mini-solver.

---

### 2.5 Subtyping and type relations

We define a subtyping relation `⊑` (read “is a subtype of”), focusing on:

* **Base types**:

  * For now: `T ⊑ U` iff `T == U` structurally (no numeric promotions, no unions yet).

* **Refinements**:

  1. `<x: T | φ> ⊑ T`
     always holds (refinement erasure).

  2. `<x: T | φ1> ⊑ <x: T | φ2>`
     iff the SMT solver confirms `φ1 ⇒ φ2` valid.

  3. For containers:

     * `(Refined base) ⊑ (Base)` via rule (1).
     * No variance for generic constructors initially:

       * `Vec<T1> ⊑ Vec<T2>` only if `T1 == T2` (or you explicitly erase refinements).

This subtyping is used in:

* Function call checking (argument type ⊑ parameter type).
* Return type checking (actual return type ⊑ declared return type).
* Type narrowing.

---

### 2.6 Type inference and narrowing

#### 2.6.1 Type inference

High-level:

* Each expression gets a type, possibly with **type variables**.
* Constraints arise from:

  * Literals (`5: i32`, `0.5: f64`).
  * Function calls (param/result types).
  * Conditionals (`if`, `match`).
  * Annotations (e.g. `(expr): T`).
* A unification engine solves constraints, producing a substitution for type variables.

We use the expected type (from context) to:

* Help resolve overloads.
* Choose `Target` for `cast`.

#### 2.6.2 Type narrowing

We use an environment mapping variable names to:

```rust
struct TypeInfo {
    base: DataType,           // e.g. i32, Vec<T>, etc.
    refinement: PredicateId,  // conjunction of predicates about this var
}
```

* **Pattern-based narrowing**:

  * `match` arms refine the scrutinee based on patterns.
  * Variables bound in patterns get precise types (enum payloads, tuple fields).

* **Condition-based narrowing**:

  * In `if $x > 0$ { ... } else { ... }`:

    * Then-branch environment: refine `x` with `x > 0`.
    * Else-branch: refine with `x <= 0` (negation).
  * These refinements are stored as extra predicates and used by the SMT solver.

First version:

* Narrow only **inside each branch**; after the branch, drop extra refinements (keep base type).
* Later, we can add global refinement merging.

---

### 2.7 `cast` design

Standard-library trait:

```mylang
trait Cast<Target> {
    fn cast |x: Self|->Target;
}
```

Standard helper function:

```mylang
fn cast<T, Target: Cast<Target>> |x: T|->Target {
    Cast.cast(x)
}
```

User code:

```mylang
impl Cast<string> for i32 {
    fn cast |x: Self|->string i32_to_string(x);
}

impl Cast<f64> for i32 {
    fn cast |x: Self|->f64 i32_to_f64(x);
}

let s (cast 5): string;   // chooses Target = string
let d (cast 5): f64;      // chooses Target = f64
```

Resolution:

* `cast` is a generic function.
* Type inference + trait resolution chooses the correct `Cast` implementation based on:

  * Argument type (`T`).
  * Expected return type (`Target`).

---

### 2.8 Mini SMT solver specification

#### 2.8.1 Logic fragment v1

Sorts:

* `Int` (models `i32`).
* `Bool`.

Atomic integer constraints:

* `x ≥ c`
* `x ≤ c`
* `x = c`
* (later) `x ≥ y + c`, `x ≤ y + c`

Formula structure:

* Conjunction `φ1 && φ2 && ...`
* Negation `! atom` (negation only directly on atoms).
* No general `||` in v1.

#### 2.8.2 Internal structures

Variables:

```rust
struct IntVarId(usize);
```

Atoms:

```rust
enum IntAtom {
    GeConst { var: IntVarId, c: i64 }, // x >= c
    LeConst { var: IntVarId, c: i64 }, // x <= c
    EqConst { var: IntVarId, c: i64 }, // x == c
    // future: GeVar, LeVar
}

enum BoolAtom {
    Int(IntAtom),
}

enum Literal {
    Pos(BoolAtom),
    Neg(BoolAtom),
}

struct ConstraintSet {
    literals: Vec<Literal>, // conjunction
}
```

Intervals:

```rust
struct Interval {
    min: i64, // use i64::MIN as -∞
    max: i64, // use i64::MAX as +∞
}

struct IntEnv {
    domains: Vec<Interval>, // one per IntVarId
}
```

#### 2.8.3 Solving

For **conjunctions**:

* Start with all intervals = `(-∞, +∞)`.
* Repeatedly apply each literal:

  * For `Pos(IntAtom::GeConst { var, c })`, update `dom[var].min = max(min, c)`.
  * For `Pos(IntAtom::LeConst { var, c })`, update `dom[var].max = min(max, c)`.
  * For `Pos(IntAtom::EqConst { var, c })`, set both bounds to `c`.
  * For `Neg(atom)`, map to equivalent positive constraint, e.g.:

    * `Neg(GeConst { var, c })` ⇒ `LeConst { var, c: c-1 }`.
    * `Neg(LeConst { var, c })` ⇒ `GeConst { var, c: c+1 }`.
* If any interval becomes empty (`min > max`) → UNSAT.
* If no more changes and all intervals non-empty → SAT.

Subtyping check `<x: T | φ1> ⊑ <x: T | φ2>`:

1. Build `ConstraintSet` for `φ1 ∧ ¬φ2`.
2. Run the solver:

   * If UNSAT → subtyping holds.
   * If SAT → subtyping does **not** hold.

---

## 3. Important Test Cases

### 3.1 Function overloading

```mylang
fn add |a: i32, b: i32|->i32 $a + b$;
fn add |a: f64, b: f64|->f64 $a + b$;

fn main | |->() {
    let x add 1 2;        // uses (i32, i32) -> i32
    let y add 0.1 0.2;    // uses (f64, f64) -> f64
}
```

* Expect: type checks, correct overload chosen.

Error case:

```mylang
fn add |a: i32, b: i32|->i32 $a + b$;
fn main | |->() {
    let x add 1 "hello";
}
```

* Expect: error

  * `no applicable overload for 'add' with argument types (i32, string)`
  * List candidate signatures and show mismatch for argument 2.

---

### 3.2 Generics and traits

```mylang
trait Numeric {
    fn zero | |->Self;
    fn add  |lhs: Self, rhs: Self|->Self;
}

impl Numeric for i32 {
    fn zero | |->Self 0;
    fn add  |lhs: Self, rhs: Self|->Self $lhs + rhs$;
}

fn sum_two<T: Numeric> |x: T, y: T|->T {
    Numeric.add(x, y)
}

fn main | |->() {
    let a sum_two 3 4;         // T = i32
}
```

* Expect: `sum_two` type checks, call works with `i32` since `i32: Numeric`.

Error case (missing impl):

```mylang
struct Foo { value: i32 }

fn main | |->() {
    let f Foo { value: 1 };
    let g sum_two f f;  // T = Foo, but no impl Numeric for Foo
}
```

* Expect: error: `Foo` does not implement `Numeric`.

---

### 3.3 Refinement subtyping with SMT

Positive vs non-zero:

```mylang
type Positive = <x: i32 | $x > 0$>;
type NonZero  = <x: i32 | $x != 0$>;

fn takes_nonzero |x: NonZero|->i32 x;

fn main | |->() {
    let p: Positive = 5;
    takes_nonzero(p);  // Positive <: NonZero?
}
```

* Internally:

  * `φ1: x > 0` → `x >= 1`
  * `φ2: x != 0`
  * Check `x >= 1 ∧ !(x != 0)` = `x >= 1 ∧ x == 0` → UNSAT.
* Expect: accepted.

Failure case:

```mylang
type Positive = <x: i32 | $x > 0$>;
type GreaterTen = <x: i32 | $x > 10$>;

fn takes_gt_ten |x: GreaterTen|->i32 x;

fn main | |->() {
    let p: Positive = 5;
    takes_gt_ten(p);  // Positive <: GreaterTen ?
}
```

* `φ1: x > 0` → `x >= 1`
* `φ2: x > 10` → `x >= 11`
* Check `x >= 1 ∧ !(x >= 11)` = `x >= 1 ∧ x <= 10` → SAT (`x=5`).
* Expect: subtyping fails → type error.

---

### 3.4 Non-empty Vec with refinement

```mylang
type NonEmptyVec<T> = <v: Vec<T> | $vec_len(v) > 0$>;

fn head<T> |xs: NonEmptyVec<T>|->T {
    vec_get(xs, 0)
}

fn main | |->() {
    let xs: NonEmptyVec<i32> = make_nonempty([1, 2, 3]);
    let x head(xs);  // OK
}
```

* Even without full SMT support for arrays, we at least:

  * Accept type definitions.
  * Use `NonEmptyVec<T>` as arguments to `Vec<T>` functions via refinement erasure.

Later, with more SMT support, we can prove `vec_get(xs, 0)` safe under refinement.

---

### 3.5 `cast` behavior

```mylang
trait Cast<Target> {
    fn cast |x: Self|->Target;
}

impl Cast<string> for i32 {
    fn cast |x: Self|->string i32_to_string(x);
}

impl Cast<f64> for i32 {
    fn cast |x: Self|->f64 i32_to_f64(x);
}

fn cast<T, Target: Cast<Target>> |x: T|->Target {
    Cast.cast(x)
}

fn main | |->() {
    let s (cast 5): string;  // i32 -> string
    let d (cast 5): f64;     // i32 -> f64

    let x cast 5;            // error: ambiguous call to `cast`
}
```

* Expect:

  * First two calls type-check.
  * Third call: error, suggesting `: string` or `: f64`.

---

## 4. Implementation Plan (Step-by-step)

A staged roadmap that you can implement incrementally.

---

### Stage 0 – Preparation

**Goals:**

* Cleanly separate:

  * Type representation (`DataType`).
  * Function signatures (`FunctionSignature`).
  * Trait / impl structures.

**Tasks:**

1. Ensure `DataType` is the single canonical type representation.
2. Add `TypeVar(String)` to `DataType`.
3. Clean up existing `string_to_type` / type parsing in preparation for more complex types (no behavior change yet).

---

### Stage 1 – Basic function overloading (no generics)

**Goals:**

* Multiple `fn` with same name but different monomorphic types.
* Arity + exact-type based resolution.
* Good error messages.

**Tasks:**

1. Change `function_table` to `BTreeMap<String, Vec<FunctionSignature>>`.
2. Remove “duplicate function name” error; allow multiple entries.
3. Implement overload resolution:

   * Filter by arity.
   * Filter by argument type equality (no subtyping yet).
4. Implement error reporting:

   * When no candidate matches: show candidates + mismatch reasons.
   * When ambiguous (multiple exact matches; rare in this stage), error.

**Tests:** see 3.1.

---

### Stage 2 – Generics and TypeVars (no traits yet)

**Goals:**

* Support parametric polymorphism (`fn length<T> |xs: Vec<T>|->i32`).
* Use type variables in `DataType`.

**Tasks:**

1. Extend parser to accept generic params:

   * `fn name<T, U> |...|->...`.
2. Store `type_params: Vec<String>` in `FunctionSignature`.
3. Update type checker:

   * On call, create fresh type variables for each `T`.
   * Unify `param_types` with `arg_types` to solve type variables.
4. Implement simple unification over `DataType` with `TypeVar`.

---

### Stage 3 – Traits and trait bounds

**Goals:**

* Basic trait definitions and `impl` blocks.
* Use trait bounds in generic signatures.

**Tasks:**

1. Add AST nodes & data structures for:

   * `TraitDef`
   * `ImplDef`
   * `TraitBound`
2. Parser:

   * `trait TraitName<T, ...> { fn ... }`
   * `impl TraitName<...> for Type { fn ... }`
3. Type checking:

   * For each `impl`, check that all required methods are present and match signatures.
   * When instantiating a generic function with `T: Trait`, ensure an `impl Trait for T` exists.

**Tests:** see 3.2.

---

### Stage 4 – Refinement types (syntax + type-level only)

**Goals:**

* Parse `<p: T | predicate>` into `DataType::Refined`.
* Wire refinements into types, but without SMT yet.

**Tasks:**

1. Extend type parser:

   * If a type starts with `<`, parse refinement:

     * Pattern `p`
     * `:`
     * Base type `T`
     * `|`
     * Predicate expression (as `RawAstNode` / `MathAstNode`).
2. In type resolution:

   * Convert base type `T` to `DataType`.
   * Create `Refined` entry with a fresh `RefinementId` pointing to predicate.
3. Allow refinements in:

   * Function params and return types.
   * `type` aliases.

At this stage, no SMT checks; refinements behave as distinct types that only subtyping rule is:

* `<x: T | φ> ⊑ T` (erasure).

---

### Stage 5 – Mini SMT solver v1 (conjunctions only)

**Goals:**

* Implement the interval-based solver for conjunctions of integer constraints.
* Use it to check simple refinement subtyping.

**Tasks:**

1. Implement logical AST:

   * `IntVarId`, `IntAtom`, `Literal`, `ConstraintSet`, `IntEnv`, `Interval`.
2. Implement `apply_atom` and fixed-point propagation:

   * Return UNSAT when intervals become inconsistent.
3. Implement predicate translation:

   * From typed predicate AST to `ConstraintSet`.
   * Restrict to `&&`, `!`, and integer comparisons.
4. Implement subtyping check:

   * For `<x: i32 | φ1> ⊑ <x: i32 | φ2>`:

     * Build constraints for `φ1` and `¬φ2`.
     * Ask solver if UNSAT.

**Tests:** see 3.3.

---

### Stage 6 – Integrate refinements with function calls and narrowing

**Goals:**

* Use subtyping with refinements in:

  * Function call type checking.
  * Return type checking.
* Start basic narrowing in `if` statements.

**Tasks:**

1. Update function call checking:

   * When `arg_type` and `param_type` are refinements or base types, use subtyping rules (with SMT).
2. Update return checking:

   * Ensure actual return type ⊑ declared return type for refined returns.
3. Narrowing:

   * For `if $x > 0$ { ... } else { ... }`:

     * In then-branch, add refinement `x > 0`.
     * In else-branch, add `x <= 0`.
   * Use refined env when type-checking operations like indexing (future).

---

### Stage 7 – `cast` and trait-based conversions

**Goals:**

* Implement `Cast<Target>` trait and `cast` function in the standard library.
* Integrate with overload resolution and type inference.

**Tasks:**

1. Define `Cast<Target>` trait and a few `impl`s in library.
2. Implement generic `fn cast<T, Target: Cast<Target>> |x: T|->Target`.
3. Ensure the expected return type is used in overload resolution:

   * `(cast 5): string` and `(cast 5): f64` pick different `Cast` impls.
4. Add ambiguity error when no expected return type exists.

**Tests:** see 3.5.

---

### Stage 8 – Optional: add OR (`||`) and tiny DPLL-style solver

**Goals:**

* Support more expressive logical formulas in refinements.
* Add a simple OR-handling algorithm on top of the interval solver.

**Tasks:**

1. Add `Or` and `And` nodes to a formula AST.
2. Implement recursive SAT check:

   * Use the interval solver as a theory solver.
   * Backtrack over OR branches.
3. Gradually allow `||` inside refinements.

---

This plan should give you a clear roadmap from your current compiler to:

* Overloading
* Generics & traits
* Refinement types
* A small, fully self-written SMT-style solver
* And eventually, a type system powerful enough to justify the effort of self-hosting in mylang itself.
