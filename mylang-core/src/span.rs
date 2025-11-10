//! ソースコード上の位置(行・列)を表現するためのデータ構造。

use core::fmt;

/// ソースコード上の位置を示す構造体
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

/// 2つのSpanを結合して、開始位置と終了位置を表現する (将来の拡張用)
pub fn combine_spans(start: Span, _end: Span) -> Span {
    // 今は開始位置を代表とする
    start
}