//! # Sea Query Common Like
//!
//! `sea_query_common_like` is a collection of utilities to enhance `sea_query`
//! with typical `LIKE` search support, including escape sequences for patterns
//! (`%fuzzy%`, `%prefix`, `suffix%`) and multi-column fuzzy search.

use fancy_regex::Regex as FancyRegex;
use regex::Regex;
use sea_query::{Cond, Condition, Expr, IntoColumnRef, IntoLikeExpr, LikeExpr};
use std::sync::LazyLock;

/// Represents a keyword used for `LIKE` search with different matching types.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Keyword {
    ty: KeywordType,
    value: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum KeywordType {
    // prefix%
    Prefix,
    // %suffix
    Suffix,
    // %fuzzy%
    Fuzzy,
}

/// A collection of keywords for complex `LIKE` search conditions.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Keywords(Vec<Keyword>);

// Escape character used in `LIKE` search queries.
const ESCAPE_CHAR: char = '!';

// Regular expression to escape special characters in `LIKE` search patterns.
static ESCAPE_REGEX: LazyLock<FancyRegex> =
    LazyLock::new(|| FancyRegex::new(r"(?=[!_%])").unwrap());

// Regular expression to split input text into keywords based on whitespace.
static SEPARATOR_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\p{Zs}++").unwrap());

/// Create a prefix keyword for `LIKE` search.
///
/// # Examples
///
/// ```
/// use sea_query::{Expr, Iden, IntoLikeExpr, LikeExpr, PostgresQueryBuilder, SelectStatement};
/// use sea_query_common_like::prefix;
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BookSchema {
///     #[iden = "book"]
///     Schema,
/// }
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BooksTable {
///     #[iden = "books"]
///     Table,
///     Id,
///     Title,
///     Author,
///     DeletedAt
/// }
///
/// assert_eq!(
///     format(
///         SelectStatement::new()
///             .from((BookSchema::Schema, BooksTable::Table))
///             .columns([
///                 BooksTable::Id,
///                 BooksTable::Title,
///                 BooksTable::Author,
///             ])
///             .cond_where(prefix("Thomas").into_condition_for_column(BooksTable::Author))
///             .and_where(Expr::col(BooksTable::DeletedAt).is_null())
///             .take()
///             .to_string(PostgresQueryBuilder)
///             .as_str(),
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "id",
///                 "title",
///                 "author"
///             FROM
///                 "book"."books"
///             WHERE
///                 "author" LIKE 'Thomas%' ESCAPE '!'
///                 AND "deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
/// );
/// ```
pub fn prefix<T: Into<String>>(text: T) -> Keyword {
    Keyword {
        ty: KeywordType::Prefix,
        value: text.into(),
    }
}

/// Create a suffix keyword for `LIKE` search.
///
/// ```
/// use sea_query::{Expr, Iden, IntoLikeExpr, LikeExpr, PostgresQueryBuilder, SelectStatement};
/// use sea_query_common_like::suffix;
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BookSchema {
///     #[iden = "book"]
///     Schema,
/// }
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BooksTable {
///     #[iden = "books"]
///     Table,
///     Id,
///     Title,
///     Author,
///     DeletedAt
/// }
///
/// assert_eq!(
///     format(
///         SelectStatement::new()
///             .from((BookSchema::Schema, BooksTable::Table))
///             .columns([
///                 BooksTable::Id,
///                 BooksTable::Title,
///                 BooksTable::Author,
///             ])
///             .cond_where(suffix("99% perspiration!").into_condition_for_column(BooksTable::Title))
///             .and_where(Expr::col(BooksTable::DeletedAt).is_null())
///             .take()
///             .to_string(PostgresQueryBuilder)
///             .as_str(),
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "id",
///                 "title",
///                 "author"
///             FROM
///                 "book"."books"
///             WHERE
///                 "title" LIKE '%99!% perspiration!!' ESCAPE '!'
///                 AND "deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
/// );
/// ```
pub fn suffix<T: Into<String>>(text: T) -> Keyword {
    Keyword {
        ty: KeywordType::Suffix,
        value: text.into(),
    }
}

/// Create a fuzzy keyword for `LIKE` search.
///
/// # Examples
///
/// ```
/// use sea_query::{Expr, Iden, IntoLikeExpr, LikeExpr, PostgresQueryBuilder, SelectStatement};
/// use sea_query_common_like::fuzzy;
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BookSchema {
///     #[iden = "book"]
///     Schema,
/// }
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BooksTable {
///     #[iden = "books"]
///     Table,
///     Id,
///     Title,
///     Author,
///     DeletedAt
/// }
///
/// assert_eq!(
///     format(
///         SelectStatement::new()
///             .from((BookSchema::Schema, BooksTable::Table))
///             .columns([
///                 BooksTable::Id,
///                 BooksTable::Title,
///                 BooksTable::Author,
///             ])
///             .cond_where(fuzzy("99%").into_condition_for_column(BooksTable::Title))
///             .and_where(Expr::col(BooksTable::DeletedAt).is_null())
///             .take()
///             .to_string(PostgresQueryBuilder)
///             .as_str(),
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "id",
///                 "title",
///                 "author"
///             FROM
///                 "book"."books"
///             WHERE
///                 "title" LIKE '%99!%%' ESCAPE '!'
///                 AND "deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
/// );
/// ```
pub fn fuzzy<T: Into<String>>(text: T) -> Keyword {
    Keyword {
        ty: KeywordType::Fuzzy,
        value: text.into(),
    }
}

/// Split a single string by whitespace and create a collection of fuzzy keywords.
///
/// # Examples
///
/// ```
/// use sea_query::{Expr, Iden, IntoLikeExpr, LikeExpr, PostgresQueryBuilder, SelectStatement};
/// use sea_query_common_like::fuzzy_separated;
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BookSchema {
///     #[iden = "book"]
///     Schema,
/// }
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BooksTable {
///     #[iden = "books"]
///     Table,
///     Id,
///     Title,
///     Author,
///     DeletedAt
/// }
///
/// assert_eq!(
///     format(
///         SelectStatement::new()
///             .from((BookSchema::Schema, BooksTable::Table))
///             .columns([
///                 BooksTable::Id,
///                 BooksTable::Title,
///                 BooksTable::Author,
///             ])
///             .cond_where(fuzzy_separated("1% 99% Edison").into_condition_for_columns([BooksTable::Title, BooksTable::Author]))
///             .and_where(Expr::col(BooksTable::DeletedAt).is_null())
///             .take()
///             .to_string(PostgresQueryBuilder)
///             .as_str(),
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "id",
///                 "title",
///                 "author"
///             FROM
///                 "book"."books"
///             WHERE
///                 (
///                     "title" LIKE '%1!%%' ESCAPE '!'
///                     OR "author" LIKE '%1!%%' ESCAPE '!'
///                 )
///                 AND (
///                     "title" LIKE '%99!%%' ESCAPE '!'
///                     OR "author" LIKE '%99!%%' ESCAPE '!'
///                 )
///                 AND (
///                     "title" LIKE '%Edison%' ESCAPE '!'
///                     OR "author" LIKE '%Edison%' ESCAPE '!'
///                 )
///                 AND "deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
/// );
/// ```
pub fn fuzzy_separated<T: Into<String>>(text: T) -> Keywords {
    keywords(
        SEPARATOR_REGEX
            .split(&text.into())
            .filter(|s| !s.is_empty())
            .map(fuzzy)
            .collect::<Vec<_>>(),
    )
}

/// Create a collection of keywords for complex `LIKE` search conditions.
///
/// # Examples
///
/// ```
/// use sea_query::{Expr, Iden, IntoLikeExpr, LikeExpr, PostgresQueryBuilder, SelectStatement};
/// use sea_query_common_like::{fuzzy, keywords, prefix};
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BookSchema {
///     #[iden = "book"]
///     Schema,
/// }
///
/// #[derive(Clone, Debug, Iden)]
/// pub enum BooksTable {
///     #[iden = "books"]
///     Table,
///     Id,
///     Title,
///     Author,
///     DeletedAt
/// }
///
/// assert_eq!(
///     format(
///         SelectStatement::new()
///             .from((BookSchema::Schema, BooksTable::Table))
///             .columns([
///                 BooksTable::Id,
///                 BooksTable::Title,
///                 BooksTable::Author,
///             ])
///             .cond_where(keywords([fuzzy("99%"), prefix("Thomas")]).into_condition_for_columns([BooksTable::Title, BooksTable::Author]))
///             .and_where(Expr::col(BooksTable::DeletedAt).is_null())
///             .take()
///             .to_string(PostgresQueryBuilder)
///             .as_str(),
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "id",
///                 "title",
///                 "author"
///             FROM
///                 "book"."books"
///             WHERE
///                 (
///                     "title" LIKE '%99!%%' ESCAPE '!'
///                     OR "author" LIKE '%99!%%' ESCAPE '!'
///                 )
///                 AND (
///                     "title" LIKE 'Thomas%' ESCAPE '!'
///                     OR "author" LIKE 'Thomas%' ESCAPE '!'
///                 )
///                 AND "deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         FormatOptions::default(),
///     ),
/// );
/// ```
pub fn keywords<T: Into<Keyword>, Iter: IntoIterator<Item = T>>(texts: Iter) -> Keywords {
    Keywords(texts.into_iter().map(Into::into).collect())
}

// Escape special characters in a `LIKE` search pattern.
fn escape_like_value(input: &str) -> String {
    ESCAPE_REGEX
        .replace_all(input, ESCAPE_CHAR.to_string())
        .to_string()
}

/// Default conversion from `String` to fuzzy Keyword.
impl From<String> for Keyword {
    fn from(value: String) -> Self {
        fuzzy(value)
    }
}

/// Default conversion from `&str` to fuzzy Keyword.
impl From<&str> for Keyword {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

/// Implement the conversion from Keyword to `sea_query::LikeExpr` for use in `sea_query`.
impl IntoLikeExpr for Keyword {
    fn into_like_expr(self) -> LikeExpr {
        LikeExpr::new(match self.ty {
            KeywordType::Prefix => [&escape_like_value(&self.value), "%"].join(""),
            KeywordType::Suffix => ["%", &escape_like_value(&self.value)].join(""),
            KeywordType::Fuzzy => ["%", &escape_like_value(&self.value), "%"].join(""),
        })
        .escape(ESCAPE_CHAR)
    }
}

/// Methods for converting `Keyword` into `sea_query::Condition` for a single or multiple columns.
impl Keyword {
    /// Generate a `Condition` for a single column with the `LIKE` pattern.
    pub fn into_condition_for_column<T: IntoColumnRef>(self, column: T) -> Condition {
        self.into_condition_for_columns([column])
    }

    /// Generate a `Condition` for multiple columns with the `LIKE` pattern.
    pub fn into_condition_for_columns<T: IntoColumnRef, Iter: IntoIterator<Item = T>>(
        self,
        columns: Iter,
    ) -> Condition {
        columns
            .into_iter()
            .map(|col| Expr::col(col).like(self.clone()))
            .fold(Cond::all(), Cond::add)
    }
}

/// Methods for converting `Keywords` into `sea_query::Condition` for a single or multiple columns.
impl Keywords {
    /// Generate a `Condition` for a single column with multiple `LIKE` patterns.
    pub fn into_condition_for_column<T: IntoColumnRef + Clone>(self, column: T) -> Condition {
        self.into_condition_for_columns([column])
    }

    /// Generate a `Condition` for multiple columns with multiple `LIKE` patterns.
    pub fn into_condition_for_columns<T: IntoColumnRef + Clone, Iter: IntoIterator<Item = T>>(
        self,
        columns: Iter,
    ) -> Condition {
        let columns = columns.into_iter().collect::<Vec<_>>();
        self.0
            .into_iter()
            .map(|keyword| {
                columns
                    .iter()
                    .map(|col| Expr::col(col.clone()).like(keyword.clone()))
                    .fold(Cond::any(), Cond::add)
            })
            .fold(Cond::all(), Cond::add)
    }
}
