//! # Sea Query Common Like
//!
//! `sea_query_common_like` is a collection of utilities to enhance [`sea_query`]
//! with typical `LIKE` search support, including escape sequences for patterns
//! (`%fuzzy%`, `prefix%`, `%suffix`) and multi-column fuzzy search.

#![cfg_attr(docsrs, feature(doc_cfg))]

use fancy_regex::Regex as FancyRegex;
use regex::Regex;
use sea_query::{Cond, Condition, Expr, IntoColumnRef, IntoLikeExpr, LikeExpr};
use std::sync::LazyLock;

#[cfg(feature = "with-sea-orm")]
use sea_orm::ColumnTrait;

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

/// A collection of [`Keyword`] for complex `LIKE` search conditions.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Keywords(Vec<Keyword>);

// Escape character used in `LIKE` search queries.
const ESCAPE_CHAR: char = '!';

// Regular expression to escape special characters in `LIKE` search patterns.
static ESCAPE_REGEX: LazyLock<FancyRegex> =
    LazyLock::new(|| FancyRegex::new(r"(?=[!_%])").unwrap());

// Regular expression to split input text into keywords based on whitespace.
static SEPARATOR_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\p{Zs}++").unwrap());

/// Create a prefix [`Keyword`] for `LIKE` search.
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
///         &FormatOptions::default(),
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
///         &FormatOptions::default(),
///     ),
/// );
/// ```
///
/// # Examples (`with-sea-orm`)
///
/// See [`fuzzy_separated`] examples.
pub fn prefix(text: impl Into<String>) -> Keyword {
    Keyword {
        ty: KeywordType::Prefix,
        value: text.into(),
    }
}

/// Create a suffix [`Keyword`] for `LIKE` search.
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
///         &FormatOptions::default(),
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
///         &FormatOptions::default(),
///     ),
/// );
/// ```
///
/// # Examples (`with-sea-orm`)
///
/// See [`fuzzy_separated`] examples.
pub fn suffix(text: impl Into<String>) -> Keyword {
    Keyword {
        ty: KeywordType::Suffix,
        value: text.into(),
    }
}

/// Create a fuzzy [`Keyword`] for `LIKE` search.
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
///         &FormatOptions::default(),
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
///         &FormatOptions::default(),
///     ),
/// );
/// ```
///
/// # Examples (`with-sea-orm`)
///
/// See [`fuzzy_separated`] examples.
pub fn fuzzy(text: impl Into<String>) -> Keyword {
    Keyword {
        ty: KeywordType::Fuzzy,
        value: text.into(),
    }
}

/// Split a single [`String`] by whitespace and create fuzzy [`Keywords`].
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
///         &FormatOptions::default(),
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
///         &FormatOptions::default(),
///     ),
/// );
/// ```
///
/// # Examples (`with-sea-orm`)
///
/// ```
/// use sea_query::all;
/// use sea_query_common_like::fuzzy_separated;
/// use sqlformat::{format, FormatOptions, QueryParams};
///
/// #[cfg(feature = "with-sea-orm")]
/// use sea_orm::{ColumnTrait, DbBackend, EntityTrait, QueryFilter, QueryTrait};
///
/// #[cfg(feature = "with-sea-orm")]
/// mod book {
///     use sea_orm::entity::prelude::*;
///
///     #[derive(Clone, Debug, PartialEq, DeriveEntityModel, Eq)]
///     #[sea_orm(schema_name = "book", table_name = "books")]
///     pub struct Model {
///         #[sea_orm(primary_key, auto_increment = false)]
///         pub id: Uuid,
///         #[sea_orm(column_type = "Text")]
///         pub title: String,
///         #[sea_orm(column_type = "Text")]
///         pub author: String,
///         pub deleted_at: Option<DateTimeWithTimeZone>,
///     }
///
///     #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
///     pub enum Relation {
///     }
///
///     impl ActiveModelBehavior for ActiveModel {}
/// }
///
/// #[cfg(feature = "with-sea-orm")]
/// assert_eq!(
///     format(
///         book::Entity::find()
///             .filter(all![
///                 fuzzy_separated("1% 99% Edison").into_condition_for_orm_columns([book::Column::Title, book::Column::Author]),
///                 book::Column::DeletedAt.is_null(),
///             ])
///             .build(DbBackend::Postgres)
///             .to_string()
///             .as_str(),
///         &QueryParams::default(),
///         &FormatOptions::default(),
///     ),
///     format(
///         r#"
///             SELECT
///                 "books"."id",
///                 "books"."title",
///                 "books"."author",
///                 "books"."deleted_at"
///             FROM
///                 "book"."books"
///             WHERE
///                 (
///                     "books"."title" LIKE '%1!%%' ESCAPE '!'
///                     OR "books"."author" LIKE '%1!%%' ESCAPE '!'
///                 )
///                 AND (
///                     "books"."title" LIKE '%99!%%' ESCAPE '!'
///                     OR "books"."author" LIKE '%99!%%' ESCAPE '!'
///                 )
///                 AND (
///                     "books"."title" LIKE '%Edison%' ESCAPE '!'
///                     OR "books"."author" LIKE '%Edison%' ESCAPE '!'
///                 )
///                 AND "books"."deleted_at" IS NULL
///         "#,
///         &QueryParams::default(),
///         &FormatOptions::default(),
///     ),
/// );
/// ```
pub fn fuzzy_separated(text: impl Into<String>) -> Keywords {
    keywords(
        SEPARATOR_REGEX
            .split(&text.into())
            .filter(|s| !s.is_empty())
            .map(fuzzy)
            .collect::<Vec<_>>(),
    )
}

/// Collect [`Keyword`] into [`Keywords`] for complex `LIKE` search conditions.
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
///         &FormatOptions::default(),
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
///         &FormatOptions::default(),
///     ),
/// );
/// ```
///
/// # Examples (`with-sea-orm`)
///
/// See [`fuzzy_separated`] examples.
pub fn keywords<T, Iter>(texts: Iter) -> Keywords
where
    T: Into<Keyword>,
    Iter: IntoIterator<Item = T>,
{
    Keywords(texts.into_iter().map(Into::into).collect())
}

// Escape special characters in a `LIKE` search pattern.
#[inline(always)]
fn escape_like_value(input: &str) -> String {
    ESCAPE_REGEX
        .replace_all(input, ESCAPE_CHAR.to_string())
        .to_string()
}

/// Default conversion from [`String`] to fuzzy [`Keyword`].
impl From<String> for Keyword {
    #[inline]
    fn from(value: String) -> Self {
        fuzzy(value)
    }
}

/// Default conversion from `&str` to fuzzy [`Keyword`].
impl From<&str> for Keyword {
    #[inline]
    fn from(value: &str) -> Self {
        fuzzy(value.to_string())
    }
}

/// Implement the conversion from [`Keyword`] to [`sea_query::LikeExpr`] for use in [`sea_query`].
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

/// Methods for converting [`Keyword`] into [`sea_query::Condition`] for a single or multiple columns.
impl Keyword {
    /// Generate a [`Condition`] for a single column with the `LIKE` pattern.
    #[inline]
    pub fn into_condition_for_column(self, column: impl IntoColumnRef) -> Condition {
        self.into_condition_for_columns([column])
    }

    /// Generate a [`Condition`] for multiple columns with the `LIKE` pattern.
    pub fn into_condition_for_columns<C, Iter>(self, columns: Iter) -> Condition
    where
        C: IntoColumnRef,
        Iter: IntoIterator<Item = C>,
    {
        columns
            .into_iter()
            .map(|col| Expr::col(col).like(self.clone()))
            .fold(Cond::all(), Cond::add)
    }

    /// Generate a [`Condition`] for a single column with the `LIKE` pattern using a fully-qualified column name with [`sea_orm`].
    #[cfg(feature = "with-sea-orm")]
    #[cfg_attr(docsrs, doc(cfg(feature = "with-sea-orm")))]
    #[inline]
    pub fn into_condition_for_orm_column(self, column: impl ColumnTrait) -> Condition {
        self.into_condition_for_orm_columns([column])
    }

    /// Generate a [`Condition`] for multiple columns with the `LIKE` pattern using fully-qualified column names with [`sea_orm`].
    #[cfg(feature = "with-sea-orm")]
    #[cfg_attr(docsrs, doc(cfg(feature = "with-sea-orm")))]
    #[inline]
    pub fn into_condition_for_orm_columns<C, Iter>(self, columns: Iter) -> Condition
    where
        C: ColumnTrait,
        Iter: IntoIterator<Item = C>,
    {
        self.into_condition_for_columns(columns.into_iter().map(|col| col.as_column_ref()))
    }
}

/// Methods for converting [`Keywords`] into [`sea_query::Condition`] for a single or multiple columns.
impl Keywords {
    /// Generate a [`Condition`] for a single column with multiple `LIKE` patterns.
    #[inline]
    pub fn into_condition_for_column(self, column: impl IntoColumnRef + Clone) -> Condition {
        self.into_condition_for_columns([column])
    }

    /// Generate a [`Condition`] for multiple columns with multiple `LIKE` patterns.
    pub fn into_condition_for_columns<C, Iter>(self, columns: Iter) -> Condition
    where
        C: IntoColumnRef + Clone,
        Iter: IntoIterator<Item = C>,
    {
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

    /// Generate a [`Condition`] for a single column with multiple `LIKE` patterns using a fully-qualified column name with [`sea_orm`].
    #[cfg(feature = "with-sea-orm")]
    #[cfg_attr(docsrs, doc(cfg(feature = "with-sea-orm")))]
    #[inline]
    pub fn into_condition_for_orm_column(self, column: impl ColumnTrait) -> Condition {
        self.into_condition_for_orm_columns([column])
    }

    /// Generate a [`Condition`] for multiple columns with multiple `LIKE` patterns using fully-qualified column names with [`sea_orm`].
    #[cfg(feature = "with-sea-orm")]
    #[cfg_attr(docsrs, doc(cfg(feature = "with-sea-orm")))]
    #[inline]
    pub fn into_condition_for_orm_columns<C, Iter>(self, columns: Iter) -> Condition
    where
        C: ColumnTrait,
        Iter: IntoIterator<Item = C>,
    {
        self.into_condition_for_columns(columns.into_iter().map(|col| col.as_column_ref()))
    }
}
