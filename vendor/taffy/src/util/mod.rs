//! Helpful misc. utilities such as a function to debug print a tree
pub mod layout_abort;
mod math;
mod resolve;
pub(crate) mod sys;

/// Abort the current Taffy layout computation (see [`layout_abort::abort_now`]).
pub use layout_abort::abort_now as abort_layout_now;
pub use math::MaybeMath;
#[cfg(feature = "std")]
pub(crate) use layout_abort::{with_layout_abort, LayoutAbort};
pub(crate) use layout_abort::check_layout_abort;
pub use resolve::{MaybeResolve, ResolveOrZero};

#[doc(hidden)]
#[macro_use]
pub(crate) mod debug;

#[cfg(feature = "std")]
mod print;
#[cfg(feature = "std")]
pub use print::print_tree;

/// Deserialize a type `S` by deserializing a string, then using the `FromStr`
/// impl of `S` to create the result. The generic type `S` is not required to
/// implement `Deserialize`.
#[cfg(feature = "serde")]
pub(crate) fn deserialize_from_str<'de, S, D>(deserializer: D) -> Result<S, D::Error>
where
  S: for<'a> From<&'a str>,
  D: serde::Deserializer<'de>,
{
  let s: String = serde::Deserialize::deserialize(deserializer)?;
  Ok(S::from(&s))
}
