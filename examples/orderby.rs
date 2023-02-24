use enumit::EnumIt;
use serde::{Deserialize, Serialize};

#[derive(EnumIt, Serialize, Deserialize)]
#[enumit(
    name = "UnitOrderBy",
    tagged(type = "u8"),
    attrs(derive(Serialize, Deserialize))
)]
pub struct Unit {
    pub a: i32,
    #[enumit(tag = { 10 - 3 }, type = "u64", attrs(serde(rename = "bb")))]
    pub b: i32,
}

#[derive(EnumIt, Serialize, Deserialize)]
#[enumit(
    name = "FooOrderBy",
    tagged(type = "u16"),
    attrs(derive(Serialize, Deserialize))
)]
pub struct Foo<T: std::fmt::Display, C: Copy> {
    pub a: i32,
    #[enumit(tag = { 10 - 3 }, type = "u64", attrs(serde(rename = "bb")))]
    pub b: i32,
    pub c: C,
    pub t: T,
}

#[derive(EnumIt, Serialize, Deserialize)]
#[enumit(
    name = "BarOrderBy",
    tagged(type = "u16"),
    typed("u128"),
    attrs(derive(Serialize, Deserialize))
)]
pub struct Bar {
    pub a: i32,
    #[enumit(tag = { 10 - 3 }, type = "u64", attrs(serde(rename = "bb")))]
    pub b: i32,
}

fn main() {}
