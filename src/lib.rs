//! # Collada parser
//!
//! This is a parser for the Collada (`.dae`) format, used for interchange between 3D renderers
//! and games. Compared to the [`collada`](https://crates.io/crates/collada) crate,
//! this crate attempts to more directly represent the Collada data model, and it is also
//! significantly more complete.
//!
//! Currently it only supports reading, but writing is a planned addition.
//!
//! ## Usage
//!
//! The main entry point is the [`Document`] type, which has a [`FromStr`] implementation to convert
//! literal strings / slices, or [`Document::from_file`] to read from a `.dae` file on disk.
//!
//! Collada documents are parsed eagerly, validating everything according to the
//! [COLLADA schema](https://www.khronos.org/files/collada_spec_1_4.pdf).
//! Once parsed, the data structures (structs and enums) can be navigated directly,
//! as all the data structures are public, and reflect the XML schema closely.
//!
//! This library implements only version 1.4.1 of the Collada spec, although it may be expanded
//! in the future. (Please open an issue or PR if you find anything missing from the spec,
//! or if you have a use case for a later version.)
//!
//! ```
//! use std::str::FromStr;
//! use dae_parser::*;
//!
//! let dae_file = r##"\
//! <?xml version="1.0" encoding="utf-8"?>
//! <COLLADA xmlns="http://www.collada.org/2005/11/COLLADASchema" version="1.4.1">
//!   <asset>
//!     <created>1970-01-01T00:00:00</created>
//!     <modified>1970-01-01T00:00:00</modified>
//!   </asset>
//!   <library_geometries>
//!     <geometry id="Cube-mesh" name="Cube">
//!       <mesh>
//!         <source id="Cube-mesh-positions">
//!           <float_array id="Cube-mesh-positions-array" count="18">
//!             1 1 1 1 -1 1 1 -1 -1 -1 1 1 -1 -1 1 -1 -1 -1
//!           </float_array>
//!           <technique_common>
//!             <accessor source="#Cube-mesh-positions-array" count="6" stride="3">
//!               <param name="X" type="float"/>
//!               <param name="Y" type="float"/>
//!               <param name="Z" type="float"/>
//!             </accessor>
//!           </technique_common>
//!         </source>
//!         <vertices id="Cube-mesh-vertices">
//!           <input semantic="POSITION" source="#Cube-mesh-positions"/>
//!         </vertices>
//!         <triangles material="Material-material" count="4">
//!           <input semantic="VERTEX" source="#Cube-mesh-vertices" offset="0"/>
//!           <p>3 1 0 1 5 2 3 4 1 1 4 5</p>
//!         </triangles>
//!       </mesh>
//!     </geometry>
//!   </library_geometries>
//! </COLLADA>"##;
//!
//! let document = Document::from_str(dae_file).unwrap();
//! let cube = document.local_map::<Geometry>().unwrap().get("Cube-mesh").unwrap();
//! let sources_map = document.local_map::<Source>().unwrap();
//! let vertices_map = document.local_map::<Vertices>().unwrap();
//! // sources.get("Cube-mesh-positions").unwrap();
//! assert_eq!(cube.id.as_ref().unwrap(), "Cube-mesh");
//! let tris = if let GeometryElement::Mesh(mesh) = &cube.element {
//!     if let Primitive::Triangles(tris) = &mesh.elements[0] {
//!         tris
//!     } else { panic!() }
//! } else { panic!() };
//! assert_eq!(
//!     tris.data.as_deref().unwrap(),
//!     &[3, 1, 0, 1, 5, 2, 3, 4, 1, 1, 4, 5]
//! );
//! assert_eq!(tris.inputs[0].semantic, Semantic::Vertex);
//! let vertices = vertices_map.get_url(&tris.inputs[0].source).unwrap();
//! assert_eq!(vertices.id, "Cube-mesh-vertices");
//! let source = sources_map
//!     .get_url(&vertices.position_input().source)
//!     .unwrap();
//! assert_eq!(source.id.as_deref(), Some("Cube-mesh-positions"));
//! ```
//! ## License
//!
//! Licensed under either of
//!
//!  * Apache License, Version 2.0
//!    ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
//!  * MIT license
//!    ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
//!
//! at your option.
//!
//! ## Contribution
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.

#![deny(unsafe_code)]
#![warn(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    unused_import_braces,
    unused_qualifications
)]

#[macro_use]
mod macros;
mod api;
mod core;
mod fx;
mod physics;
mod url;

use crate::api::*;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::io::{BufRead, BufReader};
use std::ops::Deref;
use std::path::Path;
use std::str::FromStr;

pub use crate::{api::*, core::*, fx::*, physics::*};
pub use minidom::Element;
pub use url::Url;

type XReader<R> = minidom::quick_xml::Reader<R>;

/// The main error type used by this library.
#[derive(Debug)]
pub enum Error {
    /// An error during XML parsing.
    Parse(minidom::Error),
    /// A generic error given by a string.
    Other(&'static str),
    /// A generic error given by a string.
    Str(String),
}

impl From<std::io::Error> for Error {
    fn from(v: std::io::Error) -> Self {
        Self::Parse(v.into())
    }
}

impl From<minidom::Error> for Error {
    fn from(v: minidom::Error) -> Self {
        Self::Parse(v)
    }
}

impl From<&'static str> for Error {
    fn from(v: &'static str) -> Self {
        Self::Other(v)
    }
}

impl From<String> for Error {
    fn from(v: String) -> Self {
        Self::Str(v)
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

type ElementIter<'a> = std::iter::Peekable<minidom::Children<'a>>;

fn get_text(element: &Element) -> Option<&str> {
    let mut it = element.nodes();
    let text = match it.next() {
        None => "",
        Some(s) => s.as_text()?,
    };
    if it.next().is_some() {
        return None;
    }
    Some(text)
}

fn parse_text(element: &Element) -> Result<String> {
    Ok(get_text(element).ok_or("expecting a text node")?.to_owned())
}

fn parse_array<T: FromStr>(e: &Element) -> Result<Box<[T]>> {
    get_text(e)
        .ok_or("expected text node")?
        .split_ascii_whitespace()
        .map(|s| s.parse())
        .collect::<Result<_, _>>()
        .map_err(|_| "parse error".into())
}

fn parse_array_n<T: FromStr, const N: usize>(element: &Element) -> Result<Box<[T; N]>> {
    Ok(parse_array(element)?
        .try_into()
        .map_err(|_| "unexpected number of elements")?)
}

fn parse_elem<T: FromStr>(e: &Element) -> Result<T> {
    get_text(e)
        .ok_or("expected text node")?
        .parse()
        .map_err(|_| "parse error".into())
}

fn parse_attr<T: FromStr>(attr: Option<&str>) -> Result<Option<T>> {
    Ok(match attr {
        None => None,
        Some(s) => Some(s.parse().map_err(|_| "parse failure")?),
    })
}

fn parse_one<'a, T>(
    name: &str,
    it: &mut impl Iterator<Item = &'a Element>,
    f: impl FnOnce(&'a Element) -> Result<T>,
) -> Result<T> {
    let e = it.next().ok_or_else(|| format!("expected <{}>", name))?;
    if e.name() != name {
        return Err(format!("expected <{}>", name).into());
    }
    f(e)
}

fn parse_one_many<'a, T>(
    it: &mut impl Iterator<Item = &'a Element>,
    f: impl FnOnce(&'a Element) -> Result<Option<T>>,
) -> Result<T> {
    let e = it.next().ok_or("expected element")?;
    Ok(f(e)?.ok_or("expected element")?)
}

fn parse_opt<'a, T>(
    name: &str,
    it: &mut ElementIter<'a>,
    f: impl FnOnce(&'a Element) -> Result<T>,
) -> Result<Option<T>> {
    let mut res = None;
    if let Some(&e) = it.peek() {
        if e.name() == name {
            res = Some(f(e)?);
            it.next();
        }
    }
    Ok(res)
}

fn parse_opt_many<'a, T>(
    it: &mut ElementIter<'a>,
    f: impl FnOnce(&'a Element) -> Result<Option<T>>,
) -> Result<Option<T>> {
    let res = match it.peek() {
        None => None,
        Some(&e) => f(e)?,
    };
    if res.is_some() {
        it.next();
    }
    Ok(res)
}

fn parse_list<'a, T>(
    name: &str,
    it: &mut ElementIter<'a>,
    mut f: impl FnMut(&'a Element) -> Result<T>,
) -> Result<Vec<T>> {
    parse_list_many(it, |e| {
        Ok(if e.name() == name { Some(f(e)?) } else { None })
    })
}

fn finish<'a, T>(t: T, mut it: impl Iterator<Item = &'a Element>) -> Result<T> {
    if let Some(e) = it.next() {
        return Err(format!("unexpected node <{}>", e.name()).into());
    }
    Ok(t)
}

fn parse_list_many<'a, T>(
    it: &mut ElementIter<'a>,
    mut f: impl FnMut(&'a Element) -> Result<Option<T>>,
) -> Result<Vec<T>> {
    let mut res = vec![];
    while let Some(&e) = it.peek() {
        match f(e)? {
            Some(t) => res.push(t),
            None => break,
        }
        it.next();
    }
    Ok(res)
}

/// A common trait for all data structures that represent an XML element.
pub trait XNode: Sized {
    /// The name of the XML element.
    const NAME: &'static str;

    /// Parse an XML element into this type. In most cases, the parser will require with a
    /// `debug_assert` that the element to parse has name [`Self::NAME`].
    fn parse(element: &Element) -> Result<Self>;

    /// Parse an XML element and return the data structure in a `Box`.
    /// This can be faster in some cases when the data structure is large.
    fn parse_box(element: &Element) -> Result<Box<Self>> {
        Self::parse(element).map(Box::new)
    }

    /// Parse a single required element from the given element iterator.
    fn parse_one<'a>(it: &mut impl Iterator<Item = &'a Element>) -> Result<Self> {
        parse_one(Self::NAME, it, Self::parse)
    }

    /// Parse an optional element from the given element iterator, using [`Self::NAME`] to
    /// determine if it is the correct type.
    fn parse_opt(it: &mut ElementIter<'_>) -> Result<Option<Self>> {
        parse_opt(Self::NAME, it, Self::parse)
    }

    /// Parse an optional boxed element from the given element iterator, using [`Self::NAME`] to
    /// determine if it is the correct type.
    fn parse_opt_box(it: &mut ElementIter<'_>) -> Result<Option<Box<Self>>> {
        parse_opt(Self::NAME, it, Self::parse_box)
    }

    /// Parse a list of elements from the given element iterator,
    /// as long as it continues yielding elements of name [`Self::NAME`].
    fn parse_list(it: &mut ElementIter<'_>) -> Result<Vec<Self>> {
        parse_list(Self::NAME, it, Self::parse)
    }

    /// Parse a list of elements from the given element iterator,
    /// as long as it continues yielding elements of name [`Self::NAME`],
    /// and assert that the resulting list has length at least `N`.
    fn parse_list_n<const N: usize>(it: &mut ElementIter<'_>) -> Result<Vec<Self>> {
        let arr = parse_list(Self::NAME, it, Self::parse)?;
        if arr.len() < N {
            return Err(format!("parse error: expected {} {} elements", N, Self::NAME).into());
        }
        Ok(arr)
    }
}

/// A Collada document. Represents the `<COLLADA>` root node.
///
/// This is the main entry point for parsing.
#[derive(Clone, Debug)]
pub struct Document {
    /// Metadata about the origin of the document
    pub asset: Asset,
    /// The main content, organized into a list of "libraries".
    pub library: Vec<LibraryElement>,
    /// The actual scene being described, which references / instantiates
    /// objects in the libraries.
    pub scene: Option<Scene>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl FromStr for Document {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Self::try_from(s.as_bytes())
    }
}

impl TryFrom<&str> for Document {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self> {
        Self::from_str(s)
    }
}

impl TryFrom<&[u8]> for Document {
    type Error = Error;
    fn try_from(s: &[u8]) -> Result<Self> {
        Self::from_reader(std::io::Cursor::new(s))
    }
}

impl Document {
    /// Constructs a [`Document`] from a file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        Self::from_reader(BufReader::new(std::fs::File::open(path)?))
    }

    /// Constructs a [`Document`] from any [`BufRead`] reader.
    /// Use [`BufReader`] to construct a [`BufRead`] if you only have a
    /// [`Read`](std::io::Read) instance.
    pub fn from_reader<R: BufRead>(reader: R) -> Result<Self> {
        Self::from_xml_reader(&mut XReader::from_reader(reader))
    }

    /// Constructs a [`Document`] from a
    /// [`quick_xml::Reader`](minidom::quick_xml::Reader).
    pub fn from_xml_reader<R: BufRead>(reader: &mut XReader<R>) -> Result<Self> {
        let root = minidom::Element::from_reader(reader)?;
        Self::parse(&root)
    }
}

impl XNode for Document {
    const NAME: &'static str = "COLLADA";
    /// Parse a [`Document`] from a `<COLLADA>` [`Element`].
    fn parse(element: &Element) -> Result<Self> {
        if element.name() != Self::NAME {
            return Err("Expected COLLADA root node".into());
        }
        if element.attr("version") != Some("1.4.1") {
            return Err("Unsupported COLLADA version".into());
        }
        let mut it = element.children().peekable();
        Ok(Document {
            asset: Asset::parse_one(&mut it)?,
            library: parse_list_many(&mut it, LibraryElement::parse)?,
            scene: Scene::parse_opt(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}
