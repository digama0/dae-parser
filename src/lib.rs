//! A parser for the COLLADA format (`.dae` extension).
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
//! in the future (PRs welcome).
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
//! if let LibraryElement::Geometries(lib) = &document.library[0] {
//!     let geom = &lib.items[0];
//!     assert_eq!(geom.id.as_ref().unwrap(), "Cube-mesh");
//!     if let GeometryElement::Mesh(mesh) = &geom.element {
//!         assert_eq!(mesh.source[0].id.as_ref().unwrap(), "Cube-mesh-positions");
//!         if let Primitive::Triangles(tris) = &mesh.elements[0] {
//!             assert_eq!(
//!                 tris.data.0.as_deref().unwrap(),
//!                 &[3, 1, 0, 1, 5, 2, 3, 4, 1, 1, 4, 5]
//!             );
//!             return;
//!         }
//!     }
//! }
//! panic!() // ensure that the `if let`s above are taken
//! ```

#![forbid(unsafe_code)]
#![warn(missing_docs)]

mod url;

use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::io::{BufRead, BufReader};
use std::ops::Deref;
use std::path::Path;
use std::str::FromStr;

pub use crate::url::Url;
pub use minidom::Element;

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

/// Provides arbitrary additional information about or related to its parent element.
#[derive(Clone, Default, Debug)]
pub struct Extra {
    /// The unique identifier of the `Extra` element.
    /// This value must be unique within the document.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// A hint as to the type of information that the particular `Extra` element represents.
    /// This text string must be understood by the application.
    pub ty: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Declares the information used to process some portion of the content.
    /// This field is always nonempty, because the spec provides no common data
    /// for `Extra` elements.
    pub technique: Vec<Technique>,
}

impl XNode for Extra {
    const NAME: &'static str = "extra";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Extra {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            ty: element.attr("type").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            technique: Technique::parse_list_n::<1>(&mut it)?,
        };
        finish(res, it)
    }
}

impl Extra {
    fn parse_many<'a>(it: impl Iterator<Item = &'a Element>) -> Result<Vec<Extra>> {
        let mut extras = vec![];
        for e in it {
            match e.name() {
                "extra" => extras.push(Extra::parse(e)?),
                k => return Err(format!("unexpected element {}", k).into()),
            }
        }
        Ok(extras)
    }
}

/// Defines authoring information for asset management.
#[derive(Clone, Default, Debug)]
pub struct Contributor {
    /// The author’s name
    pub author: Option<String>,
    /// The name of the authoring tool
    pub authoring_tool: Option<String>,
    /// Comments from this contributor
    pub comments: Option<String>,
    /// Copyright information
    pub copyright: Option<String>,
    /// A reference to the source data used for this asset
    pub source_data: Option<Url>,
}

impl XNode for Contributor {
    const NAME: &'static str = "contributor";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Contributor {
            author: parse_opt("author", &mut it, parse_text)?,
            authoring_tool: parse_opt("authoring_tool", &mut it, parse_text)?,
            comments: parse_opt("comments", &mut it, parse_text)?,
            copyright: parse_opt("copyright", &mut it, parse_text)?,
            source_data: parse_opt("source_data", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

/// Defines unit of distance for COLLADA elements and objects.
/// This unit of distance applies to all spatial measurements
/// within the scope of `Asset`’s parent element, unless
/// overridden by a more local `Asset` / `Unit`.
///
/// The value of the unit is self-describing and does not have to be consistent
/// with any real-world measurement.
#[derive(Clone, Debug)]
pub struct Unit {
    /// The name of the distance unit. For example, "meter", "centimeter", "inches",
    /// or "parsec". This can be the real name of a measurement, or an imaginary name.
    pub name: Option<String>,
    /// How many real-world meters in one
    /// distance unit as a floating-point number. For
    /// example, 1.0 for the name "meter"; 1000 for the
    /// name "kilometer"; 0.3048 for the name "foot".
    pub meter: f32,
}

impl Default for Unit {
    fn default() -> Self {
        Unit {
            name: None,
            meter: 1.,
        }
    }
}

impl XNode for Unit {
    const NAME: &'static str = "unit";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Unit {
            name: element.attr("name").map(Into::into),
            meter: match element.attr("meter") {
                None => 1.,
                Some(s) => s.parse().map_err(|_| "parse error")?,
            },
        })
    }
}

/// Descriptive information about the coordinate system of the geometric data.
/// All coordinates are right-handed by definition.
#[derive(Clone, Debug)]
pub enum UpAxis {
    /// Right: `-y`, Up: `+x`, In: `+z`
    XUp,
    /// Right: `+x`, Up: `+y`, In: `+z`
    YUp,
    /// Right: `+x`, Up: `+z`, In: `-y`
    ZUp,
}

impl Default for UpAxis {
    fn default() -> Self {
        Self::YUp
    }
}

impl XNode for UpAxis {
    const NAME: &'static str = "up_axis";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(match get_text(element) {
            Some("X_UP") => UpAxis::XUp,
            Some("Y_UP") => UpAxis::YUp,
            Some("Z_UP") => UpAxis::ZUp,
            _ => return Err("invalid <up_axis> value".into()),
        })
    }
}

/// Defines asset-management information regarding its parent element.
#[derive(Clone, Debug)]
pub struct Asset {
    /// Defines data related to a contributor that worked on the parent element.
    pub contributor: Vec<Contributor>,
    /// Contains date and time that the parent element was created.
    /// Represented in an ISO 8601 format as per the XML Schema
    /// `dateTime` primitive type.
    pub created: String,
    /// Contains a list of words used as search criteria for the parent element.
    pub keywords: Vec<String>,
    /// Contains date and time that the parent element was last
    /// modified. Represented in an ISO 8601 format as per the
    /// XML Schema `dateTime` primitive type.
    pub modified: String,
    /// Contains revision information for the parent element.
    pub revision: Option<String>,
    /// Contains a description of the topical subject of the parent element.
    pub subject: Option<String>,
    /// Contains title information for the parent element.
    pub title: Option<String>,
    /// Defines unit of distance for COLLADA elements and objects.
    pub unit: Option<Unit>,
    /// Contains descriptive information about the coordinate system of the geometric data.
    pub up_axis: UpAxis,
}

impl XNode for Asset {
    const NAME: &'static str = "asset";

    fn parse_box(element: &Element) -> Result<Box<Self>> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Box::new(Asset {
            contributor: Contributor::parse_list(&mut it)?,
            created: parse_one("created", &mut it, parse_text)?,
            keywords: parse_opt("keywords", &mut it, parse_text)?.map_or_else(Vec::new, |s| {
                s.split_ascii_whitespace().map(|s| s.to_owned()).collect()
            }),
            modified: parse_one("modified", &mut it, parse_text)?,
            revision: parse_opt("revision", &mut it, parse_text)?,
            subject: parse_opt("subject", &mut it, parse_text)?,
            title: parse_opt("title", &mut it, parse_text)?,
            unit: Unit::parse_opt(&mut it)?,
            up_axis: UpAxis::parse_opt(&mut it)?.unwrap_or_default(),
        });
        finish(res, it)
    }

    fn parse(element: &Element) -> Result<Self> {
        Ok(*Self::parse_box(element)?)
    }
}

/// A trait for nodes that can be placed in a library element.
pub trait ParseLibrary: XNode {
    /// The name of the library element. For example, the [`Geometry`] element has
    /// `LIBRARY = "library_geometries"`,
    /// and the corresponding library type is [`Library`]`<Geometry>`.
    const LIBRARY: &'static str;
}

/// Declares an interpolation sampling function for an animation.
#[derive(Clone, Debug)]
pub struct Sampler {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// Assigns semantics to each [`Source`]. See the COLLADA spec for details.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Interpolation`] input (which must exist).
    pub interpolation: usize,
}

impl XNode for Sampler {
    const NAME: &'static str = "sampler";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        let res = Sampler {
            id: element.attr("id").map(Into::into),
            interpolation: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Interpolation)
                .ok_or("sampler: missing INTERPOLATION input")?,
            inputs,
        };
        finish(res, it)
    }
}

/// An unparsed COLLADA target address.
/// See the "Address Syntax" section in Chapter 3: Schema concepts of the
/// [COLLADA spec](https://www.khronos.org/files/collada_spec_1_4.pdf).
#[derive(Clone, Debug)]
pub struct Address(pub String);

/// Declares an output channel of an animation.
#[derive(Clone, Debug)]
pub struct Channel {
    /// The location of the animation sampler using a URL expression.
    pub source: Url,
    /// The location of the element bound to the output of the sampler.
    pub target: Address,
}

impl XNode for Channel {
    const NAME: &'static str = "channel";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("expecting target attr")?;
        Ok(Channel {
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            target: Address(target.into()),
        })
    }
}

/// Categorizes the declaration of animation information.
#[derive(Clone, Debug)]
pub struct Animation {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Allows the formation of a hierarchy of related animations.
    pub children: Vec<Animation>,
    /// The data repository that provides values according to the semantics of an
    /// [`Input`] element that refers to it.
    pub source: Vec<Source>,
    /// Describes the interpolation sampling function for the animation.
    pub sampler: Vec<Sampler>,
    /// Describes an output channel for the animation.
    pub channel: Vec<Channel>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Animation {
    const NAME: &'static str = "animation";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Animation {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            children: Animation::parse_list(&mut it)?,
            source: Source::parse_list(&mut it)?,
            sampler: Sampler::parse_list(&mut it)?,
            channel: Channel::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        if res.children.is_empty() && res.sampler.is_empty() {
            return Err("animation: no sampler/channel or children".into());
        }
        if res.sampler.is_empty() != res.channel.is_empty() {
            return Err("animation: sampler and channel must be used together".into());
        }
        Ok(res)
    }
}

/// Defines a section of a set of animation curves to be used together as an animation clip.
#[derive(Clone, Debug)]
pub struct AnimationClip {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The time in seconds of the beginning of the clip. This time is the same as that used in the
    /// key-frame data and is used to determine which set of key frames will be included in the
    /// clip. The start time does not specify when the clip will be played. If the time falls between
    /// two key frames of a referenced animation, an interpolated value should be used.
    pub start: f32,
    /// The time in seconds of the end of the clip. This is used in the same way as the start time.
    /// If `end` is not specified, the value is taken to be the end time of the longest animation.
    pub end: Option<f32>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Instantiates an [`Animation`] object.
    pub instance_animation: Vec<Instance<Animation>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for AnimationClip {
    const NAME: &'static str = "animation_clip";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(AnimationClip {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            start: parse_attr(element.attr("start"))?.unwrap_or(0.),
            end: parse_attr(element.attr("end"))?,
            asset: Asset::parse_opt_box(&mut it)?,
            instance_animation: Instance::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes the field of view of an orthographic camera.
#[derive(Clone, Debug)]
pub struct Orthographic {
    /// The horizontal (X) magnification of the view.
    pub xmag: Option<f32>,
    /// The vertical (Y) magnification of the view.
    pub ymag: Option<f32>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
    /// The aspect ratio of the field of view.
    pub aspect_ratio: Option<f32>,
    /// The distance to the near clipping plane.
    pub znear: f32,
    /// The distance to the far clipping plane.
    pub zfar: f32,
}

impl XNode for Orthographic {
    const NAME: &'static str = "orthographic";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Orthographic {
            xmag: parse_opt("xmag", &mut it, parse_elem)?,
            ymag: parse_opt("ymag", &mut it, parse_elem)?,
            extra: Extra::parse_list(&mut it)?,
            aspect_ratio: parse_opt("aspect_ratio", &mut it, parse_elem)?,
            znear: parse_one("znear", &mut it, parse_elem)?,
            zfar: parse_one("zfar", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

/// Describes the field of view of a perspective camera.
#[derive(Clone, Debug)]
pub struct Perspective {
    /// The horizontal field of view in degrees.
    pub xfov: Option<f32>,
    /// The vertical field of view in degrees.
    pub yfov: Option<f32>,
    /// The aspect ratio of the field of view.
    pub aspect_ratio: Option<f32>,
    /// The distance to the near clipping plane.
    pub znear: f32,
    /// The distance to the far clipping plane.
    pub zfar: f32,
}

impl XNode for Perspective {
    const NAME: &'static str = "perspective";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Perspective {
            xfov: parse_opt("xfov", &mut it, parse_elem)?,
            yfov: parse_opt("yfov", &mut it, parse_elem)?,
            aspect_ratio: parse_opt("aspect_ratio", &mut it, parse_elem)?,
            znear: parse_one("znear", &mut it, parse_elem)?,
            zfar: parse_one("zfar", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

/// The projection type of the camera.
#[derive(Clone, Debug)]
pub enum ProjectionType {
    /// An orthographic camera
    Orthographic(Orthographic),
    /// A perspective camera
    Perspective(Perspective),
}

impl ProjectionType {
    /// Parse a [`ProjectionType`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Orthographic::NAME => Ok(Some(Self::Orthographic(Orthographic::parse(e)?))),
            Perspective::NAME => Ok(Some(Self::Perspective(Perspective::parse(e)?))),
            _ => Ok(None),
        }
    }
}

/// Represents the apparatus on a camera that projects the image onto the image sensor.
#[derive(Clone, Debug)]
pub struct Optics {
    /// The projection type.
    pub ty: ProjectionType,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Optics {
    const NAME: &'static str = "optics";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Optics {
            ty: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_one_many(&mut it, ProjectionType::parse)?, it)
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Represents the image sensor of a camera (for example, film or CCD).
#[derive(Clone, Debug)]
pub struct Imager {
    /// Declares the information used to process some portion of the content.
    /// This field is always nonempty, because the spec provides no common data
    /// for `imager` elements.
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Imager {
    const NAME: &'static str = "imager";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Imager {
            technique: Technique::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares a view of the visual scene hierarchy or scene graph.
/// The camera contains elements that describe the camera’s optics and imager.
#[derive(Clone, Debug)]
pub struct Camera {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Describes the field of view and viewing frustum using canonical parameters.
    pub optics: Optics,
    /// Represents the image sensor of a camera (for example, film or CCD).
    pub imager: Option<Imager>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Camera {
    const NAME: &'static str = "camera";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Camera {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            optics: Optics::parse_one(&mut it)?,
            imager: Imager::parse_opt(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares the association between joint nodes and attribute data.
#[derive(Clone, Debug)]
pub struct Joints {
    /// The interpretation of the [`Source`]s.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Joint`] input (which must exist).
    /// The [`Source`] referenced by this input should contain a [`ArrayElement::Name`]
    /// that contains `sid`s to identify the joint nodes.
    /// `sid`s are used instead of [`IdRef`](ArrayElement::IdRef)s to allow a skin controller
    /// to be instantiated multiple times, where each instance can be animated independently.
    pub joint: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Joints {
    const NAME: &'static str = "joints";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list_n::<2>(&mut it)?;
        Ok(Joints {
            joint: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Joint)
                .ok_or("vertex_weights: missing JOINT input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes the combination of joints and weights used by a skin.
#[derive(Clone, Debug)]
pub struct VertexWeights {
    /// The number of vertices in the base mesh.
    pub count: usize,
    /// The [`InputS`] elements describe the joints and the attributes to be associated with them.
    pub inputs: InputList,
    /// The index into `inputs` for the [`Semantic::Joint`] input (which must exist).
    pub joint: usize,
    /// Contains a list of integers, each specifying the number of
    /// bones associated with one of the influences defined by [`VertexWeights`].
    pub vcount: Option<Box<[u32]>>,
    /// Contains a list of indices that describe which bones and
    /// attributes are associated with each vertex. An index of `-1`
    /// into the array of joints refers to the bind shape. Weights
    /// should be normalized before use.
    pub prim: Option<Box<[i32]>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for VertexWeights {
    const NAME: &'static str = "vertex_weights";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = InputList::parse::<2>(&mut it)?;
        let res = VertexWeights {
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            joint: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Joint)
                .ok_or("vertex_weights: missing JOINT input")?,
            inputs,
            vcount: parse_opt("vcount", &mut it, parse_array)?,
            prim: parse_opt("v", &mut it, parse_array)?,
            extra: Extra::parse_many(it)?,
        };
        validate_vcount(
            res.count,
            res.inputs.depth,
            res.vcount.as_deref(),
            res.prim.as_deref(),
        )?;
        Ok(res)
    }
}

/// Contains vertex and primitive information sufficient to describe blend-weight skinning.
#[derive(Clone, Debug)]
pub struct Skin {
    /// A URI reference to the base mesh (a static mesh or a morphed mesh).
    /// This also provides the bind-shape of the skinned mesh.
    pub source: Url,
    /// Provides extra information about the position and
    /// orientation of the base mesh before binding. Contains
    /// sixteen floating-point numbers representing a four-by-
    /// four matrix in column-major order. If `bind_shape_matrix` is not specified
    /// then an identity matrix may be used as the `bind_shape_matrix`.
    pub bind_shape_matrix: Option<Box<[f32; 16]>>,
    /// Provides most of the data required for skinning the given base mesh.
    pub sources: Vec<Source>,
    /// Aggregates the per-joint information needed for this skin.
    pub joints: Joints,
    /// Describes a per-vertex combination of joints and
    /// weights used in this skin. An index of `–1` into the array of
    /// joints refers to the bind shape. Weights should be
    /// normalized before use.
    pub weights: VertexWeights,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Skin {
    const NAME: &'static str = "skin";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Skin {
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            bind_shape_matrix: parse_opt("bind_shape_matrix", &mut it, parse_array_n)?,
            sources: Source::parse_list_n::<3>(&mut it)?,
            joints: Joints::parse_one(&mut it)?,
            weights: VertexWeights::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Which blending technique to use.
#[derive(Clone, Debug)]
pub enum MorphMethod {
    /// ```text
    /// (Target1, Target2, ...)*(w1, w2, ...) =
    ///     (1-w1-w2-...)*BaseMesh + w1*Target1 + w2*Target2 + ...
    /// ```
    Normalized,
    /// ```text
    /// (Target1, Target2, ...) + (w1, w2, ...) =
    ///     BaseMesh + w1*Target1 + w2*Target2 + ...
    /// ```
    Relative,
}

impl Default for MorphMethod {
    fn default() -> Self {
        Self::Normalized
    }
}

impl FromStr for MorphMethod {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "NORMALIZED" => Ok(Self::Normalized),
            "RELATIVE" => Ok(Self::Relative),
            _ => Err(()),
        }
    }
}

/// Declares morph targets, their weights, and any user-defined attributes associated with them.
#[derive(Clone, Debug)]
pub struct Targets {
    /// The interpretation of the [`Source`]s.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::MorphTarget`] input (which must exist).
    pub morph_target: usize,
    /// The index into `inputs` for the [`Semantic::MorphWeight`] input (which must exist).
    pub morph_weight: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Targets {
    const NAME: &'static str = "targets";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        Ok(Targets {
            morph_target: inputs
                .iter()
                .position(|i| i.semantic == Semantic::MorphTarget)
                .ok_or("targets: missing MORPH_TARGET input")?,
            morph_weight: inputs
                .iter()
                .position(|i| i.semantic == Semantic::MorphWeight)
                .ok_or("targets: missing MORPH_WEIGHT input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes the data required to blend between sets of static meshes.
#[derive(Clone, Debug)]
pub struct Morph {
    /// Data for morph weights and for morph targets.
    pub source: Url,
    /// Which blending technique to use.
    pub method: MorphMethod,
    /// Data for morph weights and for morph targets.
    pub sources: Vec<Source>,
    /// Input meshes (morph targets) to be blended.
    pub targets: Targets,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Morph {
    const NAME: &'static str = "morph";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Morph {
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            method: parse_attr(element.attr("method"))?.unwrap_or_default(),
            sources: Source::parse_list_n::<2>(&mut it)?,
            targets: Targets::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// The element that contains control data.
#[derive(Clone, Debug)]
pub enum ControlElement {
    /// Control data for blend-weight skinning.
    Skin(Skin),
    /// Control data for blending between sets of static meshes.
    Morph(Morph),
}

impl ControlElement {
    /// Parse a [`ControlElement`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Skin::NAME => Ok(Some(Self::Skin(Skin::parse(e)?))),
            Morph::NAME => Ok(Some(Self::Morph(Morph::parse(e)?))),
            _ => Ok(None),
        }
    }
}

/// Categorizes the declaration of generic control information.
#[derive(Clone, Debug)]
pub struct Controller {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The element that contains control data.
    pub element: ControlElement,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Controller {
    const NAME: &'static str = "controller";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Controller {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            element: parse_one_many(&mut it, ControlElement::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A strongly typed value that represents the `VALUE` in an object of the form `SYMBOL = VALUE`.
#[derive(Clone, Debug)]
pub enum AnnotType {
    /// `bool` type
    Bool(bool),
    /// `bool2` type
    Bool2([bool; 2]),
    /// `bool3` type
    Bool3([bool; 3]),
    /// `bool4` type
    Bool4([bool; 4]),
    /// `int` type
    Int(u32),
    /// `int2` type
    Int2([u32; 2]),
    /// `int3` type
    Int3(Box<[u32; 3]>),
    /// `int4` type
    Int4(Box<[u32; 4]>),
    /// `float` type
    Float(f32),
    /// `float2` type
    Float2([f32; 2]),
    /// `float3` type
    Float3(Box<[f32; 3]>),
    /// `float4` type
    Float4(Box<[f32; 4]>),
    /// `float2x2` type (linearized)
    Float2x2(Box<[f32; 2 * 2]>),
    /// `float3x3` type (linearized)
    Float3x3(Box<[f32; 3 * 3]>),
    /// `float4x4` type (linearized)
    Float4x4(Box<[f32; 4 * 4]>),
    /// `string` type
    String(Box<str>),
}

impl AnnotType {
    /// Parse a [`AnnotType`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            "bool" => Self::Bool(parse_elem(e)?),
            "bool2" => Self::Bool2(*parse_array_n(e)?),
            "bool3" => Self::Bool3(*parse_array_n(e)?),
            "bool4" => Self::Bool4(*parse_array_n(e)?),
            "int" => Self::Int(parse_elem(e)?),
            "int2" => Self::Int2(*parse_array_n(e)?),
            "int3" => Self::Int3(parse_array_n(e)?),
            "int4" => Self::Int4(parse_array_n(e)?),
            "float" => Self::Float(parse_elem(e)?),
            "float2" => Self::Float2(*parse_array_n(e)?),
            "float3" => Self::Float3(parse_array_n(e)?),
            "float4" => Self::Float4(parse_array_n(e)?),
            "float2x2" => Self::Float2x2(parse_array_n(e)?),
            "float3x3" => Self::Float3x3(parse_array_n(e)?),
            "float4x4" => Self::Float4x4(parse_array_n(e)?),
            "string" => Self::String(parse_text(e)?.into()),
            _ => return Ok(None),
        }))
    }
}

/// Adds a strongly typed annotation remark to the parent object.
#[derive(Clone, Debug)]
pub struct Annotate {
    /// The text string name of this element that represents the `SYMBOL` in an object of
    /// the form `SYMBOL = VALUE`.
    pub name: String,
    /// A strongly typed value that represents the VALUE in an object of
    /// the form SYMBOL = VALUE. Consists of a COLLADA type
    /// element that contains a value of that type.
    pub value: AnnotType,
}

impl XNode for Annotate {
    const NAME: &'static str = "annotate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Annotate {
            name: element.attr("name").ok_or("expecting name attr")?.into(),
            value: parse_one_many(&mut it, AnnotType::parse)?,
        };
        finish(res, it)
    }
}

/// Specifies a surface on a cube.
#[derive(Clone, Debug)]
pub enum SurfaceFace {
    /// The `+x` face
    PosX,
    /// The `-x` face
    NegX,
    /// The `+y` face
    PosY,
    /// The `-y` face
    NegY,
    /// The `+z` face
    PosZ,
    /// The `-z` face
    NegZ,
}

impl Default for SurfaceFace {
    fn default() -> Self {
        Self::PosX
    }
}

impl FromStr for SurfaceFace {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "POSITIVE_X" => Ok(Self::PosX),
            "NEGATIVE_X" => Ok(Self::NegX),
            "POSITIVE_Y" => Ok(Self::PosY),
            "NEGATIVE_Y" => Ok(Self::NegY),
            "POSITIVE_Z" => Ok(Self::PosZ),
            "NEGATIVE_Z" => Ok(Self::NegZ),
            _ => Err(()),
        }
    }
}

/// A [`Surface`] initialization option, which specifies
/// whether to initialize the surface and how to do so.
#[derive(Clone, Debug)]
pub enum SurfaceInit {
    /// This surface is intended to be initialized later externally by a [`EffectSetParam`] element.
    /// If it is used before being initialized, there is profile- and platform-specific behavior.
    /// Most elements on the [`Surface`] element that contains this will be ignored,
    /// including [`mip_levels`](Surface::mip_levels), [`mipmap_generate`](Surface::mipmap_generate),
    /// [`size`](Surface::size), [`viewport_ratio`](Surface::viewport_ratio),
    /// and [`format`](Surface::format).
    Null,
    /// Initializes this surface as a target for depth, stencil, or color. It does not need image
    /// data. If this element is used, [`mipmap_generate`](Surface::mipmap_generate) is ignored.
    Target,
    // Cube(InitCube),
    // Volume(InitVolume),
    // Planar(InitPlanar),
    /// Contains a reference to a 1D or 2D image. Initializes the surface one subsurface at
    /// a time by specifying combinations of `mip`, `face`, and `slice` that make sense for a
    /// particular surface type. Each subsurface is initialized by a common 1-D or 2-D
    /// image, not a complex compound image such as DDS. If not all subsurfaces are
    /// initialized, the surface is invalid and will result in profile- and platform-specific
    /// behavior unless [`mipmap_generate`](Surface::mipmap_generate) is responsible for
    /// initializing the remaining subsurfaces.
    /// All attributes are optional:
    /// • mip: An xs:unsignedInt that specifies the MIP level. The default is 0.
    /// • slice: An xs:unsignedInt that specifies which 2D layer within a
    /// volume to initialize. There are anywhere from 0 to n slices in a volume,
    /// where n is the volume’s depth slice. This attribute is used in combination
    /// with mip because a volume might have MIPmaps The default is 0.
    /// • face: An enumerated value of type fx_surface_face_enum that
    /// specifies which surface of a cube to initialize from the specified image.
    /// This attribute is used in combination with mip because a cubemap might
    /// have MIPmaps. The default is POSITIVE_
    From {
        /// The MIP level.
        mip: u32,
        /// Which 2D layer within a volume to initialize.
        /// There are anywhere from 0 to `n` slices in a volume,
        /// where `n` is the volume’s depth slice.
        /// This attribute is used in combination with `mip` because a volume might have MIPmaps.
        slice: u32,
        /// Which surface of a cube to initialize from the specified image.
        /// This attribute is used in combination with `mip` because a cubemap might
        /// have MIPmaps.
        face: SurfaceFace,
    },
}

impl SurfaceInit {
    /// Parse a [`SurfaceInit`] from an XML element.
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            "init_as_null" => Self::Null,
            "init_as_target" => Self::Target,
            "init_cube" | "init_volume" | "init_planar" => unimplemented!(),
            "init_from" => Self::From {
                mip: parse_attr(element.attr("mip"))?.unwrap_or(0),
                slice: parse_attr(element.attr("slice"))?.unwrap_or(0),
                face: parse_attr(element.attr("face"))?.unwrap_or_default(),
            },
            _ => return Ok(None),
        }))
    }
}

/// The per-texel layout of the format.
/// The length of the enumeration string indicates how many channels there are
/// and each letter represents the name of a channel. There are typically 1 to 4 channels.
#[derive(Clone, Debug)]
pub enum SurfaceChannels {
    /// Red/Green/Blue color map.
    RGB,
    /// Red/Green/Blue/Alpha map, often used for color and transparency
    /// or other things packed into channel A, such as specular power.
    RGBA,
    /// Luminance map, often used for light mapping.
    L,
    /// Luminance/Alpha map, often used for light mapping.
    LA,
    /// Depth map, often used for displacement, parallax, relief, or shadow mapping.
    D,
    /// Typically used for normal maps or three-component displacement maps.
    XYZ,
    /// Typically used for normal maps, where `W` is the depth for relief or parallax mapping.
    XYZW,
}

impl FromStr for SurfaceChannels {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "RGB" => Ok(Self::RGB),
            "RGBA" => Ok(Self::RGBA),
            "L" => Ok(Self::L),
            "LA" => Ok(Self::LA),
            "D" => Ok(Self::D),
            "XYZ" => Ok(Self::XYZ),
            "XYZW" => Ok(Self::XYZW),
            _ => Err(()),
        }
    }
}

/// The range of texel channel values. Each channel represents a range of values.
/// Some example ranges are signed or unsigned integers, or
/// are within a clamped range such as 0.0f to 1.0f, or are a
/// high dynamic range via floating point.
#[derive(Clone, Debug)]
pub enum SurfaceRange {
    /// Format represents a decimal value that remains within the -1 to 1 range.
    /// Implementation could be integer, fixed-point, or float.
    SNorm,
    /// Format represents a decimal value that remains within the 0 to 1 range.
    /// Implementation could be integer, fixed-point, or float.
    UNorm,
    /// Format represents signed integer numbers;
    /// for example, 8 bits can represent -128 to 127.
    SInt,
    /// Format represents unsigned integer numbers.
    /// For example, 8 bits can represent 0 to 255.
    UInt,
    /// Format should support full floating-point ranges typically used for high dynamic range.
    Float,
}

impl FromStr for SurfaceRange {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SNORM" => Ok(Self::SNorm),
            "UNORM" => Ok(Self::UNorm),
            "SINT" => Ok(Self::SInt),
            "UINT" => Ok(Self::UInt),
            "FLOAT" => Ok(Self::Float),
            _ => Err(()),
        }
    }
}
/// The precision of the texel channel value.
///
/// Each channel of the texel has a precision. Typically, channels have the same precision. An
/// exact format may lower the precision of an individual channel
/// but applying a higher precision by linking the channels may still convey the same information.
#[derive(Clone, Debug)]
pub enum SurfacePrecision {
    /// For integers, this typically represents 8 bits.
    /// For floats, typically 16 bits.
    Low,
    /// For integers, this typically represents 8 to 24
    /// bits. For floats, typically 16 to 32 bits.
    Mid,
    /// For integers, this typically represents 16 to
    /// 32 bits. For floats, typically 24 to 32 bits.
    High,
}

impl FromStr for SurfacePrecision {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "LOW" => Ok(Self::Low),
            "MID" => Ok(Self::Mid),
            "HIGH" => Ok(Self::High),
            _ => Err(()),
        }
    }
}

/// Contains additional hints about data relationships and other
/// things to help an application pick the best format.
#[derive(Clone, Debug)]
pub enum SurfaceOption {
    /// Colors are stored with respect
    /// to the sRGB 2.2 gamma curve rather than linear.
    SrgbGamma,
    /// The texel’s XYZ/RGB should be
    /// normalized such as in a normal map.
    Normalized3,
    /// The texel’s XYZW/RGBA should
    /// be normalized such as in a normal map.
    Normalized4,
    /// The surface may use run-time compression.
    /// Consider the best compression based on desired [`SurfaceChannels`],
    /// [`SurfaceRange`], [`SurfacePrecision`], and [`SurfaceOption`]s.
    Compressible,
}

impl FromStr for SurfaceOption {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SRGB_GAMMA" => Ok(Self::SrgbGamma),
            "NORMALIZED3" => Ok(Self::Normalized3),
            "NORMALIZED4" => Ok(Self::Normalized4),
            "COMPRESSABLE" => Ok(Self::Compressible),
            _ => Err(()),
        }
    }
}

/// This element describes the important features intended by the author so that the
/// application can pick a format that best represents what the author wanted.
#[derive(Clone, Debug)]
pub struct FormatHint {
    /// The per-texel layout of the format.
    pub channels: SurfaceChannels,
    /// The range of texel channel values.
    pub range: SurfaceRange,
    /// The precision of the texel channel value.
    pub precision: SurfacePrecision,
    /// Additional hints about data relationships and other
    /// things to help an application pick the best format.
    pub options: Vec<SurfaceOption>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for FormatHint {
    const NAME: &'static str = "format_hint";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(FormatHint {
            channels: parse_one("channels", &mut it, parse_elem)?,
            range: parse_one("range", &mut it, parse_elem)?,
            precision: parse_one("precision", &mut it, parse_elem)?,
            options: parse_list("option", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares a resource that can be used both as the source for
/// texture samples and as the target of a rendering pass.
#[derive(Clone, Debug)]
pub struct Surface {
    /// An initialization option for this surface.
    pub init: SurfaceInit,
    /// Contains a string representing the texel format for this surface.
    /// If this element is not specified or understood by the application,
    /// then the application will attempt to use `format_hint` if it is provided;
    /// otherwise, it should use a common format linear `R8G8B8A8`.
    pub format: Option<String>,
    /// An application uses `format_hint` if `format` does not exist or
    /// is not understood by the application and `format_hint` exists.
    /// This element describes the important features intended by the author
    /// so that the application can pick a format that best represents what the author wanted.
    pub format_hint: Option<Box<FormatHint>>,
    /// Contains three integer values. If specified, the surface is
    /// sized to these exact dimensions in texels. Surfaces of
    /// type `1D` and `CUBE` use only the first value. Surfaces of
    /// type `2D` and `RECT` use only the first two values,
    /// representing width and then height. Type `3D` uses all
    /// three values, representing width, height, and depth.
    /// Invalid if `viewport_ratio` is used.
    pub size: Option<Box<[u32; 3]>>,
    /// Contains two floating-point values representing width and then height.
    /// If specified, the surface is sized to a dimension
    /// based on these ratios of the viewport's (backbuffer's) dimensions.
    /// For example, `viewport_ratio = Some([0.5, 2])`
    /// scales the surface’s width to half the viewport’s width
    /// and its height to twice the viewport’s height.
    /// This element is valid only for surfaces of type `2D` or `RECT`.
    /// Invalid if `size` is used.
    pub viewport_ratio: Option<Box<[f32; 2]>>,
    /// Contains the number of MIP levels in the surface. A value
    /// of 0 assumes that all MIP levels exist until a dimension
    /// becomes 1 texel. To create a surface that has only one
    /// level of MIP maps (`mip` = 0), set this to 1.
    pub mip_levels: u32,
    /// If false and not all subsurfaces are
    /// initialized because you have not provided MIP-map
    /// levels, the generated surface will have profile- and
    ///
    /// platform-specific behavior. If true, the application is
    /// responsible for initializing the remainder of the
    /// subsurfaces; this is typically done through a graphics API
    /// render state or function that does this automatically, such
    /// as `glGenerateMipmap()`.
    pub mipmap_generate: bool,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Surface {
    const NAME: &'static str = "surface";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Surface {
            init: parse_one_many(&mut it, SurfaceInit::parse)?,
            format: parse_opt("format", &mut it, parse_text)?,
            format_hint: FormatHint::parse_opt_box(&mut it)?,
            size: parse_opt("size", &mut it, parse_array_n)?,
            viewport_ratio: parse_opt("viewport_ratio", &mut it, parse_array_n)?,
            mip_levels: parse_opt("mip_levels", &mut it, parse_elem)?.unwrap_or(0),
            mipmap_generate: parse_opt("mipmap_generate", &mut it, parse_elem)?.unwrap_or(false),
            extra: Extra::parse_many(it)?,
        };
        if res.size.is_some() && res.viewport_ratio.is_some() {
            return Err("size and viewport_ratio cannot be used together".into());
        }
        Ok(res)
    }
}

/// Wrap modes that affect the interpretation of `s`, `t`, and `p` texture coordinates in `Sampler*`
/// elements.
#[derive(Clone, Debug)]
pub enum WrapMode {
    /// OpenGL symbol `GL_REPEAT`.
    /// Ignores the integer part of texture coordinates, using only the fractional part.
    Wrap,
    /// OpenGL symbol `GL_MIRRORED_REPEAT`.
    /// First mirrors the texture coordinate.
    /// The mirrored coordinate is then clamped as described for [`Clamp`](WrapMode::Clamp).
    Mirror,
    /// OpenGL symbol `GL_CLAMP_TO_EDGE`.
    /// Clamps texture coordinates at all mipmap levels such
    /// that the texture filter never samples a border texel.
    /// *Note*: `GL_CLAMP` takes any texels beyond the
    /// sampling border and substitutes those texels with
    /// the border color. So `CLAMP_TO_EDGE` is more
    /// appropriate. This also works much better with
    /// OpenGL ES where the `GL_CLAMP` symbol was
    /// removed from the OpenGL ES specification.
    Clamp,
    /// OpenGL symbol `GL_CLAMP_TO_BORDER`.
    /// Clamps texture coordinates at all MIPmaps such that
    /// the texture filter always samples border texels for
    /// fragments whose corresponding texture coordinate
    /// is sufficiently far outside the range [0, 1].
    Border,
    /// The defined behavior for `None` is consistent with
    /// decal texturing where the border is black. Mapping
    /// this calculation to `GL_CLAMP_TO_BORDER` is the best approximation of this.
    None,
}

impl Default for WrapMode {
    fn default() -> Self {
        Self::Wrap
    }
}

impl FromStr for WrapMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "WRAP" => Ok(Self::Wrap),
            "MIRROR" => Ok(Self::Mirror),
            "CLAMP" => Ok(Self::Clamp),
            "BORDER" => Ok(Self::Border),
            "NONE" => Ok(Self::None),
            _ => Err(()),
        }
    }
}

/// (Undocumented?) Enumerated type `fx_sampler_filter_common` from COLLADA spec.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum SamplerFilterMode {
    None,
    Nearest,
    Linear,
    NearestMipmapNearest,
    LinearMipmapNearest,
    NearestMipmapLinear,
    LinearMipmapLinear,
}

impl Default for SamplerFilterMode {
    fn default() -> Self {
        Self::None
    }
}

impl FromStr for SamplerFilterMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "NONE" => Ok(Self::None),
            "NEAREST" => Ok(Self::Nearest),
            "LINEAR" => Ok(Self::Linear),
            "NEAREST_MIPMAP_NEAREST" => Ok(Self::NearestMipmapNearest),
            "LINEAR_MIPMAP_NEAREST" => Ok(Self::LinearMipmapNearest),
            "NEAREST_MIPMAP_LINEAR" => Ok(Self::NearestMipmapLinear),
            "LINEAR_MIPMAP_LINEAR" => Ok(Self::LinearMipmapLinear),
            _ => Err(()),
        }
    }
}

/// Declares a two-dimensional texture sampler.
#[derive(Clone, Debug)]
pub struct Sampler2D {
    /// A name, which is the `sid` of a [`Surface`].
    /// A `Sampler*` is a definition of how a shader will resolve a
    /// color out of a [`Surface`]. `source` identifies the [`Surface`] to read.
    pub source: String,
    /// Wrap mode in the first texture coordinate.
    pub wrap_s: WrapMode,
    /// Wrap mode in the second texture coordinate.
    pub wrap_t: WrapMode,
    /// Texture minimization. Applying a texture to a primitive
    /// implies a mapping from texture image space to framebuffer image space.
    /// In general, this mapping involves a reconstruction of the sampled texture image,
    /// followed by a homogeneous warping implied by the mapping to framebuffer space,
    /// then a filtering, followed finally by a resampling of the filtered, warped,
    /// reconstructed image before applying it to a fragment.
    pub min_filter: SamplerFilterMode,
    /// Texture magnification. Enumerated type
    /// fx_sampler_filter_common. When gamma indicates
    /// magnification, this value determines how the texture value is
    /// obtained.
    pub mag_filter: SamplerFilterMode,
    /// MIPmap filter.
    pub mip_filter: SamplerFilterMode,
    /// When reading past the edge of the texture address space
    /// based on the wrap modes involving clamps, this color takes
    /// over. Type `fx_color_common` (four floating-point numbers in RGBA order).
    pub border_color: Option<Box<[f32; 4]>>,
    /// The maximum number of progressive levels that the sampler will evaluate.
    pub mipmap_max_level: u8,
    /// Biases the gamma (level of detail parameter) that is used by the sampler
    /// to evaluate the MIPmap chain.
    pub mipmap_bias: f32,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Sampler2D {
    const NAME: &'static str = "sampler2D";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Sampler2D {
            source: parse_one("source", &mut it, parse_text)?,
            wrap_s: parse_opt("wrap_s", &mut it, parse_elem)?.unwrap_or_default(),
            wrap_t: parse_opt("wrap_t", &mut it, parse_elem)?.unwrap_or_default(),
            min_filter: parse_opt("minfilter", &mut it, parse_elem)?.unwrap_or_default(),
            mag_filter: parse_opt("magfilter", &mut it, parse_elem)?.unwrap_or_default(),
            mip_filter: parse_opt("mipfilter", &mut it, parse_elem)?.unwrap_or_default(),
            border_color: parse_opt("border_color", &mut it, parse_array_n)?,
            mipmap_max_level: parse_opt("mipmap_maxlevel", &mut it, parse_elem)?.unwrap_or(0),
            mipmap_bias: parse_opt("mipmap_bias", &mut it, parse_elem)?.unwrap_or(0.),
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A parameter's type. We do not have full support here,
/// but unknown types can be retrieved in the `Other` variant.
#[derive(Clone, Debug)]
pub enum ParamType {
    /// `float` type
    Float(f32),
    /// `float2` type
    Float2([f32; 2]),
    /// `float3` type
    Float3(Box<[f32; 3]>),
    /// `float4` type
    Float4(Box<[f32; 4]>),
    /// `surface` type
    Surface(Box<Surface>),
    /// `sampler2D` type
    Sampler2D(Box<Sampler2D>),
    /// Any other type, stored as a raw XML element.
    Other(Box<Element>),
}

impl ParamType {
    /// Parse a [`ParamType`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            "float" => Self::Float(parse_elem(e)?),
            "float2" => Self::Float2(*parse_array_n(e)?),
            "float3" => Self::Float3(parse_array_n(e)?),
            "float4" => Self::Float4(parse_array_n(e)?),
            Surface::NAME => Self::Surface(Surface::parse_box(e)?),
            Sampler2D::NAME => Self::Sampler2D(Sampler2D::parse_box(e)?),
            _ => Self::Other(Box::new(e.clone())),
        }))
    }
}

macro_rules! mk_extensible_enum {
    ($(#[$tydoc:meta])* pub enum $ty:ident { $($(#[$doc:meta])* $n:ident = $t:literal,)* }) => {
        $(#[$tydoc])*
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum $ty {
            $($(#[$doc])* $n,)*
            /// Any value not covered above
            Other(Box<str>),
        }

        impl $ty {
            fn parse(s: &str) -> Self {
                match s {
                    $($t => Self::$n,)*
                    _ => Self::Other(s.into()),
                }
            }
        }

        impl FromStr for $ty {
            type Err = std::convert::Infallible;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(Self::parse(s))
            }
        }
    };
}

mk_extensible_enum! {
    /// Additional information about the volatility or linkage of a parameter.
    pub enum Modifier {
        /// `CONST` linkage modifier
        Const = "CONST",
        /// `UNIFORM` linkage modifier
        Uniform = "UNIFORM",
        /// `VARYING` linkage modifier
        Varying = "VARYING",
        /// `STATIC` linkage modifier
        Static = "STATIC",
        /// `VOLATILE` linkage modifier
        Volatile = "VOLATILE",
        /// `EXTERN` linkage modifier
        Extern = "EXTERN",
        /// `SHARED` linkage modifier
        Shared = "SHARED",
    }
}

/// Instruction to create a new, named `Param` object in the FX Runtime,
/// assign it a type, an initial value, and additional attributes at declaration time.
#[derive(Clone, Debug)]
pub struct NewParam {
    /// Identifier for this parameter (that is, the variable name).
    pub sid: String,
    /// A list of strongly typed annotation remarks.
    pub annotate: Vec<Annotate>,
    /// Meta-information that describes the purpose of the parameter declaration.
    pub semantic: Option<String>,
    /// Additional information about the volatility or linkage.
    pub modifier: Option<Modifier>,
    /// The parameter's type.
    pub ty: ParamType,
}

impl XNode for NewParam {
    const NAME: &'static str = "newparam";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = NewParam {
            sid: element.attr("sid").ok_or("expecting sid attr")?.into(),
            annotate: Annotate::parse_list(&mut it)?,
            semantic: parse_opt("semantic", &mut it, parse_text)?,
            modifier: parse_opt("modifier", &mut it, parse_elem)?,
            ty: parse_one_many(&mut it, ParamType::parse)?,
        };
        finish(res, it)
    }
}

/// An [`Image`] or [`NewParam`] element.
#[derive(Clone, Debug)]
pub enum ImageParam {
    /// A [`NewParam`] element.
    NewParam(NewParam),
    /// An [`Image`] element.
    Image(Image),
}

/// Holds a description of the textures, samplers, shaders, parameters,
/// and passes necessary for rendering this effect using one method.
///
/// It is parameterized on additional data determined by the parent of this element.
#[derive(Clone, Debug)]
pub struct TechniqueFx<T> {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// A text string value containing the subidentifier of this element.
    /// This value must be unique within the scope of the parent element.
    pub sid: String,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The profile-specific child data.
    pub data: T,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl<T: ProfileData> XNode for TechniqueFx<T> {
    const NAME: &'static str = "technique";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TechniqueFx {
            id: element.attr("sid").map(Into::into),
            sid: element.attr("sid").ok_or("expecting sid attr")?.into(),
            asset: Asset::parse_opt_box(&mut it)?,
            data: T::parse(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A trait for the types that are legal to go in a [`TechniqueFx<T>`].
pub trait ProfileData: Sized {
    /// Parse the embedded data from a subsequence of children in the `<technique>` node.
    fn parse(it: &mut ElementIter<'_>) -> Result<Self>;
}

impl ImageParam {
    fn parse_list(it: &mut ElementIter<'_>) -> Result<Vec<Self>> {
        parse_list_many(it, |e| {
            Ok(Some(match e.name() {
                Image::NAME => Self::Image(Image::parse(e)?),
                NewParam::NAME => Self::NewParam(NewParam::parse(e)?),
                _ => return Ok(None),
            }))
        })
    }
}

/// A color parameter referencing a texture.
#[derive(Clone, Debug)]
pub struct Texture {
    /// The texture to reference.
    pub texture: String,
    /// A semantic token, which will be referenced within
    /// [`BindMaterial`] to bind an array of texcoords from a
    /// [`Geometry`] instance to the `TextureUnit`.
    pub texcoord: String,
    /// Provides arbitrary additional information about this element.
    pub extra: Option<Box<Extra>>,
}

impl XNode for Texture {
    const NAME: &'static str = "texture";
    fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Texture {
            texture: e.attr("texture").ok_or("expected texture attr")?.into(),
            texcoord: e.attr("texcoord").ok_or("expected texcoord attr")?.into(),
            extra: Extra::parse_opt_box(&mut it)?,
        };
        finish(res, it)
    }
}

/// A type that describes color attributes of fixed-function shader elements inside
/// `ProfileCommon` effects.
#[derive(Clone, Debug)]
pub enum ColorParam {
    /// The value is a literal color, specified by four floating-point numbers in RGBA order.
    Color(Box<[f32; 4]>),
    /// The value is specified by a reference to a previously defined parameter
    /// in the current scope that can be cast directly to a `float4`.
    Param(Box<str>),
    /// The value is specified by a reference to a previously defined `sampler2D` object.
    Texture(Box<Texture>),
}

impl ColorParam {
    /// Parse a [`ColorParam`] from an XML element.
    pub fn parse(element: &Element) -> Result<Self> {
        let mut it = element.children().peekable();
        parse_one_many(&mut it, |e| {
            Ok(Some(match e.name() {
                "color" => Self::Color(parse_array_n(e)?),
                Param::NAME => Self::Param(e.attr("ref").ok_or("expected ref attr")?.into()),
                Texture::NAME => Self::Texture(Texture::parse_box(e)?),
                _ => return Ok(None),
            }))
        })
    }
}

/// A type that describes the scalar attributes of fixed-function shader elements inside
/// `ProfileCommon` effects.
#[derive(Clone, Debug)]
pub enum FloatParam {
    /// The value is represented by a literal floating-point scalar.
    Float(f32),
    /// The value is represented by a reference to a previously
    /// defined parameter that can be directly cast to a floating-point scalar.
    Param(Box<str>),
}

impl FloatParam {
    /// Parse a [`FloatParam`] from an XML element.
    pub fn parse(element: &Element) -> Result<Self> {
        let mut it = element.children().peekable();
        parse_one_many(&mut it, |e| {
            Ok(Some(match e.name() {
                "float" => Self::Float(parse_elem(e)?),
                Param::NAME => Self::Param(e.attr("ref").ok_or("expected ref attr")?.into()),
                _ => return Ok(None),
            }))
        })
    }
}

/// Produces a specularly shaded surface with a Blinn BRDF approximation.
#[derive(Clone, Default, Debug)]
pub struct Blinn {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<ColorParam>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<ColorParam>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<ColorParam>,
    /// Declares the color of light specularly reflected from the surface of this object.
    pub specular: Option<ColorParam>,
    /// Declares the specularity or roughness of the specular reflection lobe.
    pub shininess: Option<FloatParam>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<ColorParam>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<FloatParam>,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<ColorParam>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<FloatParam>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<FloatParam>,
}

impl XNode for Blinn {
    const NAME: &'static str = "blinn";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Blinn {
            emission: parse_opt("emission", &mut it, ColorParam::parse)?,
            ambient: parse_opt("ambient", &mut it, ColorParam::parse)?,
            diffuse: parse_opt("diffuse", &mut it, ColorParam::parse)?,
            specular: parse_opt("specular", &mut it, ColorParam::parse)?,
            shininess: parse_opt("shininess", &mut it, FloatParam::parse)?,
            reflective: parse_opt("reflective", &mut it, ColorParam::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, FloatParam::parse)?,
            transparent: parse_opt("transparent", &mut it, ColorParam::parse)?,
            transparency: parse_opt("transparency", &mut it, FloatParam::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, FloatParam::parse)?,
        })
    }
}

/// Produces a constantly shaded surface that is independent of lighting.
#[derive(Clone, Default, Debug)]
pub struct ConstantFx {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<ColorParam>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<ColorParam>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<FloatParam>,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<ColorParam>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<FloatParam>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<FloatParam>,
}

impl XNode for ConstantFx {
    const NAME: &'static str = "constant";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(ConstantFx {
            emission: parse_opt("emission", &mut it, ColorParam::parse)?,
            reflective: parse_opt("reflective", &mut it, ColorParam::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, FloatParam::parse)?,
            transparent: parse_opt("transparent", &mut it, ColorParam::parse)?,
            transparency: parse_opt("transparency", &mut it, FloatParam::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, FloatParam::parse)?,
        })
    }
}

/// Produces a diffuse shaded surface that is independent of lighting.
#[derive(Clone, Default, Debug)]
pub struct Lambert {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<ColorParam>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<ColorParam>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<ColorParam>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<ColorParam>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<FloatParam>,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<ColorParam>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<FloatParam>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<FloatParam>,
}

impl XNode for Lambert {
    const NAME: &'static str = "lambert";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Lambert {
            emission: parse_opt("emission", &mut it, ColorParam::parse)?,
            ambient: parse_opt("ambient", &mut it, ColorParam::parse)?,
            diffuse: parse_opt("diffuse", &mut it, ColorParam::parse)?,
            reflective: parse_opt("reflective", &mut it, ColorParam::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, FloatParam::parse)?,
            transparent: parse_opt("transparent", &mut it, ColorParam::parse)?,
            transparency: parse_opt("transparency", &mut it, FloatParam::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, FloatParam::parse)?,
        })
    }
}

/// Produces a specularly shaded surface where the specular reflection is shaded
/// according the Phong BRDF approximation.
#[derive(Clone, Default, Debug)]
pub struct Phong {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<ColorParam>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<ColorParam>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<ColorParam>,
    /// the surface of this object.  the surface of this object.
    pub specular: Option<ColorParam>,
    /// reflection lobe.reflection lobe.
    pub shininess: Option<FloatParam>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<ColorParam>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<FloatParam>,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<ColorParam>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<FloatParam>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<FloatParam>,
}

impl XNode for Phong {
    const NAME: &'static str = "phong";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Phong {
            emission: parse_opt("emission", &mut it, ColorParam::parse)?,
            ambient: parse_opt("ambient", &mut it, ColorParam::parse)?,
            diffuse: parse_opt("diffuse", &mut it, ColorParam::parse)?,
            specular: parse_opt("specular", &mut it, ColorParam::parse)?,
            shininess: parse_opt("shininess", &mut it, FloatParam::parse)?,
            reflective: parse_opt("reflective", &mut it, ColorParam::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, FloatParam::parse)?,
            transparent: parse_opt("transparent", &mut it, ColorParam::parse)?,
            transparency: parse_opt("transparency", &mut it, FloatParam::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, FloatParam::parse)?,
        })
    }
}

/// A shader element.
#[derive(Clone, Debug)]
pub enum Shader {
    /// Produces a specularly shaded surface with a Blinn BRDF approximation.
    Blinn(Blinn),
    /// Produces a constantly shaded surface that is independent of lighting.
    Constant(ConstantFx),
    /// Produces a diffuse shaded surface that is independent of lighting.
    Lambert(Lambert),
    /// Produces a specularly shaded surface where the specular reflection is shaded
    /// according the Phong BRDF approximation.
    Phong(Phong),
}

impl Shader {
    /// Parse a [`Shader`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            Blinn::NAME => Self::Blinn(Blinn::parse(e)?),
            ConstantFx::NAME => Self::Constant(ConstantFx::parse(e)?),
            Lambert::NAME => Self::Lambert(Lambert::parse(e)?),
            Phong::NAME => Self::Phong(Phong::parse(e)?),
            _ => return Ok(None),
        }))
    }
}

/// The extra data in a [`TechniqueFx`] as the child of [`ProfileCommon`].
#[derive(Clone, Default, Debug)]
pub struct CommonData {
    /// A list of [`Image`] and [`NewParam`] elements in no particular order.
    pub image_param: Vec<ImageParam>,
    /// A list of shader elements.
    pub shaders: Vec<Shader>,
}

impl ProfileData for CommonData {
    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(CommonData {
            image_param: ImageParam::parse_list(it)?,
            shaders: parse_list_many(it, Shader::parse)?,
        })
    }
}

/// Opens a block of platform-independent declarations for the common, fixed-function shader.
#[derive(Clone, Debug)]
pub struct ProfileCommon {
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Declares a standard COLLADA image resource.
    pub image: Vec<Image>,
    /// Creates a new parameter from a constrained set of
    /// types recognizable by all platforms, see [`ParamType`].
    pub new_param: Vec<NewParam>,
    /// Declares the only technique for this effect.
    pub technique: TechniqueFx<CommonData>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for ProfileCommon {
    const NAME: &'static str = "profile_COMMON";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let asset = Asset::parse_opt_box(&mut it)?;
        let image_param = ImageParam::parse_list(&mut it)?;
        let mut image = vec![];
        let mut new_param = vec![];
        for ip in image_param {
            match ip {
                ImageParam::Image(e) => image.push(e),
                ImageParam::NewParam(e) => new_param.push(e),
            }
        }
        Ok(ProfileCommon {
            asset,
            image,
            new_param,
            technique: TechniqueFx::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// The `<profile_CG>` element and its contents are unsupported
/// and represented here as a raw XML element.
#[derive(Clone, Debug)]
pub struct ProfileCG(pub Element); // TODO

impl XNode for ProfileCG {
    const NAME: &'static str = "profile_CG";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileCG(element.clone()))
    }
}

/// The `<profile_GLES>` element and its contents are unsupported
/// and represented here as a raw XML element.
#[derive(Clone, Debug)]
pub struct ProfileGLES(pub Element); // TODO

impl ProfileGLES {
    const NAME: &'static str = "profile_GLES";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileGLES(element.clone()))
    }
}

/// The `<profile_GLSL>` element and its contents are unsupported
/// and represented here as a raw XML element.
#[derive(Clone, Debug)]
pub struct ProfileGLSL(pub Element); // TODO

impl ProfileGLSL {
    const NAME: &'static str = "profile_GLSL";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileGLSL(element.clone()))
    }
}

/// A `<profile_*>` element.
#[derive(Clone, Debug)]
pub enum Profile {
    /// Opens a block of platform-independent declarations for the common,
    /// fixed-function shader.
    Common(ProfileCommon),
    /// Declares platform-specific data types and [`Technique`](TechniqueFx)s
    /// for the Cg language.
    CG(ProfileCG),
    /// Declares platform-specific data types and [`Technique`](TechniqueFx)s
    /// for OpenGL ES.
    GLES(ProfileGLES),
    /// Declares platform-specific data types and [`Technique`](TechniqueFx)s
    /// for OpenGL Shading Language.
    GLSL(ProfileGLSL),
}

impl Profile {
    /// Parse a [`Profile`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            ProfileCommon::NAME => Self::Common(ProfileCommon::parse(e)?),
            ProfileCG::NAME => Self::CG(ProfileCG::parse(e)?),
            ProfileGLES::NAME => Self::GLES(ProfileGLES::parse(e)?),
            ProfileGLSL::NAME => Self::GLSL(ProfileGLSL::parse(e)?),
            _ => return Ok(None),
        }))
    }
}

/// Provides a self-contained description of a COLLADA effect.
#[derive(Clone, Debug)]
pub struct Effect {
    /// Global identifier for this object.
    pub id: String,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// A list of strongly typed annotation remarks.
    pub annotate: Vec<Annotate>,
    /// Declares a standard COLLADA image resource.
    pub image: Vec<Image>,
    /// Creates a new parameter from a constrained set of
    /// types recognizable by all platforms, see [`ParamType`].
    pub new_param: Vec<NewParam>,
    /// At least one profile must appear.
    pub profile: Vec<Profile>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Effect {
    const NAME: &'static str = "effect";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Effect {
            id: element.attr("id").ok_or("expected id attr")?.into(),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            annotate: Annotate::parse_list(&mut it)?,
            image: Image::parse_list(&mut it)?,
            new_param: NewParam::parse_list(&mut it)?,
            profile: parse_list_many(&mut it, Profile::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.profile.is_empty() {
            return Err("expected at least one profile".into());
        }
        Ok(res)
    }
}

/// Provides a general container for force fields.
///
/// At the moment, it contains only techniques and extra elements.
#[derive(Clone, Default, Debug)]
pub struct ForceField {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Declares the information used to process some portion of the content.
    /// This field is always nonempty, because the spec provides no common data
    /// for `force_field` elements.
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for ForceField {
    const NAME: &'static str = "force_field";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(ForceField {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            technique: Technique::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A data array element.
#[derive(Clone, Debug)]
pub enum ArrayElement {
    /// Stores a homogenous array of ID reference values.
    IdRef(Box<[String]>),
    /// Stores a homogenous array of symbolic name values.
    Name(Box<[String]>),
    /// Stores a homogenous array of Boolean values.
    Bool(Box<[bool]>),
    /// Stores a homogenous array of floating-point values.
    Float(Box<[f32]>),
    /// Stores a homogenous array of integer values.
    Int(Box<[u32]>),
}

fn parse_array_count<T: FromStr>(e: &Element) -> Result<Box<[T]>> {
    let count: usize = parse_attr(e.attr("count"))?.ok_or("expected 'count' attr")?;
    let mut vec = Vec::with_capacity(count);
    for s in get_text(e)
        .ok_or("expected text node")?
        .split_ascii_whitespace()
    {
        vec.push(s.parse().map_err(|_| "parse error")?)
    }
    if vec.len() != count {
        return Err("'count' does not match array length".into());
    }
    Ok(vec.into())
}

impl ArrayElement {
    /// Parse an [`ArrayElement`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            "IDREF_array" => Self::IdRef(parse_array_count(e)?),
            "Name_array" => Self::Name(parse_array_count(e)?),
            "bool_array" => Self::Bool(parse_array_count(e)?),
            "float_array" => Self::Float(parse_array_count(e)?),
            "int_array" => Self::Int(parse_array_count(e)?),
            _ => return Ok(None),
        }))
    }
}

/// Declares a data repository that provides values
/// according to the semantics of an [`Input`] element that refers to it.
#[derive(Clone, Debug)]
pub struct Source {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// A data array element.
    pub array: Option<ArrayElement>,
    /// The access pattern into the data element.
    pub accessor: Accessor,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
}

impl XNode for Source {
    const NAME: &'static str = "source";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Source {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            array: parse_opt_many(&mut it, ArrayElement::parse)?,
            accessor: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(Accessor::parse_one(&mut it)?, it)
            })?,
            technique: Technique::parse_list(&mut it)?,
        };
        finish(res, it)
    }
}

/// Describes a stream of values from an array data source.
#[derive(Clone, Debug)]
pub struct Accessor {
    /// The number of times the array is accessed.
    pub count: usize,
    /// The index of the first value to be read from the array.
    pub offset: usize,
    /// The location of the array to access using a URI expression.
    /// This element may refer to a COLLADA array element or to an
    /// array data source outside the scope of the document;
    /// the source does not need to be a COLLADA document.
    pub source: Url,
    /// The number of values that are to be considered a unit during each access to the array.
    /// The default is 1, indicating that a single value is accessed.
    pub stride: usize,
    /// The list of accesses.
    pub param: Vec<Param>,
}

impl XNode for Accessor {
    const NAME: &'static str = "accessor";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Accessor {
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            offset: parse_attr(element.attr("offset"))?.unwrap_or(0),
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            stride: parse_attr(element.attr("stride"))?.unwrap_or(1),
            param: Param::parse_list(&mut it)?,
        };
        if res.stride < res.param.len() {
            return Err("accessor stride does not match params".into());
        }
        finish(res, it)
    }
}

mk_extensible_enum! {
    /// An [`Input`] semantic attribute. Common / defined values are pre-parsed,
    /// and the remainder are in the `Other` variant.
    pub enum Semantic {
        /// Geometric binormal (bitangent) vector
        Binormal = "BINORMAL",
        /// Color coordinate vector. Color inputs are RGB (float3)
        Color = "COLOR",
        /// Continuity constraint at the control vertex (CV)
        Continuity = "CONTINUITY",
        /// Raster or MIP-level input
        Image = "IMAGE",
        /// Sampler input
        Input = "INPUT",
        /// Tangent vector for preceding control point
        InTangent = "IN_TANGENT",
        /// Sampler interpolation type
        Interpolation = "INTERPOLATION",
        /// Inverse of local-to-world matrix
        InvBindMatrix = "INV_BIND_MATRIX",
        /// Skin influence identifier
        Joint = "JOINT",
        /// Number of piece-wise linear approximation steps to use for the spline segment that
        /// follows this CV
        LinearSteps = "LINEAR_STEPS",
        /// Morph targets for mesh morphing
        MorphTarget = "MORPH_TARGET",
        /// Weights for mesh morphing
        MorphWeight = "MORPH_WEIGHT",
        /// Normal vector
        Normal = "NORMAL",
        /// Sampler output
        Output = "OUTPUT",
        /// Tangent vector for succeeding control point
        OutTangent = "OUT_TANGENT",
        /// Geometric coordinate vector
        Position = "POSITION",
        /// Geometric tangent vector
        Tangent = "TANGENT",
        /// Texture binormal (bitangent) vector
        TexBinormal = "TEXBINORMAL",
        /// Texture coordinate vector
        TexCoord = "TEXCOORD",
        /// Texture coordinate vector
        TexTangent = "TEXTANGENT",
        /// Generic parameter vector
        UV = "UV",
        /// Mesh vertex
        Vertex = "VERTEX",
        /// Skin influence weighting value
        Weight = "WEIGHT",
    }
}

/// Declares the input semantics of a data source and connects a consumer to that source.
/// In the COLLADA spec this is called "`<input>` (unshared)".
#[derive(Clone, Debug)]
pub struct Input {
    /// The user-defined meaning of the input connection.
    pub semantic: Semantic,
    /// The location of the data source.
    pub source: Url,
}

impl XNode for Input {
    const NAME: &'static str = "input";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Input {
            semantic: parse_attr(element.attr("semantic"))?.ok_or("missing semantic attr")?,
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
        })
    }
}

/// Declares the input semantics of a data source and connects a consumer to that source.
/// In the COLLADA spec this is called "`<input>` (shared)".
#[derive(Clone, Debug)]
pub struct InputS {
    /// [`InputS`] inherits from [`Input`].
    pub input: Input,
    /// The offset into the list of indices defined by the parent element’s `prim` field.
    /// If two [`InputS`] elements share the same offset, they are indexed the same.
    /// This is a simple form of compression for the list of indices
    /// and also defines the order in which the inputs are used.
    pub offset: u32,
    /// Which inputs to group as a single set. This is helpful when multiple inputs
    /// share the same semantics.
    pub set: Option<u32>,
}

impl Deref for InputS {
    type Target = Input;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl XNode for InputS {
    const NAME: &'static str = "input";
    fn parse(element: &Element) -> Result<Self> {
        Ok(InputS {
            input: Input::parse(element)?,
            offset: parse_attr(element.attr("offset"))?.ok_or("missing offset attr")?,
            set: parse_attr(element.attr("set"))?,
        })
    }
}

/// Wraps a group of inputs and precalculates the `depth` field.
#[derive(Clone, Default, Debug)]
pub struct InputList {
    /// The list of inputs.
    pub inputs: Vec<InputS>,
    /// The depth of an input list is the largest offset plus one;
    /// this is the "stride" of accesses applied by these inputs.
    pub depth: usize,
}

impl std::ops::Deref for InputList {
    type Target = Vec<InputS>;

    fn deref(&self) -> &Self::Target {
        &self.inputs
    }
}

impl InputList {
    fn parse<const N: usize>(it: &mut ElementIter<'_>) -> Result<Self> {
        let inputs = InputS::parse_list_n::<N>(it)?;
        let depth = inputs.iter().map(|i| i.offset).max().map_or(0, |n| n + 1) as usize;
        Ok(InputList { inputs, depth })
    }

    fn check_prim<const MIN: usize>(&self, data: &[u32]) -> bool {
        self.depth != 0 && data.len() < self.depth * MIN && data.len() % self.depth == 0
    }
}

/// Declares the attributes and identity of mesh-vertices.
#[derive(Clone, Debug)]
pub struct Vertices {
    /// A text string containing the unique identifier of the element.
    /// This value must be unique within the document.
    pub id: String,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The list of inputs.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Position`] input (which must exist).
    pub position: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Vertices {
    const NAME: &'static str = "vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        Ok(Vertices {
            id: element.attr("id").ok_or("missing 'id' attr")?.into(),
            name: element.attr("name").map(Into::into),
            position: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Position)
                .ok_or("vertices: missing POSITION input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// The common data for the geometry types:
///
/// * [`Lines`]` = Geom<`[`LineGeom`]`>`
/// * [`LineStrips`]` = Geom<`[`LineStripGeom`]`>`
/// * [`Polygons`]` = Geom<`[`PolygonGeom`]`>`
/// * [`PolyList`]` = Geom<`[`PolyListGeom`]`>`
/// * [`Triangles`]` = Geom<`[`TriangleGeom`]`>`
/// * [`TriFans`]` = Geom<`[`TriFanGeom`]`>`
/// * [`TriStrips`]` = Geom<`[`TriStripGeom`]`>`
#[derive(Clone, Default, Debug)]
pub struct Geom<T> {
    /// The text string name of this element.
    pub name: Option<String>,
    /// Declares a symbol for a material.
    /// This symbol is bound to a material at the time of instantiation;
    /// see [`Instance<Geometry>`] and [`BindMaterial`].
    /// If not specified then the lighting and shading results are application defined.
    pub material: Option<String>,
    /// The number of line/triangle/polygon primitives.
    pub count: usize,
    /// The vertex attribute access information.
    pub inputs: InputList,
    /// The specific data for
    pub data: T,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

/// The trait for types that can appear in a [`Geom<T>`].
pub trait ParseGeom: Default {
    /// The name of the element for the enclosing `Geom`, for example
    /// `"lines"` for [`LineGeom`].
    const NAME: &'static str;

    /// Parse the data from an element iterator.
    fn parse(it: &mut ElementIter<'_>) -> Result<Self>;

    /// Perform custom validation on the resulting [`Geom<T>`] before yielding it.
    fn validate(_: &Geom<Self>) -> Result<()>;
}

impl<T: ParseGeom> XNode for Geom<T> {
    const NAME: &'static str = T::NAME;

    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Geom {
            name: element.attr("name").map(Into::into),
            material: element.attr("material").map(Into::into),
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            inputs: InputList::parse::<0>(&mut it)?,
            data: T::parse(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        T::validate(&res)?;
        Ok(res)
    }
}

/// The data for a [`Lines`] element.
///
/// Each line described by the mesh has two vertices.
/// The first line is formed from the first and second vertices.
/// The second line is formed from the third and fourth vertices, and so on.
#[derive(Clone, Default, Debug)]
pub struct LineGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of individual lines.
    pub prim: Option<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual lines.
pub type Lines = Geom<LineGeom>;

impl std::ops::Deref for LineGeom {
    type Target = Option<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for LineGeom {
    const NAME: &'static str = "lines";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineGeom {
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = *res.data {
            if res.inputs.depth * 2 * res.count != data.len() {
                return Err("line count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

/// The data for a [`LineStrips`] element.
///
/// Each line-strip described by the mesh has an arbitrary number of vertices.
/// Each line segment within the line-strip is formed from the current vertex and the preceding vertex.
#[derive(Clone, Default, Debug)]
pub struct LineStripGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected line segments.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed to bind vertex attributes together and then organize those vertices into
/// connected line-strips.
pub type LineStrips = Geom<LineStripGeom>;

impl std::ops::Deref for LineStripGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for LineStripGeom {
    const NAME: &'static str = "line_strips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineStripGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("line strip count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<2>(p)) {
            return Err("incorrect <p> field in line strips".into());
        }
        Ok(())
    }
}

/// The data for an individual polygon-with-hole.
#[derive(Clone, Debug)]
pub struct PolygonHole {
    /// The vertex data for the polygon.
    pub verts: Box<[u32]>,
    /// A list of 0 or more holes, each of which describes a polygonal hole
    /// in the main polygon.
    pub hole: Vec<Box<[u32]>>,
}

/// The data for a [`Polygons`] element.
#[derive(Clone, Default, Debug)]
pub struct PolygonGeom(
    /// The list of polygons, each of which may contain 0 or more holes.
    pub Vec<PolygonHole>,
);

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual polygons.
pub type Polygons = Geom<PolygonGeom>;

impl std::ops::Deref for PolygonGeom {
    type Target = Vec<PolygonHole>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ParseGeom for PolygonGeom {
    const NAME: &'static str = "polygon";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        let mut polys = parse_list("p", it, |e| {
            Ok(PolygonHole {
                verts: parse_array(e)?,
                hole: vec![],
            })
        })?;
        let more_polys = parse_list("ph", it, |e| {
            let mut it = e.children().peekable();
            let verts = parse_one("p", &mut it, parse_array)?;
            let hole = parse_list("h", &mut it, parse_array)?;
            if hole.is_empty() {
                return Err(
                    "<ph> element can only be used when at least one hole is present".into(),
                );
            }
            finish(PolygonHole { verts, hole }, it)
        })?;
        polys.extend(more_polys);
        Ok(PolygonGeom(polys))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("polygon count does not match <p> fields".into());
        }
        if !res.data.iter().all(|ph| {
            res.inputs.check_prim::<3>(&ph.verts)
                && ph.hole.iter().all(|h| res.inputs.check_prim::<3>(h))
        }) {
            return Err("incorrect <p> field in polygon".into());
        }
        Ok(())
    }
}

/// The data for a [`PolyList`] element.
#[derive(Clone, Default, Debug)]
pub struct PolyListGeom {
    /// Contains a list of integers, each specifying the number of
    /// vertices for one polygon described by the [`PolyList`] element.
    pub vcount: Option<Box<[u32]>>,
    /// Contains a list of integers that specify the vertex attributes
    /// (indices) for an individual polylist.
    /// The winding order of vertices produced is counter-clockwise
    /// and describes the front side of each polygon.
    pub prim: Option<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual polygons.
pub type PolyList = Geom<PolyListGeom>;

fn validate_vcount<T>(
    count: usize,
    depth: usize,
    vcount: Option<&[u32]>,
    prim: Option<&[T]>,
) -> Result<()> {
    match (vcount, prim) {
        (None, None) => {}
        (Some(vcount), Some(data)) => {
            if count != vcount.len() {
                return Err("count does not match <vcount> field".into());
            }
            if depth * vcount.iter().sum::<u32>() as usize != data.len() {
                return Err("vcount does not match <p>/<v> field".into());
            }
        }
        _ => return Err("<vcount> and <p>/<v> should be provided together".into()),
    }
    Ok(())
}

impl ParseGeom for PolyListGeom {
    const NAME: &'static str = "polylist";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(PolyListGeom {
            vcount: parse_opt("vcount", it, parse_array)?,
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        validate_vcount(
            res.count,
            res.inputs.depth,
            res.data.vcount.as_deref(),
            res.data.prim.as_deref(),
        )
    }
}

/// The data for a [`Triangles`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// The second triangle is formed from the fourth, fifth, and sixth vertices, and so on.
#[derive(Clone, Default, Debug)]
pub struct TriangleGeom {
    /// Contains indices that describe the vertex attributes for a number of triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Option<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual triangles.
pub type Triangles = Geom<TriangleGeom>;

impl std::ops::Deref for TriangleGeom {
    type Target = Option<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriangleGeom {
    const NAME: &'static str = "triangles";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriangleGeom {
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = *res.data {
            if res.inputs.depth * 3 * res.count != data.len() {
                return Err("triangle count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

/// The data for a [`TriFans`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// Each subsequent triangle is formed from the current vertex,
/// reusing the first and the previous vertices.
#[derive(Clone, Default, Debug)]
pub struct TriFanGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into connected triangles.
pub type TriFans = Geom<TriFanGeom>;

impl std::ops::Deref for TriFanGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriFanGeom {
    const NAME: &'static str = "trifans";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriFanGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("triangle fan count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle fans".into());
        }
        Ok(())
    }
}

/// The data for a [`TriStrips`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// Each subsequent triangle is formed from the current vertex,
/// reusing the previous two vertices.
#[derive(Clone, Default, Debug)]
pub struct TriStripGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into connected triangles.
pub type TriStrips = Geom<TriStripGeom>;

impl std::ops::Deref for TriStripGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriStripGeom {
    const NAME: &'static str = "tristrips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriStripGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("triangle strip count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle strips".into());
        }
        Ok(())
    }
}

/// A collection of primitive elements.
#[derive(Clone, Debug)]
pub enum Primitive {
    /// Line primitives.
    Lines(Lines),
    /// Line-strip primitives.
    LineStrips(LineStrips),
    /// Polygon primitives which may contain holes.
    Polygons(Polygons),
    /// Polygon primitives that cannot contain holes.
    PolyList(PolyList),
    /// Triangle primitives.
    Triangles(Triangles),
    /// Triangle-fan primitives.
    TriFans(TriFans),
    /// Triangle-strip primitives.
    TriStrips(TriStrips),
}

impl Primitive {
    /// Parse a [`Primitive`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            LineGeom::NAME => Primitive::Lines(Geom::parse(e)?),
            LineStripGeom::NAME => Primitive::LineStrips(Geom::parse(e)?),
            PolygonGeom::NAME => Primitive::Polygons(Geom::parse(e)?),
            PolyListGeom::NAME => Primitive::PolyList(Geom::parse(e)?),
            TriangleGeom::NAME => Primitive::Triangles(Geom::parse(e)?),
            TriFanGeom::NAME => Primitive::TriFans(Geom::parse(e)?),
            TriStripGeom::NAME => Primitive::TriStrips(Geom::parse(e)?),
            _ => return Ok(None),
        }))
    }
}

/// Describes basic geometric meshes using vertex and primitive information.
#[derive(Clone, Debug)]
pub struct Mesh {
    /// If true, this is a `<convex_mesh>` element.
    /// Both elements have the same structure otherwise.
    pub convex: bool,
    /// Provides the bulk of the mesh’s vertex data.
    pub source: Vec<Source>,
    /// Describes the mesh-vertex attributes and establishes their topological identity.
    pub vertices: Option<Vertices>,
    /// Geometric primitives, which assemble values from the
    /// inputs into vertex attribute data.
    pub elements: Vec<Primitive>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl Mesh {
    /// The name of the XML node: `convex_mesh`
    pub const CONVEX: &'static str = "convex_mesh";

    /// Parse a `<convex_mesh>` XML element.
    pub fn parse_convex(element: &Element) -> Result<GeometryElement> {
        debug_assert_eq!(element.name(), Self::CONVEX);
        if let Some(s) = parse_attr(element.attr("convex_hull_of"))? {
            return Ok(GeometryElement::ConvexHullOf(s));
        }
        Ok(GeometryElement::Mesh(Mesh::parse(true, element)?))
    }

    /// Parse a [`Mesh`] from an XML element of type `<convex_mesh>` or `<mesh>`.
    pub fn parse(convex: bool, element: &Element) -> Result<Self> {
        debug_assert_eq!(
            element.name(),
            if convex { Self::CONVEX } else { Self::NAME }
        );
        let mut it = element.children().peekable();
        Ok(Mesh {
            convex,
            source: Source::parse_list_n::<1>(&mut it)?,
            vertices: Vertices::parse_opt(&mut it)?,
            elements: parse_list_many(&mut it, Primitive::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNode for Mesh {
    const NAME: &'static str = "mesh";
    fn parse(element: &Element) -> Result<Self> {
        Self::parse(false, element)
    }
}

/// Describes the control vertices (CVs) of a spline.
#[derive(Clone, Debug)]
pub struct ControlVertices {
    /// The list of inputs.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Position`] input (which must exist).
    pub position: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for ControlVertices {
    const NAME: &'static str = "control_vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        Ok(ControlVertices {
            position: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Position)
                .ok_or("control_vertices: missing POSITION input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes a multisegment spline with control vertex (CV) and segment information.
#[derive(Clone, Debug)]
pub struct Spline {
    /// Whether there is a segment connecting the first and last control vertices.
    /// The default is "false", indicating that the spline is open.
    pub closed: bool,
    /// Provides the values for the CVs and segments of the spline.
    pub source: Vec<Source>,
    /// Describes the CVs of the spline.
    pub controls: ControlVertices,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Spline {
    const NAME: &'static str = "spline";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Spline {
            closed: parse_attr(element.attr("closed"))?.unwrap_or(false),
            source: Source::parse_list_n::<1>(&mut it)?,
            controls: ControlVertices::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// An element that describes geometric data.
#[derive(Clone, Debug)]
pub enum GeometryElement {
    /// The parameter is a URI string of a `Geometry`, and this element
    /// describes the convex hull of the specified mesh.
    ConvexHullOf(Url),
    /// A mesh or convex mesh.
    Mesh(Mesh),
    /// A multisegment spline.
    Spline(Spline),
}

impl GeometryElement {
    /// Parse a [`GeometryElement`] from an XML element.
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            Mesh::CONVEX => Mesh::parse_convex(element)?,
            Mesh::NAME => GeometryElement::Mesh(Mesh::parse(false, element)?),
            Spline::NAME => GeometryElement::Spline(Spline::parse(element)?),
            _ => return Ok(None),
        }))
    }
}

/// Describes the visual shape and appearance of an object in a scene.
#[derive(Clone, Debug)]
pub struct Geometry {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The element that describes geometric data.
    pub element: GeometryElement,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Geometry {
    const NAME: &'static str = "geometry";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Geometry {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            element: parse_one_many(&mut it, GeometryElement::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A specification of the source of data for an image.
#[derive(Clone, Debug)]
pub enum ImageSource {
    /// The data is provided directly as a byte buffer.
    Data(Box<[u8]>),
    /// A URI that specifies an external image file.
    InitFrom(Url),
}

fn parse_hex_array(s: &str) -> Box<[u8]> {
    let mut out = vec![];
    let mut hi = 0;
    let mut odd = false;
    for c in s.bytes() {
        let c = match c {
            b'0'..=b'9' => c - b'0',
            b'a'..=b'f' => c - b'a' + 10,
            b'A'..=b'F' => c - b'A' + 10,
            _ => continue,
        };
        if odd {
            out.push(hi << 4 | c)
        } else {
            hi = c
        }
        odd = !odd
    }
    out.into()
}

impl ImageSource {
    /// Parse an [`ImageSource`] from an XML element.
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            "data" => {
                let s = get_text(element).ok_or("expected text element")?;
                ImageSource::Data(parse_hex_array(s))
            }
            "init_from" => ImageSource::InitFrom(parse_elem(element)?),
            _ => return Ok(None),
        }))
    }
}

/// Declares the storage for the graphical representation of an object.
#[derive(Clone, Debug)]
pub struct Image {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// A text string value that indicates the image format.
    /// It describes the encoding of the image in [`ImageSource::Data`]
    /// or the format of the image referenced by [`ImageSource::InitFrom`]
    /// if it is in a nonstandard format that cannot be identified by its file extension.
    /// For example, if [`ImageSource::Data`] in a COLLADA document
    /// contains the digital contents of a JPEG file, then set this attribute to "JPG".
    pub format: Option<String>,
    /// An integer value that indicates the height of the image in pixels.
    /// A value of 0 means the value is omitted.
    pub height: u32,
    /// An integer value that indicates the width of the image in pixels.
    /// A value of 0 means the value is omitted.
    pub width: u32,
    /// An integer value that indicates the depth of the image in pixels.
    /// A 2-D image has a depth of 1, which is the default.
    pub depth: u32,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// This specifies either embedded image data or an external image file.
    pub source: ImageSource,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Image {
    const NAME: &'static str = "image";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Image {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            format: element.attr("format").map(Into::into),
            height: parse_attr(element.attr("height"))?.unwrap_or(0),
            width: parse_attr(element.attr("width"))?.unwrap_or(0),
            depth: parse_attr(element.attr("depth"))?.unwrap_or(1),
            asset: Asset::parse_opt_box(&mut it)?,
            source: parse_one_many(&mut it, ImageSource::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes an ambient light source.
#[derive(Clone, Debug)]
pub struct AmbientLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
}

impl XNode for AmbientLight {
    const NAME: &'static str = "ambient";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let color = parse_one("color", &mut it, parse_array_n)?;
        finish(AmbientLight { color }, it)
    }
}

/// Describes a directional light source.
///
/// The light’s default direction vector in local coordinates is [0,0,-1],
/// pointing down the negative z axis.
/// The actual direction of the light is defined by the transform of the node
/// where the light is instantiated.
#[derive(Clone, Debug)]
pub struct DirectionalLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
}

impl XNode for DirectionalLight {
    const NAME: &'static str = "directional";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let color = parse_one("color", &mut it, parse_array_n)?;
        finish(DirectionalLight { color }, it)
    }
}

/// Describes a point light source.
///
/// The position of the light is defined by the transform of the node in which it is instantiated.
///
/// The `constant_attenuation`, `linear_attenuation`, and `quadratic_attenuation` are
/// used to calculate the total attenuation of this light given a distance.
/// The equation used is:
/// ```text
/// A = constant_attenuation + Dist * linear_attenuation + Dist^2 * quadratic_attenuation
/// ```
#[derive(Clone, Debug)]
pub struct PointLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
    /// The constant term in the attentuation equation, see [`PointLight`].
    pub constant_attenuation: f32,
    /// The linear term in the attentuation equation, see [`PointLight`].
    pub linear_attenuation: f32,
    /// The quadratic term in the attentuation equation, see [`PointLight`].
    pub quadratic_attenuation: f32,
}

impl XNode for PointLight {
    const NAME: &'static str = "point";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = PointLight {
            color: parse_one("color", &mut it, parse_array_n)?,
            constant_attenuation: parse_opt("constant_attenuation", &mut it, parse_elem)?
                .unwrap_or(0.),
            linear_attenuation: parse_opt("linear_attenuation", &mut it, parse_elem)?.unwrap_or(0.),
            quadratic_attenuation: parse_opt("quadratic_attenuation", &mut it, parse_elem)?
                .unwrap_or(0.),
        };
        finish(res, it)
    }
}

/// Describes a spot light source.
///
/// The light’s default direction vector in local coordinates is [0,0,-1],
/// pointing down the negative z axis.
/// The actual direction of the light is defined by the transform of the node
/// where the light is instantiated.
///
/// The `constant_attenuation`, `linear_attenuation`, and `quadratic_attenuation` are
/// used to calculate the total attenuation of this light given a distance.
/// The equation used is:
/// ```text
/// A = constant_attenuation + Dist * linear_attenuation + Dist^2 * quadratic_attenuation
/// ```
///
/// The `falloff_angle` and `falloff_exponent` are used to specify
/// the amount of attenuation based on the direction of the light.
#[derive(Clone, Debug)]
pub struct SpotLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
    /// The constant term in the attentuation equation, see [`SpotLight`].
    pub constant_attenuation: f32,
    /// The linear term in the attentuation equation, see [`SpotLight`].
    pub linear_attenuation: f32,
    /// The quadratic term in the attentuation equation, see [`SpotLight`].
    pub quadratic_attenuation: f32,
    /// The directional attenuation of the light.
    pub falloff_angle: f32,
    /// A term in the directional attenuation equation of the light.
    pub falloff_exponent: f32,
}

impl XNode for SpotLight {
    const NAME: &'static str = "spot";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = SpotLight {
            color: parse_one("color", &mut it, parse_array_n)?,
            constant_attenuation: parse_opt("constant_attenuation", &mut it, parse_elem)?
                .unwrap_or(0.),
            linear_attenuation: parse_opt("linear_attenuation", &mut it, parse_elem)?.unwrap_or(0.),
            quadratic_attenuation: parse_opt("quadratic_attenuation", &mut it, parse_elem)?
                .unwrap_or(0.),
            falloff_angle: parse_opt("falloff_angle", &mut it, parse_elem)?.unwrap_or(180.),
            falloff_exponent: parse_opt("falloff_exponent", &mut it, parse_elem)?.unwrap_or(0.),
        };
        finish(res, it)
    }
}

/// The kind of light being described.
#[derive(Clone, Debug)]
pub enum LightKind {
    /// Describes an ambient light source.
    Ambient(AmbientLight),
    /// Describes a directional light source.
    Directional(DirectionalLight),
    /// Describes a point light source.
    Point(Box<PointLight>),
    /// Describes a spot light source.
    Spot(Box<SpotLight>),
}

impl LightKind {
    /// Parse a [`LightKind`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            AmbientLight::NAME => Self::Ambient(AmbientLight::parse(e)?),
            DirectionalLight::NAME => Self::Directional(DirectionalLight::parse(e)?),
            PointLight::NAME => Self::Point(PointLight::parse_box(e)?),
            SpotLight::NAME => Self::Spot(SpotLight::parse_box(e)?),
            _ => return Ok(None),
        }))
    }
}

/// Declares a light source that illuminates a scene.
#[derive(Clone, Debug)]
pub struct Light {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The kind of light being described.
    pub kind: LightKind,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Light {
    const NAME: &'static str = "light";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Light {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            kind: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_one_many(&mut it, LightKind::parse)?, it)
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes the visual appearance of a geometric object.
#[derive(Clone, Debug)]
pub struct Material {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Instantiates a COLLADA material resource. See [`InstanceEffectData`]
    /// for the additional instance effect data.
    pub instance_effect: Instance<Effect>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Material {
    const NAME: &'static str = "material";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Material {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            instance_effect: Instance::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies physics-material information for the common
/// profile that all COLLADA implementations must support.
#[derive(Clone, Debug)]
pub struct PhysicsMaterialCommon {
    /// Contains a floating-point number that specifies the dynamic friction coefficient.
    pub dynamic_friction: f32,
    /// Contains a floating-point number that is the proportion
    /// of the kinetic energy preserved in the impact (typically ranges from 0.0 to 1.0).
    /// Also known as "bounciness" or "elasticity."
    pub restitution: f32,
    /// Contains a floating-point number that specifies the static friction coefficient.
    pub static_friction: f32,
}

impl PhysicsMaterialCommon {
    fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = PhysicsMaterialCommon {
            dynamic_friction: parse_opt("dynamic_friction", &mut it, parse_elem)?.unwrap_or(0.),
            restitution: parse_opt("restitution", &mut it, parse_elem)?.unwrap_or(0.),
            static_friction: parse_opt("static_friction", &mut it, parse_elem)?.unwrap_or(0.),
        };
        finish(res, it)
    }
}

/// Defines the physical properties of an object.
///
/// It contains a technique/profile with parameters.
/// The `COMMON` profile defines the built-in names, such as `static_friction`.
#[derive(Clone, Debug)]
pub struct PhysicsMaterial {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Specifies physics-material information for the common
    /// profile that all COLLADA implementations must support.
    pub common: PhysicsMaterialCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for PhysicsMaterial {
    const NAME: &'static str = "physics_material";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(PhysicsMaterial {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            common: parse_one(Technique::COMMON, &mut it, PhysicsMaterialCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies rigid-body information for the common profile
/// that every COLLADA implmentation must support.
#[derive(Clone, Default, Debug)]
pub struct RigidBodyCommon {
    /// A Boolean that specifies whether the [`RigidBody`] is movable, if present.
    pub dynamic: Option<bool>,
    /// Contains a floating-point value that specifies the total mass of the [`RigidBody`].
    pub mass: Option<f32>,
    /// Defines the center and orientation of mass of the [`RigidBody`] relative
    /// to the local origin of the "root" shape.
    /// This makes the off-diagonal elements of the inertia tensor
    /// (products of inertia) all 0 and allows us to just store the diagonal elements
    /// (moments of inertia).
    pub mass_frame: Vec<RigidTransform>,
    /// Contains three floating-point numbers, which are the diagonal elements of the
    /// inertia tensor (moments of inertia), which is represented in the local frame
    /// of the center of mass. See `mass_frame`.
    pub inertia: Option<Box<[f32; 3]>>,
    /// Defines or references a [`PhysicsMaterial`] for the [`RigidBody`].
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
    /// The components of the [`RigidBody`].
    pub shape: Vec<Shape>,
}

impl RigidBodyCommon {
    fn parse(mut it: ElementIter<'_>) -> Result<Self> {
        let res = Self {
            dynamic: parse_opt("dynamic", &mut it, parse_elem)?,
            mass: parse_opt("mass", &mut it, parse_elem)?,
            mass_frame: parse_opt("mass_frame", &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_list_many(&mut it, RigidTransform::parse)?, it)
            })?
            .unwrap_or_default(),
            inertia: parse_opt("inertia", &mut it, parse_array_n)?,
            physics_material: parse_opt_many(&mut it, DefInstance::parse)?.map(Box::new),
            shape: Shape::parse_list(&mut it)?,
        };
        finish(res, it)
    }
}

/// Describes simulated bodies that do not deform.
///
/// These bodies may or may not be connected by constraints (hinge, ball-joint, and so on).
#[derive(Clone, Debug)]
pub struct RigidBody {
    /// A text string containing the scoped identifier of the [`RigidBody`] element.
    /// This value must be unique among its sibling elements.
    /// Associates each rigid body with a visual [`Node`] when a [`PhysicsModel`]
    /// is instantiated.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Specifies rigid-body information for the common profile
    /// that every COLLADA implmentation must support.
    pub common: RigidBodyCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl std::ops::Deref for RigidBody {
    type Target = RigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl XNode for RigidBody {
    const NAME: &'static str = "rigid_body";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(RigidBody {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            common: parse_one(Technique::COMMON, &mut it, |e| {
                RigidBodyCommon::parse(e.children().peekable())
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies the rigid-body information for the common
/// profile that all COLLADA implementations must support.
#[derive(Clone, Default, Debug)]
pub struct InstanceRigidBodyCommon {
    /// Contains three floating-point values that
    /// specify the initial angular velocity of the
    /// rigid_body instance around each axis, in
    /// the form of an x-y-z Euler rotation. The
    /// measurement is in degrees per second.
    pub angular_velocity: [f32; 3],
    /// Contains three floating-point values that specify
    /// the initial linear velocity of the [`RigidBody`] instance.
    pub velocity: [f32; 3],
    /// Additional fields are inherited from [`RigidBodyCommon`].
    pub common: RigidBodyCommon,
}

impl std::ops::Deref for InstanceRigidBodyCommon {
    type Target = RigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl InstanceRigidBodyCommon {
    /// Parse a [`InstanceRigidBodyCommon`] from a
    /// `<instance_rigid_body>/<technique_common>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        Ok(Self {
            angular_velocity: parse_opt("angular_velocity", &mut it, parse_array_n)?
                .map_or([0.; 3], |a| *a),
            velocity: parse_opt("velocity", &mut it, parse_array_n)?.map_or([0.; 3], |a| *a),
            common: RigidBodyCommon::parse(it)?,
        })
    }
}

/// Instantiates an object described by a [`RigidBody`] within an [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstanceRigidBody {
    /// Which [`RigidBody`] to instantiate.
    pub body: String,
    /// Which [`Node`] is influenced by this [`RigidBody`] instance.
    /// Can refer to a local instance or external reference.
    pub target: Url,
    /// Specifies the rigid-body information for the common
    /// profile that all COLLADA implementations must support.
    pub common: InstanceRigidBodyCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl std::ops::Deref for InstanceRigidBody {
    type Target = InstanceRigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl XNode for InstanceRigidBody {
    const NAME: &'static str = "rigid_body";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(InstanceRigidBody {
            body: element.attr("body").ok_or("missing body attribute")?.into(),
            target: parse_attr(element.attr("target"))?.ok_or("missing url attribute")?,
            common: parse_one(Technique::COMMON, &mut it, InstanceRigidBodyCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A [`RigidBody`] transform is a subset of the full set of [`Transform`]s
/// which restricts to euclidean transformations (translation and rotation).
#[derive(Clone, Debug)]
pub enum RigidTransform {
    /// A translation.
    Translate(Translate),
    /// A rotation.
    Rotate(Rotate),
}

impl RigidTransform {
    /// Parse a [`RigidTransform`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Translate::NAME => Ok(Some(Self::Translate(Translate::parse(e)?))),
            Rotate::NAME => Ok(Some(Self::Rotate(Rotate::parse(e)?))),
            _ => Ok(None),
        }
    }
}

/// Defines an infinite plane primitive.
#[derive(Clone, Debug)]
pub struct Plane {
    /// The coefficients for the plane’s equation: `Ax + By + Cz + D = 0`.
    pub equation: [f32; 4],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Plane {
    const NAME: &'static str = "plane";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Plane {
            equation: *parse_one("equation", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares an axis-aligned, centered box primitive.
/// (Note: The type is not called `Box` to avoid the name clash with the Rust builtin type.)
#[derive(Clone, Debug)]
pub struct BoxShape {
    /// The extents of the box.
    pub half_extents: [f32; 3],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for BoxShape {
    const NAME: &'static str = "box";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(BoxShape {
            half_extents: *parse_one("half_extents", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes a centered sphere primitive.
#[derive(Clone, Debug)]
pub struct Sphere {
    /// The radius of the sphere.
    pub radius: f32,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Sphere {
    const NAME: &'static str = "sphere";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Sphere {
            radius: parse_one("radius", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares a cylinder primitive that is centered on, and aligned with, the local y axis.
#[derive(Clone, Debug)]
pub struct Cylinder {
    /// The length of the cylinder along the y axis.
    pub height: f32,
    /// The radii of the cylinder (it may be elliptical).
    pub radius: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Cylinder {
    const NAME: &'static str = "cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Cylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: *parse_one("radius", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes a tapered cylinder primitive that is centered on and aligned with the local y axis.
#[derive(Clone, Debug)]
pub struct TaperedCylinder {
    /// The length of the cylinder along the y axis.
    pub height: f32,
    /// The radii of the tapered cylinder at the positive (height/2) Y value.
    /// Both ends of the tapered cylinder may be elliptical.
    pub radius1: [f32; 2],
    /// The radii of the tapered cylinder at the negative (height/2) Y value.
    /// Both ends of the tapered cylinder may be elliptical.
    pub radius2: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCylinder {
    const NAME: &'static str = "tapered_cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: *parse_one("radius1", &mut it, parse_array_n)?,
            radius2: *parse_one("radius2", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares a capsule primitive that is centered on and aligned with the local y axis.
#[derive(Clone, Debug)]
pub struct Capsule {
    /// The length of the line segment connecting the centers of the capping hemispheres.
    pub height: f32,
    /// The radii of the capsule (it may be elliptical).
    pub radius: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Capsule {
    const NAME: &'static str = "capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Capsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: *parse_one("radius", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes a tapered capsule primitive that is centered on, and aligned with, the local y axis.
#[derive(Clone, Debug)]
pub struct TaperedCapsule {
    /// The length of the line segment connecting the centers of the capping hemispheres.
    pub height: f32,
    /// The radii of the tapered capsule at the positive (height/2) Y value.
    /// Both ends of the tapered capsule may be elliptical.
    pub radius1: [f32; 2],
    /// The radii of the tapered capsule at the negative (height/2) Y value.
    /// Both ends of the tapered capsule may be elliptical.
    pub radius2: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCapsule {
    const NAME: &'static str = "tapered_capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCapsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: *parse_one("radius1", &mut it, parse_array_n)?,
            radius2: *parse_one("radius2", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// The geometry of a shape. This can be either an inline definition
/// using [`Plane`], [`Sphere`] etc, or a [`Instance<Geometry>`],
/// which can reference other geometry types.
#[derive(Clone, Debug)]
pub enum ShapeGeom {
    /// Defines an infinite plane primitive.
    Plane(Plane),
    /// Declares an axis-aligned, centered box primitive.
    Box(BoxShape),
    /// Describes a centered sphere primitive.
    Sphere(Sphere),
    /// Declares a cylinder primitive that is centered on, and aligned with, the local y axis.
    Cylinder(Cylinder),
    /// Describes a tapered cylinder primitive that is centered on and aligned with the local y axis.
    TaperedCylinder(TaperedCylinder),
    /// Declares a capsule primitive that is centered on and aligned with the local y axis.
    Capsule(Capsule),
    /// Describes a tapered capsule primitive that is centered on, and aligned with, the local y axis.
    TaperedCapsule(TaperedCapsule),
    /// A geometry instance using the [`Instance<Geometry>`] element, which
    /// references any [`GeometryElement`] ([`Mesh`] or [`Spline`]).
    Geom(Instance<Geometry>),
}

impl ShapeGeom {
    /// Parse a [`ShapeGeom`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Plane::NAME => Ok(Some(Self::Plane(Plane::parse(e)?))),
            BoxShape::NAME => Ok(Some(Self::Box(BoxShape::parse(e)?))),
            Sphere::NAME => Ok(Some(Self::Sphere(Sphere::parse(e)?))),
            Cylinder::NAME => Ok(Some(Self::Cylinder(Cylinder::parse(e)?))),
            TaperedCylinder::NAME => Ok(Some(Self::TaperedCylinder(TaperedCylinder::parse(e)?))),
            Capsule::NAME => Ok(Some(Self::Capsule(Capsule::parse(e)?))),
            TaperedCapsule::NAME => Ok(Some(Self::TaperedCapsule(TaperedCapsule::parse(e)?))),
            Geometry::INSTANCE => Ok(Some(Self::Geom(Instance::parse(e)?))),
            _ => Ok(None),
        }
    }
}

/// Describes components of a [`RigidBody`].
#[derive(Clone, Debug)]
pub struct Shape {
    /// If true, the mass is distributed along the surface of the shape.
    pub hollow: Option<bool>,
    /// The mass of the shape.
    /// If not provided, it is derived from density x shape volume.
    pub mass: Option<f32>,
    /// The density of the shape.
    /// If not provided, it is derived from mass/shape volume.
    pub density: Option<f32>,
    /// The [`PhysicsMaterial`] used for this shape.
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
    /// The geometry of the shape ([`Plane`], [`Sphere`], [`Mesh`], etc.).
    pub geom: ShapeGeom,
    /// Transformation for the shape. Any combination of these elements in any order.
    /// See [`Node`] for additional information.
    pub transform: Vec<RigidTransform>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Shape {
    const NAME: &'static str = "shape";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Shape {
            hollow: parse_opt("hollow", &mut it, parse_elem)?,
            mass: parse_opt("mass", &mut it, parse_elem)?,
            density: parse_opt("density", &mut it, parse_elem)?,
            physics_material: parse_opt_many(&mut it, DefInstance::parse)?.map(Box::new),
            transform: parse_list_many(&mut it, RigidTransform::parse)?,
            geom: parse_one_many(&mut it, ShapeGeom::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Defines an attachment frame, to a rigid body or a node, within a rigid constraint.
#[derive(Clone, Debug)]
pub struct Attachment {
    /// A URI reference to a [`RigidBody`] or [`Node`]. This must refer to a
    /// [`RigidBody`] in `attachment` or in `ref_attachment`; they cannot both be [`Node`]s.
    pub rigid_body: Url,
    /// Changes the position of the attachment point.
    pub transform: Vec<RigidTransform>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl Attachment {
    const REF: &'static str = "ref_attachment";
}
impl XNode for Attachment {
    const NAME: &'static str = "attachment";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert!(element.name() == Self::NAME || element.name() == Self::REF);
        let mut it = element.children().peekable();
        Ok(Attachment {
            rigid_body: parse_attr(element.attr("rigid_body"))?.ok_or("missing rigid_body attr")?,
            transform: parse_list_many(&mut it, RigidTransform::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// A set of min/max limits used for both linear and angular constraints.
#[derive(Clone, Default, Debug)]
pub struct Limits {
    /// The minimum value in each direction.
    pub min: Option<Box<[f32; 3]>>,
    /// The maximum value in each direction.
    pub max: Option<Box<[f32; 3]>>,
}

impl Limits {
    /// Parse a [`Limits`] from a `<swing_cone_and_twist>` or `<linear>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            min: parse_opt("min", &mut it, parse_array_n)?,
            max: parse_opt("max", &mut it, parse_array_n)?,
        };
        finish(res, it)
    }
}

/// A spring constraint, used for both linear and angular constraints.
#[derive(Clone, Debug)]
pub struct Spring {
    /// The `stiffness` (also called spring coefficient)
    /// has units of force/distance for `spring_linear`
    /// or force/angle in degrees for `spring_angular`.
    pub stiffness: f32,
    /// The damping coefficient of the spring.
    pub damping: f32,
    /// The resting position of the spring.
    pub target_value: f32,
}

impl Default for Spring {
    fn default() -> Self {
        Self {
            stiffness: 1.,
            damping: 0.,
            target_value: 0.,
        }
    }
}

impl Spring {
    /// Parse a [`Spring`] from a `<linear>` or `<angular>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            stiffness: parse_opt("stiffness", &mut it, parse_elem)?.unwrap_or(1.),
            damping: parse_opt("damping", &mut it, parse_elem)?.unwrap_or(0.),
            target_value: parse_opt("target_value", &mut it, parse_elem)?.unwrap_or(0.),
        };
        finish(res, it)
    }
}

/// Specifies rigid-constraint information for the common profile that all COLLADA
/// implementations must support.
#[derive(Clone, Default, Debug)]
pub struct RigidConstraintCommon {
    /// If false, the [`RigidConstraint`] doesn’t exert any force or influence on
    /// the rigid bodies.
    pub enabled: bool,
    /// If true, the attached rigid bodies may interpenetrate.
    pub interpenetrate: bool,
    /// Describes the angular limits along each rotation axis in degrees.
    pub swing_cone_and_twist: Limits,
    /// Describes linear (translational) limits along each axis.
    pub linear: Limits,
    /// Describes angular spring constraints.
    pub spring_angular: Spring,
    /// Describes linear spring constraints.
    pub spring_linear: Spring,
}

impl RigidConstraintCommon {
    /// Parse a [`RigidConstraintCommon`] from a
    /// `<rigid_constraint>/<technique_common>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let enabled = parse_opt("enabled", &mut it, parse_elem)?.unwrap_or(true);
        let interpenetrate = parse_opt("interpenetrate", &mut it, parse_elem)?.unwrap_or(false);
        let (swing_cone_and_twist, linear) = parse_opt("limits", &mut it, |e| {
            let mut it = e.children().peekable();
            let scat =
                parse_opt("swing_cone_and_twist", &mut it, Limits::parse)?.unwrap_or_default();
            let lin = parse_opt("linear", &mut it, Limits::parse)?.unwrap_or_default();
            finish((scat, lin), it)
        })?
        .unwrap_or_default();
        let (spring_angular, spring_linear) = parse_opt("spring", &mut it, |e| {
            let mut it = e.children().peekable();
            let ang = parse_opt("angular", &mut it, Spring::parse)?.unwrap_or_default();
            let lin = parse_opt("linear", &mut it, Spring::parse)?.unwrap_or_default();
            finish((ang, lin), it)
        })?
        .unwrap_or_default();
        let res = Self {
            enabled,
            interpenetrate,
            swing_cone_and_twist,
            linear,
            spring_angular,
            spring_linear,
        };
        finish(res, it)
    }
}

/// Connects components, such as [`RigidBody`], into complex physics models with moveable parts.
#[derive(Clone, Debug)]
pub struct RigidConstraint {
    /// A text string containing the scoped identifier of the [`RigidConstraint`]
    /// element. This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Defines the attachment frame of reference (to a [`RigidBody`] or a [`Node`])
    /// within a rigid constraint.
    pub ref_attachment: Attachment,
    /// Defines an attachment frame (to a [`RigidBody`] or a [`Node`])
    /// within a rigid constraint.
    pub attachment: Attachment,
    /// Specifies rigid-constraint information for the common profile that all COLLADA
    /// implementations must support.
    pub common: RigidConstraintCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for RigidConstraint {
    const NAME: &'static str = "rigid_constraint";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(RigidConstraint {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            ref_attachment: parse_one(Attachment::REF, &mut it, Attachment::parse)?,
            attachment: Attachment::parse_one(&mut it)?,
            common: parse_one(Technique::COMMON, &mut it, RigidConstraintCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Instantiates an object described by a [`RigidConstraint`]
/// within an [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstanceRigidConstraint {
    /// Which [`RigidConstraint`] to instantiate.
    pub constraint: String,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for InstanceRigidConstraint {
    const NAME: &'static str = "instance_rigid_constraint";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(InstanceRigidConstraint {
            constraint: element
                .attr("constraint")
                .ok_or("missing constraint attribute")?
                .into(),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

/// Extra data associated to [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstancePhysicsModelData {
    /// Points to the id of a node in the visual scene. This allows a physics model to be
    /// instantiated under a specific transform node, which will dictate the initial position
    /// and orientation, and could be animated to influence kinematic rigid bodies.
    pub parent: Option<Url>,
    /// Instantiates a [`ForceField`] element to influence this physics model.
    pub instance_force_field: Vec<Instance<ForceField>>,
    /// Instantiates a [`RigidBody`] element and allows for overriding some or all of its
    /// properties.
    ///
    /// The target attribute defines the [`Node`] element that has its transforms overwritten
    /// by this rigid-body instance.
    pub instance_rigid_body: Vec<InstanceRigidBody>,
    /// Instantiates a [`RigidConstraint`] element to override some of its properties.
    /// This element does not have a `target` field because its [`RigidConstraint`]
    /// children define which [`Node`] elements are targeted.
    pub instance_rigid_constraint: Vec<InstanceRigidConstraint>,
}

impl Instantiate for PhysicsModel {
    const INSTANCE: &'static str = "instance_physics_model";
    type Data = InstancePhysicsModelData;
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstancePhysicsModelData {
            parent: parse_attr(e.attr("parent"))?,
            instance_force_field: Instance::parse_list(it)?,
            instance_rigid_body: InstanceRigidBody::parse_list(it)?,
            instance_rigid_constraint: InstanceRigidConstraint::parse_list(it)?,
        })
    }
}

/// Allows for building complex combinations of rigid bodies and constraints
/// that may be instantiated multiple times.
#[derive(Clone, Debug)]
pub struct PhysicsModel {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Defines a [`RigidBody`] element and sets its nondefault properties.
    pub rigid_body: Vec<RigidBody>,
    /// Defines a [`RigidConstraint`] element and allows for overriding
    /// some or all of its properties.
    pub rigid_constraint: Vec<RigidConstraint>,
    /// Instantiates a physics model from the given url,
    /// and assigns an sid to it, to distinguish it from other child elements.
    pub instances: Vec<Instance<PhysicsModel>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for PhysicsModel {
    const NAME: &'static str = "physics_model";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(PhysicsModel {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            rigid_body: RigidBody::parse_list(&mut it)?,
            rigid_constraint: RigidConstraint::parse_list(&mut it)?,
            instances: Instance::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies physics-scene information for the common profile
/// that all COLLADA implementations must support.
#[derive(Clone, Default, Debug)]
pub struct PhysicsSceneCommon {
    /// A vector representation of the scene’s gravity force field.
    /// It is given as a denormalized direction vector of three
    /// floating-point values that indicate both the magnitude
    /// and direction of acceleration caused by the field.
    pub gravity: Option<Box<[f32; 3]>>,
    /// The integration time step, measured in seconds, of the physics scene.
    /// This value is engine-specific. If omitted, the physics engine’s default is used.
    pub time_step: Option<f32>,
}

impl PhysicsSceneCommon {
    /// Parse a [`PhysicsSceneCommon`] from a
    /// `<physics_scene>/<technique_common>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            gravity: parse_opt("gravity", &mut it, parse_array_n)?,
            time_step: parse_opt("time_step", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

/// Specifies an environment in which physical objects are instantiated and simulated.
#[derive(Clone, Debug)]
pub struct PhysicsScene {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Instantiates a [`ForceField`] element to influence this physics scene.
    pub instance_force_field: Vec<Instance<ForceField>>,
    /// Instantiates a [`PhysicsModel`] element and
    /// allows for overriding some or all of its children.
    pub instance_physics_model: Vec<Instance<PhysicsModel>>,
    /// Specifies physics-scene information for the common profile
    /// that all COLLADA implementations must support.
    pub common: PhysicsSceneCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for PhysicsScene {
    const NAME: &'static str = "physics_scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(PhysicsScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            instance_force_field: Instance::parse_list(&mut it)?,
            instance_physics_model: Instance::parse_list(&mut it)?,
            common: parse_one(Technique::COMMON, &mut it, PhysicsSceneCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Contains a position and orientation transformation suitable for aiming a camera.
#[derive(Clone, Debug)]
pub struct LookAt(
    /// A list of 9 floating-point values.
    /// These values are organized into three vectors as follows:
    /// 1.  Eye position is given as Px, Py, Pz.
    /// 2.  Interest point is given as Ix, Iy, Iz.
    /// 3.  Up-axis direction is given as UPx, UPy, UPz.
    pub Box<[f32; 9]>,
);

impl XNode for LookAt {
    const NAME: &'static str = "lookat";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(LookAt(parse_array_n(element)?))
    }
}

/// Describes transformations that embody mathematical changes to points
/// within a coordinate system or the coordinate system itself.
#[derive(Clone, Debug)]
pub struct Matrix(
    /// A list of 16 floating-point values.
    /// These values are organized into a 4-by-4
    /// column-order matrix suitable for matrix composition.
    pub Box<[f32; 16]>,
);

impl XNode for Matrix {
    const NAME: &'static str = "matrix";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Matrix(parse_array_n(element)?))
    }
}

/// Specifies how to rotate an object around an axis.
#[derive(Clone, Debug)]
pub struct Rotate(
    /// A list of four floating-point values.
    /// These values are organized into a column vector `[X, Y, Z]`
    /// specifying the axis of rotation, followed by an angle in degrees.
    pub Box<[f32; 4]>,
);

impl XNode for Rotate {
    const NAME: &'static str = "rotate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Rotate(parse_array_n(element)?))
    }
}

/// Specifies how to change an object’s size.
#[derive(Clone, Debug)]
pub struct Scale(
    /// A list of three floating-point values.
    /// These values are organized into a column vector suitable for matrix composition.
    pub Box<[f32; 3]>,
);

impl XNode for Scale {
    const NAME: &'static str = "scale";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Scale(parse_array_n(element)?))
    }
}

/// Specifies how to deform an object along one axis.
#[derive(Clone, Debug)]
pub struct Skew(
    /// A list of seven floating-point values.
    /// These values are organized into an angle in degrees
    /// followed by two column vectors specifying the axes of rotation and translation.
    pub Box<[f32; 7]>,
);

impl XNode for Skew {
    const NAME: &'static str = "skew";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Skew(parse_array_n(element)?))
    }
}

/// Changes the position of an object in a local coordinate system.
#[derive(Clone, Debug)]
pub struct Translate(
    /// A list of three floating-point values.
    /// These values are organized into a column vector suitable for a matrix composition.
    pub Box<[f32; 3]>,
);

impl XNode for Translate {
    const NAME: &'static str = "translate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Translate(parse_array_n(element)?))
    }
}

/// A transformation, that can be represented as a matrix
/// (but may be expressed in another way for convenience).
#[derive(Clone, Debug)]
pub enum Transform {
    /// Contains a position and orientation transformation suitable for aiming a camera.
    LookAt(LookAt),
    /// A generic 4x4 matrix.
    Matrix(Matrix),
    /// An axis-angle rotation.
    Rotate(Rotate),
    /// A scale along the three dimentions.
    Scale(Scale),
    /// A skew deformation.
    Skew(Skew),
    /// A translation by a vector.
    Translate(Translate),
}

impl Transform {
    /// Parse a [`Transform`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            LookAt::NAME => Ok(Some(Self::LookAt(LookAt::parse(e)?))),
            Matrix::NAME => Ok(Some(Self::Matrix(Matrix::parse(e)?))),
            Rotate::NAME => Ok(Some(Self::Rotate(Rotate::parse(e)?))),
            Scale::NAME => Ok(Some(Self::Scale(Scale::parse(e)?))),
            Skew::NAME => Ok(Some(Self::Skew(Skew::parse(e)?))),
            Translate::NAME => Ok(Some(Self::Translate(Translate::parse(e)?))),
            _ => Ok(None),
        }
    }
}

/// Embodies the hierarchical relationship of elements in a scene.
///
/// The [`Node`] element declares a point of interest in a scene.
/// A node denotes one point on a branch of the scene graph.
/// The [`Node`] element is essentially the root of a subgraph of the entire scene graph.
#[derive(Clone, Debug)]
pub struct Node {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Any combination of geometric transforms.
    pub transforms: Vec<Transform>,
    /// Allows the node to instantiate a camera object.
    pub instance_camera: Vec<Instance<Camera>>,
    /// Allows the node to instantiate a controller object.
    pub instance_controller: Vec<Instance<Controller>>,
    /// Allows the node to instantiate a geometry object.
    pub instance_geometry: Vec<Instance<Geometry>>,
    /// Allows the node to instantiate a light object.
    pub instance_light: Vec<Instance<Light>>,
    /// Allows the node to instantiate a hierarchy of other nodes.
    pub instance_node: Vec<Instance<Node>>,
    /// Allows the node to recursively define hierarchy.
    pub children: Vec<Node>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Node {
    const NAME: &'static str = "node";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Node {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            transforms: parse_list_many(&mut it, Transform::parse)?,
            instance_camera: Instance::parse_list(&mut it)?,
            instance_controller: Instance::parse_list(&mut it)?,
            instance_geometry: Instance::parse_list(&mut it)?,
            instance_light: Instance::parse_list(&mut it)?,
            instance_node: Instance::parse_list(&mut it)?,
            children: Node::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Embodies the entire set of information that can be visualized
/// from the contents of a COLLADA resource.
#[derive(Clone, Debug)]
pub struct VisualScene {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The scene graph subtrees.
    pub nodes: Vec<Node>,
    // evaluate_scene: Vec<EvaluateScene>
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for VisualScene {
    const NAME: &'static str = "visual_scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(VisualScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            nodes: Node::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares a module of elements of type `T`.
#[derive(Clone, Debug)]
pub struct Library<T> {
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The individual items in the module.
    pub items: Vec<T>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl<T: ParseLibrary> XNode for Library<T> {
    const NAME: &'static str = T::LIBRARY;
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Library {
            asset: Asset::parse_opt_box(&mut it)?,
            items: T::parse_list(&mut it)?, // should be 1 or more but blender disagrees
            extra: Extra::parse_many(it)?,
        })
    }
}

macro_rules! mk_libraries {
    ($($(#[$doc:meta])* $name:ident($arg:ident) = $s:literal,)*) => {
        $(impl ParseLibrary for $arg {
            const LIBRARY: &'static str = $s;
        })*

        /// A library element, which can be a module of any of the kinds supported by COLLADA.
        #[derive(Clone, Debug)]
        pub enum LibraryElement {
            $(#[doc = concat!("Declares a module of [`", stringify!($arg), "`] elements.")]
                $name(Library<$arg>),)*
        }

        impl LibraryElement {
            /// Parse a [`LibraryElement`] from an XML element.
            pub fn parse(e: &Element) -> Result<Option<Self>> {
                Ok(Some(match e.name() {
                    $($arg::LIBRARY => Self::$name(Library::parse(e)?),)*
                    _ => return Ok(None),
                }))
            }
        }
    }
}

mk_libraries! {
    Animations(Animation) = "library_animations",
    AnimationClips(AnimationClip) = "library_animation_clips",
    Cameras(Camera) = "library_cameras",
    Controllers(Controller) = "library_controllers",
    Effects(Effect) = "library_effects",
    ForceFields(ForceField) = "library_force_fields",
    Geometries(Geometry) = "library_geometries",
    Images(Image) = "library_images",
    Lights(Light) = "library_lights",
    Materials(Material) = "library_materials",
    Nodes(Node) = "library_nodes",
    PhysicsMaterials(PhysicsMaterial) = "library_physics_materials",
    PhysicsModels(PhysicsModel) = "library_physics_models",
    PhysicsScenes(PhysicsScene) = "library_physics_scenes",
    VisualScenes(VisualScene) = "library_visual_scenes",
}

/// Instantiates a COLLADA material resource,
/// possibly applying transformations or effects to the object.
///
/// The `data` field depends on the type of object being instantiated.
/// Most types use `()` for this field but some types have additional data:
/// * `Instance<`[`Geometry`]>: [`InstanceGeometryData`]
/// * `Instance<`[`Controller`]>: [`InstanceControllerData`]
/// * `Instance<`[`Effect`]>: [`InstanceEffectData`]
/// * `Instance<`[`PhysicsModel`]>: [`InstancePhysicsModelData`]
///
/// Additionally, some instance nodes are even more different and have their own types:
/// * [`InstanceMaterial`], not `Instance<`[`Material`]`>`
/// * [`InstanceRigidBody`], not `Instance<`[`RigidBody`]`>`
/// * [`InstanceRigidConstraint`], not `Instance<`[`RigidConstraint`]`>`
#[derive(Clone, Debug)]
pub struct Instance<T: Instantiate> {
    /// A text string containing the scoped identifier of this element.
    /// This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The URL of the location of the `T` element to instantiate.
    /// Can refer to a local instance or external reference.
    pub url: Url,
    /// The additional data associated with the instantiation, if any.
    pub data: T::Data,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

/// The trait for types that can be used in [`Instance<T>`].
pub trait Instantiate {
    /// The name of the instance node.
    /// For example `Geometry::INSTANCE = "instance_geometry"`.
    const INSTANCE: &'static str;

    /// The type of additional data associated with instantiations, possibly `()`.
    type Data;

    /// Parse the [`Self::Data`] given an element iterator,
    /// and a reference to the parent element.
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data>;
}

impl<T: Instantiate> XNode for Instance<T> {
    const NAME: &'static str = T::INSTANCE;
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Instance {
            sid: element.attr("sid").map(Into::into),
            url: parse_attr(element.attr("url"))?.ok_or("missing url attribute")?,
            data: T::parse_data(element, &mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Either an instance of a `T`, or a directly inlined `T` object.
pub enum DefInstance<T: Instantiate> {
    /// A definition of a `T`.
    Def(T),
    /// An instantiation of a `T`.
    Ref(Instance<T>),
}

impl<T: Instantiate + Debug> Debug for DefInstance<T>
where
    T::Data: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Def(t) => f.debug_tuple("Def").field(t).finish(),
            Self::Ref(t) => f.debug_tuple("Ref").field(t).finish(),
        }
    }
}

impl<T: Instantiate + Clone> Clone for DefInstance<T>
where
    T::Data: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Def(t) => Self::Def(t.clone()),
            Self::Ref(t) => Self::Ref(t.clone()),
        }
    }
}

impl<T: Instantiate + XNode> DefInstance<T> {
    fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(if e.name() == T::NAME {
            Some(Self::Def(T::parse(e)?))
        } else if e.name() == T::INSTANCE {
            Some(Self::Ref(Instance::parse(e)?))
        } else {
            None
        })
    }
}

macro_rules! basic_instance {
    ($($ty:ty => $val:expr;)*) => {
        $(impl Instantiate for $ty {
            const INSTANCE: &'static str = $val;
            type Data = ();
            fn parse_data(_: &Element, _: &mut ElementIter<'_>) -> Result<Self::Data> {
                Ok(())
            }
        })*
    }
}
basic_instance! {
    Animation => "instance_animation";
    Camera => "instance_camera";
    ForceField => "instance_force_field";
    Light => "instance_light";
    Node => "instance_node";
    PhysicsMaterial => "instance_physics_material";
    PhysicsScene => "instance_physics_scene";
    VisualScene => "instance_visual_scene";
}

/// Declares parametric information for its parent element.
#[derive(Clone, Debug)]
pub struct Param {
    /// A text string value containing the subidentifier of this element.
    /// This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The type of the value data. This text string must be understood by the application.
    pub ty: String,
    /// The user-defined meaning of the parameter.
    pub semantic: Option<Semantic>,
}

impl XNode for Param {
    const NAME: &'static str = "param";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Param {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            ty: element.attr("type").ok_or("expecting 'type' attr")?.into(),
            semantic: parse_attr(element.attr("semantic"))?,
        })
    }
}

/// Declares the information used to process some portion of the content.
/// Each technique conforms to an associated profile.
/// In the COLLADA spec, this element is called "<technique> (core)".
#[derive(Clone, Debug)]
pub struct Technique {
    /// The `<technique>` element can contain any well-formed XML data.
    pub element: Element,
}

impl XNode for Technique {
    const NAME: &'static str = "technique";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        element.attr("profile").ok_or("expected 'profile' attr")?;
        Ok(Technique {
            element: element.clone(),
        })
    }
}
impl Technique {
    /// The name of the `<technique_common>` element.
    pub const COMMON: &'static str = "technique_common";
}

/// Adds a hint for a platform of which technique to use in this effect.
#[derive(Clone, Debug)]
pub struct TechniqueHint {
    /// Defines a string that specifies for which platform this hint is intended.
    pub platform: Option<String>,
    /// A reference to the name of the platform.
    pub ref_: String,
    /// A string that specifies for which API profile this hint is intended.
    /// It is the name of the profile within the effect that contains the technique.
    /// Profiles are constructed by appending this attribute’s value to "Profile".
    /// For example, to select [`ProfileCG`], specify `profile="CG"`.
    pub profile: Option<String>,
}

impl XNode for TechniqueHint {
    const NAME: &'static str = "technique_hint";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(TechniqueHint {
            platform: element.attr("platform").map(Into::into),
            ref_: element.attr("ref").ok_or("expected 'ref' attr")?.into(),
            profile: element.attr("profile").map(Into::into),
        })
    }
}

/// Binds values to uniform inputs of a shader or binds values to effect
/// parameters upon instantiation.
/// In the COLLADA spec, this element is called "<bind> (material)".
#[derive(Clone, Debug)]
pub struct BindM {
    /// Which effect parameter to bind.
    pub semantic: Option<String>,
    /// The location of the value to bind to the specified semantic.
    pub target: Address,
}

impl XNode for BindM {
    const NAME: &'static str = "bind";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("missing target attribute")?;
        Ok(BindM {
            semantic: element.attr("semantic").map(Into::into),
            target: Address(target.into()),
        })
    }
}

/// Binds geometry vertex inputs to effect vertex inputs upon instantiation.
#[derive(Clone, Debug)]
pub struct BindVertexInput {
    /// Which effect parameter to bind.
    pub semantic: String,
    /// Which input semantic to bind.
    pub input_semantic: String,
    /// Which input set to bind.
    pub input_set: Option<u32>,
}

impl XNode for BindVertexInput {
    const NAME: &'static str = "bind_vertex_input";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let semantic = element.attr("semantic");
        let input_semantic = element.attr("input_semantic");
        Ok(BindVertexInput {
            semantic: semantic.ok_or("missing semantic attribute")?.into(),
            input_semantic: input_semantic.ok_or("missing input semantic")?.into(),
            input_set: parse_attr(element.attr("input_set"))?,
        })
    }
}

/// Instantiates a COLLADA material resource.
#[derive(Clone, Debug)]
pub struct InstanceMaterial {
    /// A text string value containing the subidentifier of this element.
    /// This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The URI of the location of the [`Material`] element to instantiate.
    /// Can refer to a local instance or external reference.
    /// For a local instance, this is a relative URI fragment identifier
    /// that begins with the `"#"` character.
    /// The fragment identifier is an XPointer shorthand pointer that
    /// consists of the ID of the element to instantiate.
    /// For an external reference, this is an absolute or relative URL.
    pub target: Url,
    /// Which symbol defined from within the geometry this material binds to.
    pub symbol: String,
    /// Connects a parameter in the material’s effect by semantic
    /// to a target in the scene.
    pub bind: Vec<BindM>,
    /// Binds vertex inputs to effect parameters upon instantiation.
    pub bind_vertex_input: Vec<BindVertexInput>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for InstanceMaterial {
    const NAME: &'static str = "instance_material";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let symbol = element.attr("symbol").ok_or("expecting symbol attr")?;
        let mut it = element.children().peekable();
        Ok(InstanceMaterial {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            target: parse_attr(element.attr("target"))?.ok_or("missing target attribute")?,
            symbol: symbol.into(),
            bind: BindM::parse_list(&mut it)?,
            bind_vertex_input: BindVertexInput::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Binds a specific material to a piece of geometry,
/// binding varying and uniform parameters at the same time.
#[derive(Clone, Debug)]
pub struct BindMaterial {
    /// In [`BindMaterial`] these are added to be targets for animation.
    /// These objects can then be bound to input parameters in the normal manner
    /// without requiring the animation targeting system to parse the internal
    /// layout of an [`Effect`].
    pub param: Vec<Param>,
    /// The common profile data is list of [`InstanceMaterial`]s.
    pub instance_material: Vec<InstanceMaterial>,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for BindMaterial {
    const NAME: &'static str = "bind_material";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(BindMaterial {
            param: Param::parse_list(&mut it)?,
            instance_material: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(InstanceMaterial::parse_list_n::<1>(&mut it)?, it)
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Extra data associated to [`Instance`]<[`Geometry`]>.
#[derive(Clone, Debug)]
pub struct InstanceGeometryData {
    /// Binds material symbols to material instances. This allows a
    /// single geometry to be instantiated into a scene multiple times
    /// each with a different appearance.
    pub bind_material: Option<BindMaterial>,
}

impl Instantiate for Geometry {
    const INSTANCE: &'static str = "instance_geometry";
    type Data = InstanceGeometryData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstanceGeometryData {
            bind_material: BindMaterial::parse_opt(it)?,
        })
    }
}

/// Extra data associated to [`Instance`]<[`Controller`]>.
#[derive(Clone, Debug)]
pub struct InstanceControllerData {
    /// Indicates where a skin controller is to start to search for the
    /// joint nodes it needs. This element is meaningless for morph controllers.
    pub skeleton: Vec<Url>,
    /// Binds a specific material to a piece of geometry.
    pub bind_material: Option<BindMaterial>,
}

impl Instantiate for Controller {
    const INSTANCE: &'static str = "instance_controller";
    type Data = InstanceControllerData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstanceControllerData {
            skeleton: parse_list("skeleton", it, parse_elem)?,
            bind_material: BindMaterial::parse_opt(it)?,
        })
    }
}

/// Assigns a new value to a previously defined parameter.
///
/// This type corresponds to the `<setparam>` element in the COLLADA spec
/// in the special case where the parent is an `<instance_effect>`.
#[derive(Clone, Debug)]
pub struct EffectSetParam {
    /// Attempts to reference the predefined parameter that will have its value set.
    pub ref_: String,
    /// The value of the parameter.
    // This is slightly inaccurate; the collada spec allows a larger set of types here
    // than the ones used in <annotation>.
    pub value: AnnotType,
}

impl XNode for EffectSetParam {
    const NAME: &'static str = "setparam";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = EffectSetParam {
            ref_: element.attr("ref").ok_or("expected ref attr")?.into(),
            value: parse_one_many(&mut it, AnnotType::parse)?,
        };
        finish(res, it)
    }
}

/// Extra data associated to [`Instance`]<[`Effect`]>.
#[derive(Clone, Debug)]
pub struct InstanceEffectData {
    /// [`TechniqueHint`]s indicate the desired or last-used technique
    /// inside an effect profile.
    pub technique_hint: Vec<TechniqueHint>,
    /// [`EffectSetParam`]s assign values to specific effect and profile
    /// parameters that will be unique to the instance.
    pub set_param: Vec<EffectSetParam>,
}

impl Instantiate for Effect {
    const INSTANCE: &'static str = "instance_effect";
    type Data = InstanceEffectData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstanceEffectData {
            technique_hint: TechniqueHint::parse_list(it)?,
            set_param: EffectSetParam::parse_list(it)?,
        })
    }
}

/// Embodies the entire set of information that can be visualized
/// from the contents of a COLLADA resource.
#[derive(Clone, Default, Debug)]
pub struct Scene {
    /// The instantiated [`PhysicsScene`] elements describe
    /// any physics being applied to the scene.
    pub instance_physics_scene: Vec<Instance<PhysicsScene>>,
    /// The scene graph is built from the [`VisualScene`] elements instantiated under [`Scene`].
    pub instance_visual_scene: Option<Instance<VisualScene>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Scene {
    const NAME: &'static str = "scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Scene {
            instance_physics_scene: Instance::parse_list(&mut it)?,
            instance_visual_scene: Instance::parse_opt(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
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
