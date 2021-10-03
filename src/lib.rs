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
//! panic!()
//! ```

#![forbid(unsafe_code)]

mod url;

use std::convert::{TryFrom, TryInto};
use std::io::BufRead;
use std::ops::Deref;
use std::path::Path;
use std::str::FromStr;

pub use crate::url::Url;
pub use minidom::Element;

type XReader<R> = minidom::quick_xml::Reader<R>;

#[derive(Debug)]
pub enum Error {
    Parse(minidom::Error),
    Other(&'static str),
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

pub trait XNode: Sized {
    const NAME: &'static str;
    fn parse(element: &Element) -> Result<Self>;

    fn parse_box(element: &Element) -> Result<Box<Self>> {
        Self::parse(element).map(Box::new)
    }

    fn parse_one<'a>(it: &mut impl Iterator<Item = &'a Element>) -> Result<Self> {
        parse_one(Self::NAME, it, Self::parse)
    }

    fn parse_opt(it: &mut ElementIter<'_>) -> Result<Option<Self>> {
        parse_opt(Self::NAME, it, Self::parse)
    }

    fn parse_opt_box(it: &mut ElementIter<'_>) -> Result<Option<Box<Self>>> {
        parse_opt(Self::NAME, it, Self::parse_box)
    }

    fn parse_list(it: &mut ElementIter<'_>) -> Result<Vec<Self>> {
        parse_list(Self::NAME, it, Self::parse)
    }

    fn parse_list_n<const N: usize>(it: &mut ElementIter<'_>) -> Result<Vec<Self>> {
        let arr = parse_list(Self::NAME, it, Self::parse)?;
        if arr.len() < N {
            return Err(format!("parse error: expected {} {} elements", N, Self::NAME).into());
        }
        Ok(arr)
    }
}

#[derive(Default, Debug)]
pub struct Extra {
    pub id: Option<String>,
    pub name: Option<String>,
    pub ty: Option<String>,
    pub asset: Option<Box<Asset>>,
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
            technique: Technique::parse_list(&mut it)?,
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

#[derive(Default, Debug)]
pub struct Contributor {
    pub author: Option<String>,
    pub authoring_tool: Option<String>,
    pub comments: Option<String>,
    pub copyright: Option<String>,
    pub source_data: Option<String>,
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
            source_data: parse_opt("source_data", &mut it, parse_text)?,
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub struct Unit {
    pub name: Option<String>,
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

#[derive(Debug)]
pub enum UpAxis {
    XUp,
    YUp,
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

#[derive(Debug)]
pub struct Asset {
    pub contributor: Vec<Contributor>,
    pub created: String,
    pub keywords: Vec<String>,
    pub modified: String,
    pub revision: Option<String>,
    pub subject: Option<String>,
    pub title: Option<String>,
    pub unit: Option<Unit>,
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

pub trait ParseLibrary: XNode {
    const LIBRARY: &'static str;
}

#[derive(Debug)]
pub struct Sampler {
    pub id: Option<String>,
    pub inputs: Vec<Input>,
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

#[derive(Debug)]
pub struct Channel {
    pub source: Url,
    pub target: String,
}

impl XNode for Channel {
    const NAME: &'static str = "channel";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("expecting target attr")?;
        Ok(Channel {
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            target: target.into(),
        })
    }
}

#[derive(Debug)]
pub struct Animation {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub children: Vec<Animation>,
    pub source: Vec<Source>,
    pub sampler: Vec<Sampler>,
    pub channel: Vec<Channel>,
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

#[derive(Debug)]
pub struct AnimationClip {
    pub id: Option<String>,
    pub name: Option<String>,
    pub start: f32,
    pub end: f32,
    pub asset: Option<Box<Asset>>,
    pub instance_animation: Vec<Instance<Animation>>,
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
            end: parse_attr(element.attr("end"))?.unwrap_or(0.),
            asset: Asset::parse_opt_box(&mut it)?,
            instance_animation: Instance::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Orthographic {
    pub xmag: Option<f32>,
    pub ymag: Option<f32>,
    pub extra: Vec<Extra>,
    pub aspect_ratio: Option<f32>,
    pub znear: f32,
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

#[derive(Debug)]
pub struct Perspective {
    pub xfov: Option<f32>,
    pub yfov: Option<f32>,
    pub aspect_ratio: Option<f32>,
    pub znear: f32,
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

#[derive(Debug)]
pub enum OpticsCommon {
    Orthographic(Orthographic),
    Perspective(Perspective),
}

impl OpticsCommon {
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Orthographic::NAME => Ok(Some(Self::Orthographic(Orthographic::parse(e)?))),
            Perspective::NAME => Ok(Some(Self::Perspective(Perspective::parse(e)?))),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct Optics {
    pub common: OpticsCommon,
    pub technique: Vec<Technique>,
    pub extra: Vec<Extra>,
}

impl XNode for Optics {
    const NAME: &'static str = "optics";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Optics {
            common: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_one_many(&mut it, OpticsCommon::parse)?, it)
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Imager {
    pub technique: Vec<Technique>,
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

#[derive(Debug)]
pub struct Camera {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub optics: Optics,
    pub imager: Option<Imager>,
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

#[derive(Debug)]
pub struct Joints {
    pub inputs: Vec<Input>,
    pub extra: Vec<Extra>,
}

impl XNode for Joints {
    const NAME: &'static str = "joints";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Joints {
            inputs: Input::parse_list_n::<2>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct VertexWeights {
    pub count: usize,
    pub inputs: InputList,
    pub joint: usize,
    pub vcount: Option<Box<[u32]>>,
    pub prim: Option<Box<[u32]>>,
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

#[derive(Debug)]
pub struct Skin {
    pub source: Url,
    pub bind_shape_matrix: Option<Box<[f32; 16]>>,
    pub sources: Vec<Source>,
    pub joints: Joints,
    pub weights: VertexWeights,
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

#[derive(Debug)]
pub enum MorphMethod {
    Normalized,
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

#[derive(Debug)]
pub struct Targets {
    pub inputs: Vec<Input>,
    pub morph_target: usize,
    pub morph_weight: usize,
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

#[derive(Debug)]
pub struct Morph {
    pub source: Url,
    pub method: MorphMethod,
    pub sources: Vec<Source>,
    pub targets: Targets,
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

#[derive(Debug)]
pub enum ControlElement {
    Skin(Skin),
    Morph(Morph),
}

impl ControlElement {
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Skin::NAME => Ok(Some(Self::Skin(Skin::parse(e)?))),
            Morph::NAME => Ok(Some(Self::Morph(Morph::parse(e)?))),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct Controller {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub element: ControlElement,
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

#[derive(Debug)]
pub enum AnnotType {
    Bool(bool),
    Bool2([bool; 2]),
    Bool3([bool; 3]),
    Bool4([bool; 4]),
    Int(u32),
    Int2([u32; 2]),
    Int3(Box<[u32; 3]>),
    Int4(Box<[u32; 4]>),
    Float(f32),
    Float2([f32; 2]),
    Float3(Box<[f32; 3]>),
    Float4(Box<[f32; 4]>),
    Float2x2(Box<[f32; 2 * 2]>),
    Float3x3(Box<[f32; 3 * 3]>),
    Float4x4(Box<[f32; 4 * 4]>),
    String(Box<str>),
}

impl AnnotType {
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

#[derive(Debug)]
pub struct Annotate {
    pub name: String,
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

#[derive(Debug)]
pub enum SurfaceFace {
    PosX,
    NegX,
    PosY,
    NegY,
    PosZ,
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

#[derive(Debug)]
pub enum SurfaceInit {
    Null,
    Target,
    // Cube(InitCube),
    // Volume(InitVolume),
    // Planar(InitPlanar),
    From {
        mip: u32,
        slice: u32,
        face: SurfaceFace,
    },
}

impl SurfaceInit {
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

#[derive(Debug)]
pub struct Surface {
    pub init: SurfaceInit,
    pub format: Option<String>,
    pub format_hint: Option<String>,
    pub size: Option<Box<[u32; 3]>>,
    pub viewport_ratio: Option<Box<[f32; 2]>>,
    pub mip_levels: u32,
    pub mipmap_generate: bool,
    pub extra: Vec<Extra>,
}

impl XNode for Surface {
    const NAME: &'static str = "surface";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Surface {
            init: parse_one_many(&mut it, SurfaceInit::parse)?,
            format: parse_opt("format", &mut it, parse_text)?,
            format_hint: parse_opt("format_hint", &mut it, parse_text)?,
            size: parse_opt("size", &mut it, parse_array_n)?,
            viewport_ratio: parse_opt("viewport_ratio", &mut it, parse_array_n)?,
            mip_levels: parse_opt("mip_levels", &mut it, parse_elem)?.unwrap_or(0),
            mipmap_generate: parse_opt("mipmap_generate", &mut it, parse_elem)?.unwrap_or(false),
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub enum WrapMode {
    Wrap,
    Mirror,
    Clamp,
    Border,
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Sampler2D {
    pub source: String,
    pub wrap_s: WrapMode,
    pub wrap_t: WrapMode,
    pub min_filter: SamplerFilterMode,
    pub mag_filter: SamplerFilterMode,
    pub mip_filter: SamplerFilterMode,
    pub border_color: Option<Box<[f32; 4]>>,
    pub mipmap_max_level: u8,
    pub mipmap_bias: f32,
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

#[derive(Debug)]
pub enum ParamType {
    Float(f32),
    Float2([f32; 2]),
    Float3(Box<[f32; 3]>),
    Float4(Box<[f32; 4]>),
    Surface(Box<Surface>),
    Sampler2D(Box<Sampler2D>),
    Other(Box<Element>),
}

impl ParamType {
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

#[derive(Debug)]
pub struct NewParam {
    pub sid: String,
    pub annotate: Vec<Annotate>,
    pub semantic: Option<String>,
    pub modifier: Option<String>,
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
            modifier: parse_opt("modifier", &mut it, parse_text)?,
            ty: parse_one_many(&mut it, ParamType::parse)?,
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub enum ImageParam {
    NewParam(NewParam),
    Image(Image),
}

#[derive(Debug)]
pub struct TechniqueFx<T> {
    pub id: Option<String>,
    pub sid: String,
    pub asset: Option<Box<Asset>>,
    pub data: T,
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

pub trait ProfileData: Sized {
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

#[derive(Debug)]
pub struct Texture {
    pub texture: String,
    pub texcoord: String,
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

#[derive(Debug)]
pub enum ColorParam {
    Color(Box<[f32; 4]>),
    Param(Box<str>),
    Texture(Box<Texture>),
}

impl ColorParam {
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

#[derive(Debug)]
pub enum FloatParam {
    Float(f32),
    Param(Box<str>),
}

impl FloatParam {
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

#[derive(Default, Debug)]
pub struct Blinn {
    pub emission: Option<ColorParam>,
    pub ambient: Option<ColorParam>,
    pub diffuse: Option<ColorParam>,
    pub specular: Option<ColorParam>,
    pub shininess: Option<FloatParam>,
    pub reflective: Option<ColorParam>,
    pub reflectivity: Option<FloatParam>,
    pub transparent: Option<ColorParam>,
    pub transparency: Option<FloatParam>,
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

#[derive(Default, Debug)]
pub struct ConstantFx {
    pub emission: Option<ColorParam>,
    pub reflective: Option<ColorParam>,
    pub reflectivity: Option<FloatParam>,
    pub transparent: Option<ColorParam>,
    pub transparency: Option<FloatParam>,
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

#[derive(Default, Debug)]
pub struct Lambert {
    pub emission: Option<ColorParam>,
    pub ambient: Option<ColorParam>,
    pub diffuse: Option<ColorParam>,
    pub reflective: Option<ColorParam>,
    pub reflectivity: Option<FloatParam>,
    pub transparent: Option<ColorParam>,
    pub transparency: Option<FloatParam>,
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

#[derive(Default, Debug)]
pub struct Phong {
    pub emission: Option<ColorParam>,
    pub ambient: Option<ColorParam>,
    pub diffuse: Option<ColorParam>,
    pub specular: Option<ColorParam>,
    pub shininess: Option<FloatParam>,
    pub reflective: Option<ColorParam>,
    pub reflectivity: Option<FloatParam>,
    pub transparent: Option<ColorParam>,
    pub transparency: Option<FloatParam>,
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

#[derive(Debug)]
pub enum Shader {
    Blinn(Blinn),
    Constant(ConstantFx),
    Lambert(Lambert),
    Phong(Phong),
}

impl Shader {
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

#[derive(Default, Debug)]
pub struct CommonData {
    pub image_param: Vec<ImageParam>,
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

#[derive(Default, Debug)]
pub struct ProfileCommon {
    pub asset: Option<Box<Asset>>,
    pub image: Vec<Image>,
    pub new_param: Vec<NewParam>,
    pub technique: Vec<TechniqueFx<CommonData>>,
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
            technique: TechniqueFx::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct ProfileCG(pub Element); // TODO

impl XNode for ProfileCG {
    const NAME: &'static str = "profile_CG";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileCG(element.clone()))
    }
}

#[derive(Debug)]
pub struct ProfileGLES(pub Element); // TODO

impl ProfileGLES {
    const NAME: &'static str = "profile_GLES";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileGLES(element.clone()))
    }
}

#[derive(Debug)]
pub struct ProfileGLSL(pub Element); // TODO

impl ProfileGLSL {
    const NAME: &'static str = "profile_GLSL";
    fn parse(element: &Element) -> Result<Self> {
        Ok(ProfileGLSL(element.clone()))
    }
}

#[derive(Debug)]
pub enum Profile {
    Common(ProfileCommon),
    CG(ProfileCG),
    GLES(ProfileGLES),
    GLSL(ProfileGLSL),
}

impl Profile {
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

#[derive(Debug)]
pub struct Effect {
    pub id: String,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub annotate: Vec<Annotate>,
    pub image: Vec<Image>,
    pub new_param: Vec<NewParam>,
    pub profile: Vec<Profile>,
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

#[derive(Default, Debug)]
pub struct ForceField {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub technique: Vec<Technique>,
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
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub enum ArrayElement {
    IdRef(Box<[String]>),
    Name(Box<[String]>),
    Bool(Box<[bool]>),
    Float(Box<[f32]>),
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

/// `<source>` (core)
#[derive(Debug)]
pub struct Source {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub array: Option<ArrayElement>,
    pub accessor: Accessor,
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

#[derive(Debug)]
pub struct Accessor {
    pub count: usize,
    pub offset: usize,
    pub source: Url,
    pub stride: usize,
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

macro_rules! mk_semantic {
    ($($(#[$doc:meta])* $n:ident = $t:literal,)*) => {
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum Semantic {
            $($(#[$doc])* $n,)*
            /// Any semantic value not covered above
            Other(Box<str>),
        }

        impl Semantic {
            fn parse(s: &str) -> Self {
                match s {
                    $($t => Self::$n,)*
                    _ => Self::Other(s.into()),
                }
            }
        }
    };
}

mk_semantic! {
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

/// `<input>` (unshared)
#[derive(Debug)]
pub struct Input {
    pub semantic: Semantic,
    pub source: Url,
}

impl XNode for Input {
    const NAME: &'static str = "input";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let semantic = element.attr("semantic").ok_or("missing semantic attr")?;
        Ok(Input {
            semantic: Semantic::parse(semantic),
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
        })
    }
}

#[derive(Debug)]
pub struct InputS {
    pub input: Input,
    pub offset: u32,
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

#[derive(Default, Debug)]
pub struct InputList {
    pub inputs: Vec<InputS>,
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

#[derive(Debug)]
pub struct Vertices {
    pub id: String,
    pub name: Option<String>,
    pub input: Vec<Input>,
    pub extra: Vec<Extra>,
}

impl XNode for Vertices {
    const NAME: &'static str = "vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Vertices {
            id: element.attr("id").ok_or("missing 'id' attr")?.into(),
            name: element.attr("name").map(Into::into),
            input: Input::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Default, Debug)]
pub struct Geom<T> {
    pub name: Option<String>,
    pub material: Option<String>,
    pub count: usize,
    pub inputs: InputList,
    pub data: T,
    pub extra: Vec<Extra>,
}

pub trait ParseGeom: Default {
    const NAME: &'static str;
    fn parse(it: &mut ElementIter<'_>) -> Result<Self>;

    fn validate(_: &Geom<Self>) -> Result<()> {
        Ok(())
    }

    fn parse_geom(element: &Element) -> Result<Geom<Self>> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Geom {
            name: element.attr("name").map(Into::into),
            material: element.attr("material").map(Into::into),
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            inputs: InputList::parse::<0>(&mut it)?,
            data: Self::parse(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        Self::validate(&res)?;
        Ok(res)
    }
}
impl<T: ParseGeom> Geom<T> {
    pub const NAME: &'static str = T::NAME;
}

#[derive(Default, Debug)]
pub struct LineGeom(pub Option<Box<[u32]>>);
pub type Lines = Geom<LineGeom>;

impl ParseGeom for LineGeom {
    const NAME: &'static str = "lines";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineGeom(parse_opt("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = res.data.0 {
            if res.inputs.depth * 2 * res.count != data.len() {
                return Err("line count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct LineStripGeom(pub Vec<Box<[u32]>>);
pub type LineStrips = Geom<LineStripGeom>;

impl ParseGeom for LineStripGeom {
    const NAME: &'static str = "line_strips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineStripGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.0.len() {
            return Err("line strip count does not match <p> fields".into());
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<2>(p)) {
            return Err("incorrect <p> field in line strips".into());
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct PolygonHole {
    pub verts: Box<[u32]>,
    pub hole: Vec<Box<[u32]>>,
}

#[derive(Default, Debug)]
pub struct PolygonGeom(pub Vec<PolygonHole>);
pub type Polygons = Geom<PolygonGeom>;

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
        if res.count != res.data.0.len() {
            return Err("polygon count does not match <p> fields".into());
        }
        if !res.data.0.iter().all(|ph| {
            res.inputs.check_prim::<3>(&ph.verts)
                && ph.hole.iter().all(|h| res.inputs.check_prim::<3>(h))
        }) {
            return Err("incorrect <p> field in polygon".into());
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct PolyListGeom {
    pub vcount: Option<Box<[u32]>>,
    pub prim: Option<Box<[u32]>>,
}
pub type PolyList = Geom<PolyListGeom>;

fn validate_vcount(
    count: usize,
    depth: usize,
    vcount: Option<&[u32]>,
    prim: Option<&[u32]>,
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

#[derive(Default, Debug)]
pub struct TriangleGeom(pub Option<Box<[u32]>>);
pub type Triangles = Geom<TriangleGeom>;

impl ParseGeom for TriangleGeom {
    const NAME: &'static str = "triangles";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriangleGeom(parse_opt("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = res.data.0 {
            if res.inputs.depth * 3 * res.count != data.len() {
                return Err("triangle count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct TriFanGeom(pub Vec<Box<[u32]>>);
pub type TriFans = Geom<TriFanGeom>;

impl ParseGeom for TriFanGeom {
    const NAME: &'static str = "trifans";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriFanGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.0.len() {
            return Err("triangle fan count does not match <p> fields".into());
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle fans".into());
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct TriStripGeom(pub Vec<Box<[u32]>>);
pub type TriStrips = Geom<TriStripGeom>;

impl ParseGeom for TriStripGeom {
    const NAME: &'static str = "tristrips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriStripGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.0.len() {
            return Err("triangle strip count does not match <p> fields".into());
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle strips".into());
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Primitive {
    Lines(Lines),
    LineStrips(LineStrips),
    Polygons(Polygons),
    PolyList(PolyList),
    Triangles(Triangles),
    TriFans(TriFans),
    TriStrips(TriStrips),
}

impl Primitive {
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            LineGeom::NAME => Primitive::Lines(LineGeom::parse_geom(e)?),
            LineStripGeom::NAME => Primitive::LineStrips(LineStripGeom::parse_geom(e)?),
            PolygonGeom::NAME => Primitive::Polygons(PolygonGeom::parse_geom(e)?),
            PolyListGeom::NAME => Primitive::PolyList(PolyListGeom::parse_geom(e)?),
            TriangleGeom::NAME => Primitive::Triangles(TriangleGeom::parse_geom(e)?),
            TriFanGeom::NAME => Primitive::TriFans(TriFanGeom::parse_geom(e)?),
            TriStripGeom::NAME => Primitive::TriStrips(TriStripGeom::parse_geom(e)?),
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct ConvexMesh {
    pub source: Vec<Source>,
    pub vertices: Option<Vertices>,
    pub elements: Vec<Primitive>,
    pub extra: Vec<Extra>,
}

impl ConvexMesh {
    pub const NAME: &'static str = "convex_mesh";
    pub fn parse(element: &Element) -> Result<GeometryElement> {
        debug_assert_eq!(element.name(), Self::NAME);
        if let Some(s) = parse_attr(element.attr("convex_hull_of"))? {
            return Ok(GeometryElement::ConvexHullOf(s));
        }
        Ok(GeometryElement::Mesh(Mesh::parse(true, element)?))
    }
}

#[derive(Debug)]
pub struct Mesh {
    pub convex: bool,
    pub source: Vec<Source>,
    pub vertices: Option<Vertices>,
    pub elements: Vec<Primitive>,
    pub extra: Vec<Extra>,
}

impl Mesh {
    pub fn parse(convex: bool, element: &Element) -> Result<Self> {
        debug_assert_eq!(
            element.name(),
            if convex { ConvexMesh::NAME } else { Self::NAME }
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

#[derive(Debug)]
pub struct ControlVertices {
    pub input: Vec<Input>,
    pub extra: Vec<Extra>,
}

impl XNode for ControlVertices {
    const NAME: &'static str = "control_vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(ControlVertices {
            input: Input::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Spline {
    pub closed: bool,
    pub source: Vec<Source>,
    pub controls: ControlVertices,
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

#[derive(Debug)]
pub enum GeometryElement {
    ConvexHullOf(Url),
    Mesh(Mesh),
    Spline(Spline),
}

impl GeometryElement {
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            ConvexMesh::NAME => ConvexMesh::parse(element)?,
            Mesh::NAME => GeometryElement::Mesh(Mesh::parse(false, element)?),
            Spline::NAME => GeometryElement::Spline(Spline::parse(element)?),
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct Geometry {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub element: GeometryElement,
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

#[derive(Debug)]
pub enum ImageSource {
    Data(Box<[u8]>),
    InitFrom(String),
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
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            "data" => {
                let s = get_text(element).ok_or("expected text element")?;
                ImageSource::Data(parse_hex_array(s))
            }
            "init_from" => ImageSource::InitFrom(parse_text(element)?),
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct Image {
    pub id: Option<String>,
    pub name: Option<String>,
    pub format: Option<String>,
    pub height: u32, // 0 means omitted
    pub width: u32,  // 0 means omitted
    pub depth: u32,  // default 1
    pub asset: Option<Box<Asset>>,
    pub source: ImageSource,
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

#[derive(Debug)]
pub struct AmbientLight {
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

#[derive(Debug)]
pub struct DirectionalLight {
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

#[derive(Debug)]
pub struct PointLight {
    pub color: Box<[f32; 3]>,
    pub constant_attenuation: f32,
    pub linear_attenuation: f32,
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

#[derive(Debug)]
pub struct SpotLight {
    pub color: Box<[f32; 3]>,
    pub constant_attenuation: f32,
    pub linear_attenuation: f32,
    pub quadratic_attenuation: f32,
    pub falloff_angle: f32,
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

#[derive(Debug)]
pub enum LightKind {
    Ambient(AmbientLight),
    Directional(DirectionalLight),
    Point(Box<PointLight>),
    Spot(Box<SpotLight>),
}

impl LightKind {
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

#[derive(Debug)]
pub struct Light {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub kind: LightKind,
    pub technique: Vec<Technique>,
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

#[derive(Debug)]
pub struct Material {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub instance_effect: Instance<Effect>,
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

#[derive(Debug)]
pub struct PhysicsMaterialCommon {
    pub dynamic_friction: f32,
    pub restitution: f32,
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

#[derive(Debug)]
pub struct PhysicsMaterial {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub common: PhysicsMaterialCommon,
    pub technique: Vec<Technique>,
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

#[derive(Default, Debug)]
pub struct RigidBodyCommon {
    pub dynamic: Option<bool>,
    pub mass: Option<f32>,
    pub mass_frame: Vec<RigidTransform>,
    pub inertia: Option<Box<[f32; 3]>>,
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
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

#[derive(Debug)]
pub struct RigidBody {
    pub sid: Option<String>,
    pub name: Option<String>,
    pub common: RigidBodyCommon,
    pub technique: Vec<Technique>,
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

#[derive(Default, Debug)]
pub struct InstanceRigidBodyCommon {
    pub angular_velocity: [f32; 3],
    pub velocity: [f32; 3],
    pub common: RigidBodyCommon,
}

impl std::ops::Deref for InstanceRigidBodyCommon {
    type Target = RigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl InstanceRigidBodyCommon {
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

#[derive(Debug)]
pub struct InstanceRigidBody {
    pub body: String,
    pub target: Url,
    pub common: InstanceRigidBodyCommon,
    pub technique: Vec<Technique>,
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

#[derive(Debug)]
pub enum RigidTransform {
    Translate(Translate),
    Rotate(Rotate),
}

impl RigidTransform {
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Translate::NAME => Ok(Some(Self::Translate(Translate::parse(e)?))),
            Rotate::NAME => Ok(Some(Self::Rotate(Rotate::parse(e)?))),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct Plane {
    pub equation: [f32; 4],
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

#[derive(Debug)]
pub struct BoxShape {
    pub half_extents: [f32; 3],
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

#[derive(Debug)]
pub struct Sphere {
    pub radius: f32,
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

#[derive(Debug)]
pub struct Cylinder {
    pub height: f32,
    pub radius: f32,
    pub extra: Vec<Extra>,
}

impl XNode for Cylinder {
    const NAME: &'static str = "cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Cylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: parse_one("radius", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct TaperedCylinder {
    pub height: f32,
    pub radius1: f32,
    pub radius2: f32,
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCylinder {
    const NAME: &'static str = "tapered_cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: parse_one("radius1", &mut it, parse_elem)?,
            radius2: parse_one("radius2", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Capsule {
    pub height: f32,
    pub radius: f32,
    pub extra: Vec<Extra>,
}

impl XNode for Capsule {
    const NAME: &'static str = "capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Capsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: parse_one("radius", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct TaperedCapsule {
    pub height: f32,
    pub radius1: f32,
    pub radius2: f32,
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCapsule {
    const NAME: &'static str = "tapered_capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCapsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: parse_one("radius1", &mut it, parse_elem)?,
            radius2: parse_one("radius2", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub enum ShapeGeom {
    Plane(Plane),
    Box(BoxShape),
    Sphere(Sphere),
    Cylinder(Cylinder),
    TaperedCylinder(TaperedCylinder),
    Capsule(Capsule),
    TaperedCapsule(TaperedCapsule),
    Geom(Instance<Geometry>),
}

impl ShapeGeom {
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

#[derive(Debug)]
pub struct Shape {
    pub hollow: Option<bool>,
    pub mass: Option<f32>,
    pub density: Option<f32>,
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
    pub geom: ShapeGeom,
    pub transform: Vec<RigidTransform>,
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

#[derive(Debug)]
pub struct Attachment {
    pub rigid_body: Url,
    pub transform: Vec<RigidTransform>,
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

#[derive(Default, Debug)]
pub struct Limits {
    pub min: Option<Box<[f32; 3]>>,
    pub max: Option<Box<[f32; 3]>>,
}
impl Limits {
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            min: parse_opt("min", &mut it, parse_array_n)?,
            max: parse_opt("max", &mut it, parse_array_n)?,
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub struct Spring {
    pub stiffness: f32,
    pub damping: f32,
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

#[derive(Default, Debug)]
pub struct RigidConstraintCommon {
    pub enabled: bool,
    pub interpenetrate: bool,
    pub swing_cone_and_twist: Limits,
    pub linear: Limits,
    pub spring_linear: Spring,
    pub spring_angular: Spring,
}

impl RigidConstraintCommon {
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
        let (spring_linear, spring_angular) = parse_opt("spring", &mut it, |e| {
            let mut it = e.children().peekable();
            let lin = parse_opt("linear", &mut it, Spring::parse)?.unwrap_or_default();
            let ang = parse_opt("angular", &mut it, Spring::parse)?.unwrap_or_default();
            finish((lin, ang), it)
        })?
        .unwrap_or_default();
        let res = Self {
            enabled,
            interpenetrate,
            swing_cone_and_twist,
            linear,
            spring_linear,
            spring_angular,
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub struct RigidConstraint {
    pub sid: Option<String>,
    pub name: Option<String>,
    pub ref_attachment: Attachment,
    pub attachment: Attachment,
    pub common: RigidConstraintCommon,
    pub technique: Vec<Technique>,
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

#[derive(Debug)]
pub struct InstanceRigidConstraint {
    pub constraint: String,
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

#[derive(Debug)]
pub struct PhysicsModelData {
    pub parent: Option<Url>,
    pub instance_force_field: Vec<Instance<ForceField>>,
    pub instance_rigid_body: Vec<InstanceRigidBody>,
    pub instance_rigid_constraint: Vec<InstanceRigidConstraint>,
}

impl Instantiate for PhysicsModel {
    const INSTANCE: &'static str = "instance_physics_model";
    type Data = PhysicsModelData;
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(PhysicsModelData {
            parent: parse_attr(e.attr("parent"))?,
            instance_force_field: Instance::parse_list(it)?,
            instance_rigid_body: InstanceRigidBody::parse_list(it)?,
            instance_rigid_constraint: InstanceRigidConstraint::parse_list(it)?,
        })
    }
}

#[derive(Debug)]
pub struct PhysicsModel {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub rigid_body: Vec<RigidBody>,
    pub rigid_constraint: Vec<RigidConstraint>,
    pub instances: Vec<Instance<PhysicsModel>>,
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

#[derive(Default, Debug)]
pub struct PhysicsSceneCommon {
    pub gravity: Option<f32>,
    pub time_step: Option<f32>,
}

impl PhysicsSceneCommon {
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            gravity: parse_opt("gravity", &mut it, parse_elem)?,
            time_step: parse_opt("time_step", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub struct PhysicsScene {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub instance_force_field: Vec<Instance<ForceField>>,
    pub instance_physics_model: Vec<Instance<PhysicsModel>>,
    pub common: PhysicsSceneCommon,
    pub technique: Vec<Technique>,
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

#[derive(Debug)]
pub struct LookAt(pub Box<[f32; 9]>);

impl XNode for LookAt {
    const NAME: &'static str = "lookat";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(LookAt(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub struct Matrix(pub Box<[f32; 16]>);

impl XNode for Matrix {
    const NAME: &'static str = "matrix";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Matrix(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub struct Rotate(pub Box<[f32; 4]>);

impl XNode for Rotate {
    const NAME: &'static str = "rotate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Rotate(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub struct Scale(pub Box<[f32; 3]>);

impl XNode for Scale {
    const NAME: &'static str = "scale";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Scale(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub struct Skew(pub Box<[f32; 7]>);

impl XNode for Skew {
    const NAME: &'static str = "skew";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Skew(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub struct Translate(pub Box<[f32; 3]>);

impl XNode for Translate {
    const NAME: &'static str = "translate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Translate(parse_array_n(element)?))
    }
}

#[derive(Debug)]
pub enum Transform {
    LookAt(LookAt),
    Matrix(Matrix),
    Rotate(Rotate),
    Scale(Scale),
    Skew(Skew),
    Translate(Translate),
}

impl Transform {
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

#[derive(Debug)]
pub struct Node {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub transforms: Vec<Transform>,
    pub instance_camera: Vec<Instance<Camera>>,
    pub instance_controller: Vec<Instance<Controller>>,
    pub instance_geometry: Vec<Instance<Geometry>>,
    pub instance_light: Vec<Instance<Light>>,
    pub instance_node: Vec<Instance<Node>>,
    pub children: Vec<Node>,
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

#[derive(Debug)]
pub struct VisualScene {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub nodes: Vec<Node>,
    // evaluate_scene: Vec<EvaluateScene>
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

#[derive(Debug)]
pub struct Library<T> {
    pub asset: Option<Box<Asset>>,
    pub items: Vec<T>,
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
    ($($name:ident($arg:ident) = $s:literal,)*) => {
        $(impl ParseLibrary for $arg {
            const LIBRARY: &'static str = $s;
        })*

        #[derive(Debug)]
        pub enum LibraryElement {
            $($name(Library<$arg>),)*
        }

        impl LibraryElement {
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

#[derive(Debug)]
pub struct Instance<T: Instantiate> {
    pub url: Url,
    pub data: T::Data,
    pub extra: Vec<Extra>,
}

pub trait Instantiate {
    const INSTANCE: &'static str;
    type Data: std::fmt::Debug;
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data>;
}

impl<T: Instantiate> XNode for Instance<T> {
    const NAME: &'static str = T::INSTANCE;
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Instance {
            url: parse_attr(element.attr("url"))?.ok_or("missing url attribute")?,
            data: T::parse_data(element, &mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub enum DefInstance<T: Instantiate> {
    Def(T),
    Ref(Instance<T>),
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

#[derive(Debug)]
pub struct Param {
    pub sid: Option<String>,
    pub name: Option<String>,
    pub ty: String,
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
            semantic: element.attr("semantic").map(Semantic::parse),
        })
    }
}

/// `<technique>` (core)
#[derive(Debug)]
pub struct Technique {
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
    pub const COMMON: &'static str = "technique_common";
}

#[derive(Debug)]
pub struct TechniqueHint {
    pub platform: Option<String>,
    pub ref_: String,
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

#[derive(Debug)]
pub struct BindM {
    pub semantic: Option<String>,
    pub target: String,
}

impl XNode for BindM {
    const NAME: &'static str = "bind";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("missing target attribute")?;
        Ok(BindM {
            semantic: element.attr("semantic").map(Into::into),
            target: target.into(),
        })
    }
}

#[derive(Debug)]
pub struct BindVertexInput {
    pub semantic: String,
    pub input_semantic: String,
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

#[derive(Debug)]
pub struct InstanceMaterial {
    pub sid: Option<String>,
    pub name: Option<String>,
    pub target: Url,
    pub symbol: String,
    pub bind: Vec<BindM>,
    pub bind_vertex_input: Vec<BindVertexInput>,
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

#[derive(Debug)]
pub struct BindMaterial {
    pub param: Vec<Param>,
    pub instance_material: Vec<InstanceMaterial>,
    pub technique: Vec<Technique>,
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

impl Instantiate for Geometry {
    const INSTANCE: &'static str = "instance_geometry";
    type Data = Option<BindMaterial>;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        BindMaterial::parse_opt(it)
    }
}

#[derive(Debug)]
pub struct ControllerData {
    pub skeleton: Vec<Url>,
    pub bind_material: Option<BindMaterial>,
}

impl Instantiate for Controller {
    const INSTANCE: &'static str = "instance_controller";
    type Data = ControllerData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(ControllerData {
            skeleton: parse_list("skeleton", it, parse_elem)?,
            bind_material: BindMaterial::parse_opt(it)?,
        })
    }
}

#[derive(Debug)]
pub struct EffectSetParam {
    pub ref_: String,
    pub value: AnnotType, // slightly inaccurate
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

#[derive(Debug)]
pub struct EffectData {
    pub technique_hint: Vec<TechniqueHint>,
    pub set_param: Vec<EffectSetParam>,
}

impl Instantiate for Effect {
    const INSTANCE: &'static str = "instance_effect";
    type Data = EffectData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(EffectData {
            technique_hint: TechniqueHint::parse_list(it)?,
            set_param: EffectSetParam::parse_list(it)?,
        })
    }
}

#[derive(Default, Debug)]
pub struct Scene {
    pub instance_physics_scene: Vec<Instance<PhysicsScene>>,
    pub instance_visual_scene: Option<Instance<VisualScene>>,
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

#[derive(Debug)]
pub struct Document {
    pub asset: Asset,
    pub library: Vec<LibraryElement>,
    pub scene: Option<Scene>,
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
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        Self::from_reader(std::io::BufReader::new(std::fs::File::open(path)?))
    }

    pub fn from_reader<R: BufRead>(reader: R) -> Result<Self> {
        Self::from_xml_reader(&mut XReader::from_reader(reader))
    }

    pub fn from_xml_reader<R: BufRead>(reader: &mut XReader<R>) -> Result<Self> {
        let root = minidom::Element::from_reader(reader)?;
        Self::parse(&root)
    }
}

impl XNode for Document {
    const NAME: &'static str = "COLLADA";
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
