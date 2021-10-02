//! TODO:
//! * `accessor`
//! * `bind_vertex_input`
//! * `diffuse`
//! * `effect`
//! * `extra`
//! * `instance_material`
//! * `newparam`
//! * `param`
//! * `phong`
//! * `profile_COMMON`
//! * `sampler2D`
//! * `source`
//! * `surface`
//! * `technique`
//! * `technique_common`
//! * `texture`
//! * `user_properties`

use minidom::{quick_xml, Element};
use std::{io::BufRead, str::FromStr};
use url::Url;

type XReader<R> = quick_xml::Reader<R>;

pub enum Error {
    Parse(minidom::Error),
    Other(&'static str),
    Str(String),
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

fn parse_text(element: &Element) -> Result<String, Error> {
    Ok(get_text(element).ok_or("expecting a text node")?.to_owned())
}

fn parse_float_list(element: &Element) -> Result<Box<[f32]>, Error> {
    get_text(element)
        .ok_or("expecting a text node")?
        .split_ascii_whitespace()
        .map(|s| s.parse())
        .collect::<Result<_, _>>()
        .map_err(|_| "failed to parse float".into())
}

fn parse_float_array<const N: usize>(element: &Element) -> Result<Box<[f32; N]>, Error> {
    Ok(parse_float_list(element)?
        .try_into()
        .map_err(|_| "unexpected number of elements")?)
}

fn parse_attr<T: FromStr>(attr: Option<&str>) -> Result<Option<T>, Error> {
    Ok(match attr {
        None => None,
        Some(s) => Some(s.parse().map_err(|_| "parse failure")?),
    })
}

fn parse_one<'a, T>(
    name: &str,
    it: &mut impl Iterator<Item = &'a Element>,
    f: impl FnOnce(&'a Element) -> Result<T, Error>,
) -> Result<T, Error> {
    let e = it.next().ok_or_else(|| format!("expected <{}>", name))?;
    if e.name() != name {
        Err(format!("expected <{}>", name))?
    }
    f(e)
}

fn parse_one_many<'a, T>(
    it: &mut impl Iterator<Item = &'a Element>,
    f: impl FnOnce(&'a Element) -> Result<Option<T>, Error>,
) -> Result<T, Error> {
    let e = it.next().ok_or_else(|| "expected element")?;
    Ok(f(e)?.ok_or_else(|| "expected element")?)
}

fn parse_opt<'a, T>(
    name: &str,
    it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    f: impl FnOnce(&'a Element) -> Result<T, Error>,
) -> Result<Option<T>, Error> {
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
    it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    f: impl FnOnce(&'a Element) -> Result<Option<T>, Error>,
) -> Result<Option<T>, Error> {
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
    it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    mut f: impl FnMut(&'a Element) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
    parse_list_many(it, |e| {
        Ok(if e.name() == name { Some(f(e)?) } else { None })
    })
}

fn finish<'a, T>(t: T, mut it: impl Iterator<Item = &'a Element>) -> Result<T, Error> {
    if let Some(e) = it.next() {
        Err(format!("unexpected node <{}>", e.name()))?
    }
    Ok(t)
}

fn parse_list_many<'a, T>(
    it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    mut f: impl FnMut(&'a Element) -> Result<Option<T>, Error>,
) -> Result<Vec<T>, Error> {
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

#[derive(Debug)]
pub struct Extra {
    // TODO: technique, user_properties
    pub element: Element,
}

impl Extra {
    pub fn parse_many<'a>(it: impl Iterator<Item = &'a Element>) -> Result<Vec<Extra>, Error> {
        let mut extras = vec![];
        for e in it {
            match e.name() {
                "extra" => extras.push(Extra::parse(e)?),
                k => Err(format!("unexpected element {}", k))?,
            }
        }
        Ok(extras)
    }

    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "extra");
        Ok(Extra {
            element: element.clone(),
        })
    }
}

#[derive(Debug)]
pub struct Contributor {
    pub author: Option<String>,
    pub authoring_tool: Option<String>,
    pub comments: Option<String>,
    pub copyright: Option<String>,
    pub source_data: Option<String>,
}

impl Contributor {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "node");
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

impl Unit {
    pub fn parse(element: &Element) -> Result<Self, Error> {
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

impl UpAxis {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        Ok(match get_text(element) {
            Some("X_UP") => UpAxis::XUp,
            Some("Y_UP") => UpAxis::YUp,
            Some("Z_UP") => UpAxis::ZUp,
            _ => Err("invalid <up_axis> value")?,
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

impl Asset {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "node");
        let mut it = element.children().peekable();
        let res = Asset {
            contributor: parse_list("contributor", &mut it, Contributor::parse)?,
            created: parse_one("created", &mut it, parse_text)?,
            keywords: parse_opt("keywords", &mut it, parse_text)?.map_or_else(Vec::new, |s| {
                s.split_ascii_whitespace().map(|s| s.to_owned()).collect()
            }),
            modified: parse_one("modified", &mut it, parse_text)?,
            revision: parse_opt("revision", &mut it, parse_text)?,
            subject: parse_opt("subject", &mut it, parse_text)?,
            title: parse_opt("title", &mut it, parse_text)?,
            unit: parse_opt("unit", &mut it, Unit::parse)?,
            up_axis: parse_opt("up_axis", &mut it, UpAxis::parse)?.unwrap_or_default(),
        };
        finish(res, it)
    }
}

#[derive(Debug)]
pub struct Animation {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl Animation {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "animation");
        Ok(Animation {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct AnimationClip {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl AnimationClip {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "animation_clip");
        Ok(AnimationClip {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Camera {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl Camera {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "camera");
        Ok(Camera {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Controller {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl Controller {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "controller");
        Ok(Controller {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Effect {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl Effect {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "effect");
        Ok(Effect {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct ForceField {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl ForceField {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "force_field");
        Ok(ForceField {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
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

fn parse_array<T: FromStr>(e: &Element) -> Result<Box<[T]>, Error> {
    let count: usize = parse_attr(e.attr("count"))?.ok_or("expected 'count' attr")?;
    let mut vec = Vec::with_capacity(count);
    for s in get_text(e).ok_or("expected text node")?.split_whitespace() {
        vec.push(s.parse().map_err(|_| "parse error")?)
    }
    if vec.len() != count {
        Err("'count' does not match array length")?
    }
    Ok(vec.into())
}

impl ArrayElement {
    pub fn parse(e: &Element) -> Result<Option<Self>, Error> {
        Ok(Some(match e.name() {
            "IDREF_array" => Self::IdRef(parse_array(e)?),
            "Name_array" => Self::Name(parse_array(e)?),
            "bool_array" => Self::Bool(parse_array(e)?),
            "float_array" => Self::Float(parse_array(e)?),
            "int_array" => Self::Int(parse_array(e)?),
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct Source {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Asset>,
    pub array: Option<ArrayElement>,
    pub technique_common: TechniqueCommon,
    pub technique: Vec<Technique>,
}

impl Source {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "source");
        let mut it = element.children().peekable();
        let res = Source {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            array: parse_opt_many(&mut it, ArrayElement::parse)?,
            technique_common: parse_one("technique_common", &mut it, TechniqueCommon::parse)?,
            technique: parse_list("technique", &mut it, Technique::parse)?,
        };
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

#[derive(Debug)]
pub struct Input {
    pub semantic: Semantic,
    pub source: Url,
}

impl Input {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "input");
        let semantic = element.attr("semantic").ok_or("missing semantic attr")?;
        let src = element.attr("source").ok_or("missing source attr")?;
        Ok(Input {
            semantic: Semantic::parse(semantic),
            source: Url::parse(src).map_err(|_| "url parse error")?,
        })
    }
}

#[derive(Debug)]
pub struct InputS {
    pub input: Input,
    pub offset: u32,
    pub set: Option<u32>,
}

impl InputS {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        Ok(InputS {
            input: Input::parse(element)?,
            offset: parse_attr(element.attr("offset"))?.ok_or("missing offset attr")?,
            set: parse_attr(element.attr("set"))?,
        })
    }
}

#[derive(Debug)]
pub struct InputList {
    pub inputs: Vec<InputS>,
    pub depth: u32,
}

impl InputList {
    pub fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        let inputs = parse_list("input", it, InputS::parse)?;
        let depth = inputs.iter().map(|i| i.offset).max().map_or(0, |n| n + 1);
        Ok(InputList { inputs, depth })
    }

    fn check_prim<const MIN: usize>(&self, data: &[u32]) -> bool {
        let depth = self.depth as usize;
        depth != 0 && data.len() < depth * MIN && data.len() % depth == 0
    }
}

#[derive(Debug)]
pub struct Vertices {
    pub id: String,
    pub name: Option<String>,
    pub input: Vec<Input>,
    pub extra: Vec<Extra>,
}

impl Vertices {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "vertices");
        let mut it = element.children().peekable();
        let res = Vertices {
            id: element.attr("id").ok_or("missing 'id' attr")?.into(),
            name: element.attr("name").map(Into::into),
            input: parse_list("input", &mut it, Input::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.input.is_empty() {
            Err("no inputs")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub struct Geom<T> {
    pub name: Option<String>,
    pub material: Option<String>,
    pub count: u32,
    pub inputs: InputList,
    pub data: T,
    pub extra: Vec<Extra>,
}

trait ParseGeom: Sized {
    const NAME: &'static str;
    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error>;

    fn validate(_: &Geom<Self>) -> Result<(), Error> {
        Ok(())
    }

    fn parse_geom(element: &Element) -> Result<Geom<Self>, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Geom {
            name: element.attr("name").map(Into::into),
            material: element.attr("material").map(Into::into),
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            inputs: InputList::parse(&mut it)?,
            data: Self::parse(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        Self::validate(&res)?;
        Ok(res)
    }
}

#[derive(Debug)]
pub struct LineGeom(pub Option<Box<[u32]>>);
pub type Lines = Geom<LineGeom>;

impl ParseGeom for LineGeom {
    const NAME: &'static str = "lines";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(LineGeom(parse_opt("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if let Some(ref data) = res.data.0 {
            if res.inputs.depth as usize * 2 * res.count as usize != data.len() {
                Err("line count does not match <p> field")?
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LineStripGeom(pub Vec<Box<[u32]>>);
pub type LineStrips = Geom<LineStripGeom>;

impl ParseGeom for LineStripGeom {
    const NAME: &'static str = "line_strips";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(LineStripGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if res.count as usize != res.data.0.len() {
            Err("line strip count does not match <p> fields")?
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<2>(p)) {
            Err("incorrect <p> field in line strips")?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct PolygonHole {
    pub verts: Box<[u32]>,
    pub hole: Vec<Box<[u32]>>,
}

#[derive(Debug)]
pub struct PolygonGeom(pub Vec<PolygonHole>);
pub type Polygons = Geom<PolygonGeom>;

impl ParseGeom for PolygonGeom {
    const NAME: &'static str = "polygon";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
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
                Err("<ph> element can only be used when at least one hole is present")?
            }
            finish(PolygonHole { verts, hole }, it)
        })?;
        polys.extend(more_polys);
        Ok(PolygonGeom(polys))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if res.count as usize != res.data.0.len() {
            Err("polygon count does not match <p> fields")?
        }
        if !res.data.0.iter().all(|ph| {
            res.inputs.check_prim::<3>(&ph.verts)
                && ph.hole.iter().all(|h| res.inputs.check_prim::<3>(h))
        }) {
            Err("incorrect <p> field in polygon")?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct PolyListGeom {
    pub vcount: Option<Box<[u32]>>,
    pub prim: Option<Box<[u32]>>,
}
pub type PolyList = Geom<PolyListGeom>;

impl ParseGeom for PolyListGeom {
    const NAME: &'static str = "polylist";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(PolyListGeom {
            vcount: parse_opt("vcount", it, parse_array)?,
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        match (&res.data.vcount, &res.data.prim) {
            (None, None) => {}
            (Some(vcount), Some(data)) => {
                if res.count as usize != vcount.len() {
                    Err("polylist count does not match <vcount> field")?
                }
                if res.inputs.depth as usize * vcount.iter().sum::<u32>() as usize != data.len() {
                    Err("polylist vcount does not match <p> field")?
                }
            }
            _ => Err("polylist: <vcount> and <p> should be provided together")?,
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct TriangleGeom(pub Option<Box<[u32]>>);
pub type Triangles = Geom<TriangleGeom>;

impl ParseGeom for TriangleGeom {
    const NAME: &'static str = "triangles";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(TriangleGeom(parse_opt("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if let Some(ref data) = res.data.0 {
            if res.inputs.depth as usize * 3 * res.count as usize != data.len() {
                Err("triangle count does not match <p> field")?
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct TriFanGeom(pub Vec<Box<[u32]>>);
pub type TriFans = Geom<TriFanGeom>;

impl ParseGeom for TriFanGeom {
    const NAME: &'static str = "trifans";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(TriFanGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if res.count as usize != res.data.0.len() {
            Err("triangle fan count does not match <p> fields")?
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            Err("incorrect <p> field in triangle fans")?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct TriStripGeom(pub Vec<Box<[u32]>>);
pub type TriStrips = Geom<TriStripGeom>;

impl ParseGeom for TriStripGeom {
    const NAME: &'static str = "tristrips";

    fn parse<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Self, Error> {
        Ok(TriStripGeom(parse_list("p", it, parse_array)?))
    }

    fn validate(res: &Geom<Self>) -> Result<(), Error> {
        if res.count as usize != res.data.0.len() {
            Err("triangle strip count does not match <p> fields")?
        }
        if !res.data.0.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            Err("incorrect <p> field in triangle strips")?
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
    pub fn parse(e: &Element) -> Result<Option<Self>, Error> {
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
    pub fn parse(element: &Element) -> Result<GeometryElement, Error> {
        debug_assert_eq!(element.name(), "convex_mesh");
        if let Some(s) = element.attr("convex_hull_of") {
            return Ok(GeometryElement::ConvexHullOf(
                Url::parse(s).map_err(|_| "url parse error")?,
            ));
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
    pub fn parse(convex: bool, element: &Element) -> Result<Self, Error> {
        let mut it = element.children().peekable();
        let res = Mesh {
            convex,
            source: parse_list("source", &mut it, Source::parse)?,
            vertices: parse_opt("vertices", &mut it, Vertices::parse)?,
            elements: parse_list_many(&mut it, Primitive::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.source.is_empty() {
            Err("no mesh source")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub struct ControlVertices {
    pub input: Vec<Input>,
    pub extra: Vec<Extra>,
}

impl ControlVertices {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "control_vertices");
        let mut it = element.children().peekable();
        let res = ControlVertices {
            input: parse_list("input", &mut it, Input::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.input.is_empty() {
            Err("no inputs")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub struct Spline {
    pub closed: bool,
    pub source: Vec<Source>,
    pub controls: ControlVertices,
    pub extra: Vec<Extra>,
}

impl Spline {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "spline");
        let mut it = element.children().peekable();
        let res = Spline {
            closed: parse_attr(element.attr("closed"))?.unwrap_or(false),
            source: parse_list("source", &mut it, Source::parse)?,
            controls: parse_one("control_vertices", &mut it, ControlVertices::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.source.is_empty() {
            Err("no spline source")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub enum GeometryElement {
    ConvexHullOf(Url),
    Mesh(Mesh),
    Spline(Spline),
}

impl GeometryElement {
    pub fn parse(element: &Element) -> Result<Option<Self>, Error> {
        Ok(Some(match element.name() {
            "convex_mesh" => ConvexMesh::parse(element)?,
            "mesh" => GeometryElement::Mesh(Mesh::parse(false, element)?),
            "spline" => GeometryElement::Spline(Spline::parse(element)?),
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct Geometry {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Asset>,
    pub element: GeometryElement,
    pub extra: Vec<Extra>,
}

impl Geometry {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "geometry");
        let mut it = element.children().peekable();
        Ok(Geometry {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
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

impl ImageSource {
    pub fn parse(element: &Element) -> Result<Option<Self>, Error> {
        Ok(Some(match element.name() {
            "data" => {
                let mut out = vec![];
                let s = get_text(element).ok_or("expected text element")?;
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
                ImageSource::Data(out.into())
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
    pub asset: Option<Asset>,
    pub source: ImageSource,
    pub extra: Vec<Extra>,
}

impl Image {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "image");
        let mut it = element.children().peekable();
        Ok(Image {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            format: element.attr("format").map(Into::into),
            height: parse_attr(element.attr("height"))?.unwrap_or(0),
            width: parse_attr(element.attr("width"))?.unwrap_or(0),
            depth: parse_attr(element.attr("depth"))?.unwrap_or(1),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            source: parse_one_many(&mut it, ImageSource::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Light {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl Light {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "light");
        Ok(Light {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Material {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Asset>,
    pub instance_effect: InstanceEffect,
    pub extra: Vec<Extra>,
}

impl Material {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "material");
        let mut it = element.children().peekable();
        Ok(Material {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            instance_effect: parse_one("instance_effect", &mut it, InstanceEffect::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct PhysicsMaterial {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl PhysicsMaterial {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "physics_material");
        Ok(PhysicsMaterial {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct PhysicsModel {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl PhysicsModel {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "physics_model");
        Ok(PhysicsModel {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct PhysicsScene {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl PhysicsScene {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "physics_scene");
        Ok(PhysicsScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct LookAt(pub Box<[f32; 9]>);

impl LookAt {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "lookat");
        Ok(LookAt(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Matrix(pub Box<[f32; 16]>);

impl Matrix {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "matrix");
        Ok(Matrix(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Rotate(pub Box<[f32; 4]>);

impl Rotate {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "Rotate");
        Ok(Rotate(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Scale(pub Box<[f32; 3]>);

impl Scale {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "scale");
        Ok(Scale(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Skew(pub Box<[f32; 7]>);

impl Skew {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "skew");
        Ok(Skew(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Translate(pub Box<[f32; 3]>);

impl Translate {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "translate");
        Ok(Translate(parse_float_array(element)?))
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

#[derive(Debug)]
pub struct Node {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Asset>,
    pub transforms: Vec<Transform>,
    pub instance_camera: Vec<Instance>,
    pub instance_controller: Vec<InstanceController>,
    pub instance_geometry: Vec<InstanceGeometry>,
    pub instance_light: Vec<Instance>,
    pub instance_node: Vec<Instance>,
    pub children: Vec<Node>,
    pub extra: Vec<Extra>,
}

impl Node {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "node");
        let mut it = element.children().peekable();
        Ok(Node {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            transforms: parse_list_many(&mut it, |e| match e.name() {
                "lookat" => Ok(Some(Transform::LookAt(LookAt::parse(e)?))),
                "matrix" => Ok(Some(Transform::Matrix(Matrix::parse(e)?))),
                "rotate" => Ok(Some(Transform::Rotate(Rotate::parse(e)?))),
                "scale" => Ok(Some(Transform::Scale(Scale::parse(e)?))),
                "skew" => Ok(Some(Transform::Skew(Skew::parse(e)?))),
                "translate" => Ok(Some(Transform::Translate(Translate::parse(e)?))),
                _ => Ok(None),
            })?,
            instance_camera: parse_list("instance_camera", &mut it, |e| {
                Instance::parse("instance_camera", e)
            })?,
            instance_controller: parse_list(
                "instance_controller",
                &mut it,
                InstanceController::parse,
            )?,
            instance_geometry: parse_list("instance_geometry", &mut it, InstanceGeometry::parse)?,
            instance_light: parse_list("instance_light", &mut it, |e| {
                Instance::parse("instance_light", e)
            })?,
            instance_node: parse_list("instance_node", &mut it, |e| {
                Instance::parse("instance_node", e)
            })?,
            children: parse_list("node", &mut it, Node::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct VisualScene {
    pub id: Option<String>,
    pub name: Option<String>,
    pub asset: Option<Asset>,
    pub nodes: Vec<Node>,
    // evaluate_scene: Vec<EvaluateScene>
    pub extra: Vec<Extra>,
}

impl VisualScene {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "visual_scene");
        let mut it = element.children().peekable();
        let res = VisualScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            nodes: parse_list("node", &mut it, Node::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.nodes.is_empty() {
            Err("no child nodes")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub struct Library<T> {
    pub asset: Option<Asset>,
    pub items: Vec<T>,
    pub extra: Vec<Extra>,
}

impl<T> Library<T> {
    pub fn parse(
        name: &str,
        element: &Element,
        f: impl FnMut(&Element) -> Result<T, Error>,
    ) -> Result<Self, Error> {
        let mut it = element.children().peekable();
        let res = Library {
            asset: parse_opt("asset", &mut it, Asset::parse)?,
            items: parse_list(name, &mut it, f)?,
            extra: Extra::parse_many(it)?,
        };
        if res.items.is_empty() {
            Err("no items")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub enum LibraryElement {
    Animations(Library<Animation>),
    AnimationClips(Library<AnimationClip>),
    Cameras(Library<Camera>),
    Controllers(Library<Controller>),
    Effects(Library<Effect>),
    ForceFields(Library<ForceField>),
    Geometries(Library<Geometry>),
    Images(Library<Image>),
    Lights(Library<Light>),
    Materials(Library<Material>),
    Nodes(Library<Node>),
    PhysicsMaterials(Library<PhysicsMaterial>),
    PhysicsModels(Library<PhysicsModel>),
    PhysicsScenes(Library<PhysicsScene>),
    VisualScenes(Library<VisualScene>),
}

impl LibraryElement {
    pub fn parse(e: &Element) -> Result<Option<Self>, Error> {
        use LibraryElement::*;
        Ok(Some(match e.name() {
            "library_animations" => Animations(Library::parse("animation", e, Animation::parse)?),
            "library_animation_clips" => {
                AnimationClips(Library::parse("animation_clip", e, AnimationClip::parse)?)
            }
            "library_cameras" => Cameras(Library::parse("camera", e, Camera::parse)?),
            "library_controllers" => {
                Controllers(Library::parse("controller", e, Controller::parse)?)
            }
            "library_effects" => Effects(Library::parse("effect", e, Effect::parse)?),
            "library_force_fields" => {
                ForceFields(Library::parse("force_field", e, ForceField::parse)?)
            }
            "library_geometries" => Geometries(Library::parse("geometry", e, Geometry::parse)?),
            "library_images" => Images(Library::parse("library_images", e, Image::parse)?),
            "library_lights" => Lights(Library::parse("library_lights", e, Light::parse)?),
            "library_materials" => Materials(Library::parse("material", e, Material::parse)?),
            "library_nodes" => Nodes(Library::parse("library_nodes", e, Node::parse)?),
            "library_physics_materials" => PhysicsMaterials(Library::parse(
                "physics_material",
                e,
                PhysicsMaterial::parse,
            )?),
            "library_physics_models" => {
                PhysicsModels(Library::parse("physics_model", e, PhysicsModel::parse)?)
            }
            "library_physics_scenes" => {
                PhysicsScenes(Library::parse("physics_scene", e, PhysicsScene::parse)?)
            }
            "library_visual_scenes" => {
                VisualScenes(Library::parse("visual_scene", e, VisualScene::parse)?)
            }
            _ => return Ok(None),
        }))
    }
}

#[derive(Debug)]
pub struct Instance {
    pub url: Url,
    pub extra: Vec<Extra>,
}

impl Instance {
    pub fn parse(name: &str, element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), name);
        let url = element.attr("url").ok_or("missing url attribute")?;
        Ok(Instance {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Param {
    // TODO
    pub extra: Vec<Extra>,
}

impl Param {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "param");
        Ok(Param {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct SetParam {
    // TODO
    pub extra: Vec<Extra>,
}

impl SetParam {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "setparam");
        Ok(SetParam {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct TechniqueCommon {
    // TODO
    pub extra: Vec<Extra>,
}

impl TechniqueCommon {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "technique_common");
        Ok(TechniqueCommon {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Technique {
    // TODO
    pub extra: Vec<Extra>,
}

impl Technique {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "technique");
        Ok(Technique {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct TechniqueHint {
    // TODO
    pub extra: Vec<Extra>,
}

impl TechniqueHint {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "technique_hint");
        Ok(TechniqueHint {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct BindMaterial {
    pub param: Vec<Param>,
    pub technique_common: TechniqueCommon,
    pub technique: Vec<Technique>,
    pub extra: Vec<Extra>,
}

impl BindMaterial {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "bind_material");
        let mut it = element.children().peekable();
        Ok(BindMaterial {
            param: parse_list("param", &mut it, Param::parse)?,
            technique_common: parse_one("technique_common", &mut it, TechniqueCommon::parse)?,
            technique: parse_list("technique", &mut it, Technique::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct InstanceGeometry {
    pub url: Url,
    pub bind_material: Option<BindMaterial>,
    pub extra: Vec<Extra>,
}

impl InstanceGeometry {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "instance_geometry");
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceGeometry {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            bind_material: parse_opt("bind_material", &mut it, BindMaterial::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Skeleton {
    // TODO
    pub extra: Vec<Extra>,
}

impl Skeleton {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "skeleton");
        Ok(Skeleton {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct InstanceController {
    pub url: Url,
    pub skeleton: Vec<Skeleton>,
    pub bind_material: Option<BindMaterial>,
    pub extra: Vec<Extra>,
}

impl InstanceController {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "instance_controller");
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceController {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            skeleton: parse_list("skeleton", &mut it, Skeleton::parse)?,
            bind_material: parse_opt("bind_material", &mut it, BindMaterial::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct InstanceEffect {
    pub url: Url,
    pub technique_hint: Vec<TechniqueHint>,
    pub set_param: Vec<SetParam>,
    pub extra: Vec<Extra>,
}

impl InstanceEffect {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "instance_effect");
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceEffect {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            technique_hint: parse_list("technique_hint", &mut it, TechniqueHint::parse)?,
            set_param: parse_list("setparam", &mut it, SetParam::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Default, Debug)]
pub struct Scene {
    pub instance_physics_scene: Vec<Instance>,
    pub instance_visual_scene: Option<Instance>,
    pub extra: Vec<Extra>,
}

impl Scene {
    pub fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), "scene");
        let mut it = element.children().peekable();
        Ok(Scene {
            instance_physics_scene: parse_list("instance_physics_scene", &mut it, |e| {
                Instance::parse("instance_physics_scene", e)
            })?,
            instance_visual_scene: parse_opt("instance_physics_scene", &mut it, |e| {
                Instance::parse("instance_visual_scene", e)
            })?,
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

impl Document {
    pub fn from_reader<R: BufRead>(reader: R) -> Result<Self, Error> {
        Self::from_xml_reader(&mut XReader::from_reader(reader))
    }

    pub fn from_xml_reader<R: BufRead>(reader: &mut XReader<R>) -> Result<Self, Error> {
        let root = minidom::Element::from_reader(reader)?;
        Self::parse(&root)
    }

    pub fn parse(element: &Element) -> Result<Self, Error> {
        if element.name() != "COLLADA" {
            return Err("Expected COLLADA root node".into());
        }
        if element.attr("version") != Some("1.4.1") {
            return Err("Unsupported COLLADA version".into());
        }
        let mut it = element.children().peekable();
        Ok(Document {
            asset: parse_one("asset", &mut it, Asset::parse)?,
            library: parse_list_many(&mut it, LibraryElement::parse)?,
            scene: parse_opt("scene", &mut it, Scene::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}
