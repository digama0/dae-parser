//! TODO:
//! * `ambient`
//! * `channel`
//! * `color`
//! * `diffuse`
//! * `float`
//! * `joints`
//! * `lambert`
//! * `newparam`
//! * `phong`
//! * `sampler`
//! * `sampler2D`
//! * `shininess`
//! * `skin`
//! * `source`
//! * `specular`
//! * `surface`
//! * `technique_common`
//! * `texture`
//! * `user_properties`
//! * `v`
//! * `vertex_weights`

use minidom::{quick_xml, Element};
use std::{io::BufRead, marker::PhantomData, str::FromStr};
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

pub trait XNode: Sized {
    const NAME: &'static str = "extra";
    fn parse(element: &Element) -> Result<Self, Error>;

    fn parse_one<'a>(it: &mut impl Iterator<Item = &'a Element>) -> Result<Self, Error> {
        parse_one(Self::NAME, it, Self::parse)
    }

    fn parse_opt<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Option<Self>, Error> {
        parse_opt(Self::NAME, it, Self::parse)
    }

    fn parse_list<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Vec<Self>, Error> {
        parse_list(Self::NAME, it, Self::parse)
    }
}

#[derive(Debug)]
pub struct Extra {
    pub id: Option<String>,
    pub name: Option<String>,
    pub ty: Option<String>,
    pub asset: Option<Box<Asset>>,
    pub technique: Vec<Technique>,
}

impl XNode for Extra {
    const NAME: &'static str = "extra";
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse_many<'a>(it: impl Iterator<Item = &'a Element>) -> Result<Vec<Extra>, Error> {
        let mut extras = vec![];
        for e in it {
            match e.name() {
                "extra" => extras.push(Extra::parse(e)?),
                k => Err(format!("unexpected element {}", k))?,
            }
        }
        Ok(extras)
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

impl XNode for Contributor {
    const NAME: &'static str = "contributor";
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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
    fn parse_box(element: &Element) -> Result<Box<Self>, Error> {
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

    fn parse_opt_box<'a>(
        it: &mut std::iter::Peekable<impl Iterator<Item = &'a Element>>,
    ) -> Result<Option<Box<Self>>, Error> {
        parse_opt(Self::NAME, it, Self::parse_box)
    }
}
impl XNode for Asset {
    const NAME: &'static str = "asset";
    fn parse(element: &Element) -> Result<Self, Error> {
        Ok(*Self::parse_box(element)?)
    }
}

pub trait ParseLibrary: XNode {
    const LIBRARY: &'static str;
}

#[derive(Debug)]
pub struct Animation {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for Animation {
    const NAME: &'static str = "animation";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for AnimationClip {
    const NAME: &'static str = "animation_clip";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for Camera {
    const NAME: &'static str = "camera";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for Controller {
    const NAME: &'static str = "controller";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Controller {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Annotate {
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for Annotate {
    const NAME: &'static str = "annotate";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Annotate {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct NewParam {
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for NewParam {
    const NAME: &'static str = "newparam";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(NewParam {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct TechniqueFx {
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for TechniqueFx {
    const NAME: &'static str = "technique";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(TechniqueFx {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct ProfileCommon {
    pub asset: Option<Box<Asset>>,
    pub image: Vec<Image>,
    pub new_param: Vec<NewParam>,
    pub technique: Vec<TechniqueFx>,
    pub extra: Vec<Extra>,
}

impl XNode for ProfileCommon {
    const NAME: &'static str = "profile_COMMON";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let asset = Asset::parse_opt_box(&mut it)?;
        let image_and_newparam = parse_list_many(&mut it, |e| {
            Ok(Some(match e.name() {
                Image::NAME => Ok(Image::parse(e)?),
                NewParam::NAME => Err(NewParam::parse(e)?),
                _ => return Ok(None),
            }))
        })?;
        let mut image = vec![];
        let mut new_param = vec![];
        for either in image_and_newparam {
            match either {
                Ok(e) => image.push(e),
                Err(e) => new_param.push(e),
            }
        }
        Ok(ProfileCommon {
            asset,
            image,
            new_param,
            technique: TechniqueFx::parse_list(&mut it)?,
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct ProfileCG(pub Element); // TODO

impl XNode for ProfileCG {
    const NAME: &'static str = "profile_CG";
    fn parse(element: &Element) -> Result<Self, Error> {
        Ok(ProfileCG(element.clone()))
    }
}

#[derive(Debug)]
pub struct ProfileGLES(pub Element); // TODO

impl ProfileGLES {
    const NAME: &'static str = "profile_GLES";
    fn parse(element: &Element) -> Result<Self, Error> {
        Ok(ProfileGLES(element.clone()))
    }
}

#[derive(Debug)]
pub struct ProfileGLSL(pub Element); // TODO

impl ProfileGLSL {
    const NAME: &'static str = "profile_GLSL";
    fn parse(element: &Element) -> Result<Self, Error> {
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
    pub fn parse(e: &Element) -> Result<Option<Self>, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
            extra: Extra::parse_many(element.children())?,
        };
        if res.profile.is_empty() {
            Err("expected at least one profile")?
        }
        Ok(res)
    }
}

#[derive(Debug)]
pub struct ForceField {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for ForceField {
    const NAME: &'static str = "force_field";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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
    pub asset: Option<Box<Asset>>,
    pub array: Option<ArrayElement>,
    pub accessor: Accessor,
    pub technique: Vec<Technique>,
}

impl XNode for Source {
    const NAME: &'static str = "source";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Source {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            array: parse_opt_many(&mut it, ArrayElement::parse)?,
            accessor: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_one(Accessor::NAME, &mut it, Accessor::parse)?, it)
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let src = element.attr("source").ok_or("missing source attr")?;
        let mut it = element.children().peekable();
        let res = Accessor {
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            offset: parse_attr(element.attr("offset"))?.unwrap_or(0),
            source: Url::parse(src).map_err(|_| "url parse error")?,
            stride: parse_attr(element.attr("stride"))?.unwrap_or(1),
            param: Param::parse_list(&mut it)?,
        };
        if res.stride < res.param.len() {
            Err("accessor stride does not match params")?
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

#[derive(Debug)]
pub struct Input {
    pub semantic: Semantic,
    pub source: Url,
}

impl XNode for Input {
    const NAME: &'static str = "input";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for InputS {
    const NAME: &'static str = "input";
    fn parse(element: &Element) -> Result<Self, Error> {
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
        let inputs = InputS::parse_list(it)?;
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

impl XNode for Vertices {
    const NAME: &'static str = "vertices";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Vertices {
            id: element.attr("id").ok_or("missing 'id' attr")?.into(),
            name: element.attr("name").map(Into::into),
            input: Input::parse_list(&mut it)?,
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

pub trait ParseGeom: Sized {
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
impl<T: ParseGeom> Geom<T> {
    pub const NAME: &'static str = T::NAME;
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
    pub const NAME: &'static str = "convex_mesh";
    pub fn parse(element: &Element) -> Result<GeometryElement, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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
        debug_assert_eq!(
            element.name(),
            if convex { ConvexMesh::NAME } else { Self::NAME }
        );
        let mut it = element.children().peekable();
        let res = Mesh {
            convex,
            source: Source::parse_list(&mut it)?,
            vertices: Vertices::parse_opt(&mut it)?,
            elements: parse_list_many(&mut it, Primitive::parse)?,
            extra: Extra::parse_many(it)?,
        };
        if res.source.is_empty() {
            Err("no mesh source")?
        }
        Ok(res)
    }
}

impl XNode for Mesh {
    const NAME: &'static str = "mesh";
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = ControlVertices {
            input: Input::parse_list(&mut it)?,
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

impl XNode for Spline {
    const NAME: &'static str = "spline";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Spline {
            closed: parse_attr(element.attr("closed"))?.unwrap_or(false),
            source: Source::parse_list(&mut it)?,
            controls: ControlVertices::parse_one(&mut it)?,
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
    pub fn parse(element: &Element) -> Result<Option<Self>, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
pub struct Light {
    pub id: Option<String>,
    pub name: Option<String>,
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for Light {
    const NAME: &'static str = "light";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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
    pub asset: Option<Box<Asset>>,
    pub instance_effect: InstanceEffect,
    pub extra: Vec<Extra>,
}

impl XNode for Material {
    const NAME: &'static str = "material";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Material {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            instance_effect: InstanceEffect::parse_one(&mut it)?,
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

impl XNode for PhysicsMaterial {
    const NAME: &'static str = "physics_material";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for PhysicsModel {
    const NAME: &'static str = "physics_model";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for PhysicsScene {
    const NAME: &'static str = "physics_scene";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(PhysicsScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct LookAt(pub Box<[f32; 9]>);

impl XNode for LookAt {
    const NAME: &'static str = "lookat";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(LookAt(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Matrix(pub Box<[f32; 16]>);

impl XNode for Matrix {
    const NAME: &'static str = "matrix";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Matrix(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Rotate(pub Box<[f32; 4]>);

impl XNode for Rotate {
    const NAME: &'static str = "Rotate";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Rotate(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Scale(pub Box<[f32; 3]>);

impl XNode for Scale {
    const NAME: &'static str = "scale";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Scale(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Skew(pub Box<[f32; 7]>);

impl XNode for Skew {
    const NAME: &'static str = "skew";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Skew(parse_float_array(element)?))
    }
}

#[derive(Debug)]
pub struct Translate(pub Box<[f32; 3]>);

impl XNode for Translate {
    const NAME: &'static str = "translate";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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
    pub asset: Option<Box<Asset>>,
    pub transforms: Vec<Transform>,
    pub instance_camera: Vec<Instance<Camera>>,
    pub instance_controller: Vec<InstanceController>,
    pub instance_geometry: Vec<InstanceGeometry>,
    pub instance_light: Vec<Instance<Light>>,
    pub instance_node: Vec<Instance<Node>>,
    pub children: Vec<Node>,
    pub extra: Vec<Extra>,
}

impl XNode for Node {
    const NAME: &'static str = "node";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Node {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            transforms: parse_list_many(&mut it, |e| match e.name() {
                LookAt::NAME => Ok(Some(Transform::LookAt(LookAt::parse(e)?))),
                Matrix::NAME => Ok(Some(Transform::Matrix(Matrix::parse(e)?))),
                Rotate::NAME => Ok(Some(Transform::Rotate(Rotate::parse(e)?))),
                Scale::NAME => Ok(Some(Transform::Scale(Scale::parse(e)?))),
                Skew::NAME => Ok(Some(Transform::Skew(Skew::parse(e)?))),
                Translate::NAME => Ok(Some(Transform::Translate(Translate::parse(e)?))),
                _ => Ok(None),
            })?,
            instance_camera: Instance::parse_list(&mut it)?,
            instance_controller: InstanceController::parse_list(&mut it)?,
            instance_geometry: InstanceGeometry::parse_list(&mut it)?,
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = VisualScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            nodes: Node::parse_list(&mut it)?,
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
    pub asset: Option<Box<Asset>>,
    pub items: Vec<T>,
    pub extra: Vec<Extra>,
}

impl<T: ParseLibrary> XNode for Library<T> {
    const NAME: &'static str = T::LIBRARY;
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Library {
            asset: Asset::parse_opt_box(&mut it)?,
            items: T::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        if res.items.is_empty() {
            Err("no items")?
        }
        Ok(res)
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
            pub fn parse(e: &Element) -> Result<Option<Self>, Error> {
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
pub struct Instance<T> {
    pub url: Url,
    pub extra: Vec<Extra>,
    pub _marker: PhantomData<T>,
}

pub trait Instantiate {
    const INSTANCE: &'static str;
}

impl Instantiate for Camera {
    const INSTANCE: &'static str = "instance_camera";
}
impl Instantiate for Light {
    const INSTANCE: &'static str = "instance_light";
}
impl Instantiate for Node {
    const INSTANCE: &'static str = "instance_node";
}
impl Instantiate for PhysicsScene {
    const INSTANCE: &'static str = "instance_physics_scene";
}
impl Instantiate for VisualScene {
    const INSTANCE: &'static str = "instance_visual_scene";
}

impl<T: Instantiate> XNode for Instance<T> {
    const NAME: &'static str = T::INSTANCE;
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let url = element.attr("url").ok_or("missing url attribute")?;
        Ok(Instance {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            extra: Extra::parse_many(element.children())?,
            _marker: PhantomData,
        })
    }
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Param {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            ty: element.attr("type").ok_or("expecting 'type' attr")?.into(),
            semantic: element.attr("semantic").map(Semantic::parse),
        })
    }
}

#[derive(Debug)]
pub struct SetParam {
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for SetParam {
    const NAME: &'static str = "setparam";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(SetParam {
            extra: Extra::parse_many(element.children())?,
        })
    }
}

#[derive(Debug)]
pub struct Technique {
    pub element: Element,
}

impl XNode for Technique {
    const NAME: &'static str = "technique";
    fn parse(element: &Element) -> Result<Self, Error> {
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
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for TechniqueHint {
    const NAME: &'static str = "technique_hint";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(TechniqueHint {
            extra: Extra::parse_many(element.children())?,
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("missing target attribute")?;
        let symbol = element.attr("symbol").ok_or("expecting symbol attr")?;
        let mut it = element.children().peekable();
        Ok(InstanceMaterial {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            target: Url::parse(target).map_err(|_| "url parse error")?,
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
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(BindMaterial {
            param: Param::parse_list(&mut it)?,
            instance_material: parse_one(Technique::COMMON, &mut it, |e| {
                let mut it = e.children().peekable();
                let res = InstanceMaterial::parse_list(&mut it)?;
                if res.is_empty() {
                    Err("expecting at least one <instance_material>")?
                }
                Ok(res)
            })?,
            technique: Technique::parse_list(&mut it)?,
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

impl XNode for InstanceGeometry {
    const NAME: &'static str = "instance_geometry";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceGeometry {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            bind_material: BindMaterial::parse_opt(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

#[derive(Debug)]
pub struct Skeleton {
    // TODO
    pub extra: Vec<Extra>,
}

impl XNode for Skeleton {
    const NAME: &'static str = "skeleton";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
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

impl XNode for InstanceController {
    const NAME: &'static str = "instance_controller";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceController {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            skeleton: Skeleton::parse_list(&mut it)?,
            bind_material: BindMaterial::parse_opt(&mut it)?,
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

impl XNode for InstanceEffect {
    const NAME: &'static str = "instance_effect";
    fn parse(element: &Element) -> Result<Self, Error> {
        debug_assert_eq!(element.name(), Self::NAME);
        let url = element.attr("url").ok_or("missing url attribute")?;
        let mut it = element.children().peekable();
        Ok(InstanceEffect {
            url: Url::parse(url).map_err(|_| "url parse error")?,
            technique_hint: TechniqueHint::parse_list(&mut it)?,
            set_param: SetParam::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
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
    fn parse(element: &Element) -> Result<Self, Error> {
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

impl Document {
    pub fn from_reader<R: BufRead>(reader: R) -> Result<Self, Error> {
        Self::from_xml_reader(&mut XReader::from_reader(reader))
    }

    pub fn from_xml_reader<R: BufRead>(reader: &mut XReader<R>) -> Result<Self, Error> {
        let root = minidom::Element::from_reader(reader)?;
        Self::parse(&root)
    }
}

impl XNode for Document {
    const NAME: &'static str = "COLLADA";
    fn parse(element: &Element) -> Result<Self, Error> {
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
