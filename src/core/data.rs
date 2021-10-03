use crate::*;

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
    pub(crate) fn parse<const N: usize>(it: &mut ElementIter<'_>) -> Result<Self> {
        let inputs = InputS::parse_list_n::<N>(it)?;
        let depth = inputs.iter().map(|i| i.offset).max().map_or(0, |n| n + 1) as usize;
        Ok(InputList { inputs, depth })
    }

    pub(crate) fn check_prim<const MIN: usize>(&self, data: &[u32]) -> bool {
        self.depth != 0 && data.len() < self.depth * MIN && data.len() % self.depth == 0
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