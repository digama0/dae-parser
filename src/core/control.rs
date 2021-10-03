use crate::*;

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
