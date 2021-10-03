use crate::*;

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
