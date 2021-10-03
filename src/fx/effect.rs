use crate::*;

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

impl HasId for Effect {
    fn id(&self) -> Option<&str> {
        Some(&self.id)
    }
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

/// A trait for the types that are legal to go in a [`TechniqueFx<T>`].
pub trait ProfileData: Sized {
    /// Parse the embedded data from a subsequence of children in the `<technique>` node.
    fn parse(it: &mut ElementIter<'_>) -> Result<Self>;
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

impl<T> HasId for TechniqueFx<T> {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
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
