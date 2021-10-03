use crate::*;

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
