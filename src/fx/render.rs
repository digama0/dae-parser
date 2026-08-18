use crate::*;

/// Describes one effect pass to evaluate a scene.
#[derive(Clone, Debug)]
pub struct Render {
    /// Refers to a node that contains a camera describing the viewpoint
    /// from which to render this compositing step.
    pub camera_node: UrlRef<Node>,
    /// Specifies which layer or layers to render in this compositing step
    /// while evaluating the scene.
    pub layers: Vec<String>,
    /// Instantiates a COLLADA material resource. See [`InstanceEffectData`]
    /// for the additional instance effect data.
    pub instance_effect: Option<Instance<Effect>>,
}

impl Render {
    /// Construct a new render pass.
    pub fn new(camera_node: Url, layers: Vec<String>, instance_effect: Url) -> Self {
        Self {
            camera_node: Ref::new(camera_node),
            layers,
            instance_effect: Some(Instance::new(instance_effect)),
        }
    }
}

impl XNode for Render {
    const NAME: &'static str = "render";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Render {
            camera_node: parse_attr(element.attr("camera_node"))?
                .ok_or("missing camera_node attr")?,
            layers: parse_list("layer", &mut it, parse_text)?,
            instance_effect: Instance::parse_opt(&mut it)?,
        };
        finish(res, it)
    }
}

impl XNodeWrite for Render {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let mut e = Self::elem();
        e.print_attr("camera_node", &self.camera_node);
        let e = e.start(w)?;
        many(&self.layers, |e| ElemBuilder::print_str("layer", e, w))?;
        self.instance_effect.write_to(w)?;
        e.end(w)
    }
}

/// A shader element.
#[derive(Clone, Debug, PartialEq)]
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

impl From<Blinn> for Shader {
    fn from(v: Blinn) -> Self {
        Self::Blinn(v)
    }
}

impl From<ConstantFx> for Shader {
    fn from(v: ConstantFx) -> Self {
        Self::Constant(v)
    }
}

impl From<Lambert> for Shader {
    fn from(v: Lambert) -> Self {
        Self::Lambert(v)
    }
}

impl From<Phong> for Shader {
    fn from(v: Phong) -> Self {
        Self::Phong(v)
    }
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

    /// Run the function `f` on all arguments of type [`Texture`] in the parameters to this shader.
    pub fn on_textures<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Blinn(s) => s.on_textures(f),
            Self::Constant(s) => s.on_textures(f),
            Self::Lambert(s) => s.on_textures(f),
            Self::Phong(s) => s.on_textures(f),
        }
    }
}

impl XNodeWrite for Shader {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        match self {
            Self::Blinn(e) => e.write_to(w),
            Self::Constant(e) => e.write_to(w),
            Self::Lambert(e) => e.write_to(w),
            Self::Phong(e) => e.write_to(w),
        }
    }
}

/// Specifies from which channel to take transparency information.
/// This is the `opaque` attribute of the `<transparent>` element, which is the only
/// element of type `common_color_or_texture_type` to have an attribute.
///
/// If either `<transparent>` or `<transparency>` exists then transparency rendering is
/// activated, the renderer needs to turn on alpha blending mode, and the equations given on the
/// individual variants define how to combine the two values. Use these equations to get the
/// correct results based on the opaque setting of `<transparent>`, where `fb` is the frame
/// buffer (that is, the image behind what is being rendered) and `mat` is the material color
/// before the transparency calculation.
///
/// The interaction between `<transparent>` and `<transparency>` is as follows:
/// * If `<transparent>` does not exist then it has no effect on the equation's result, and the
///   opaque mode is the default opaque mode. This is equivalent to:
///   `transparent = <color> 1.0 1.0 1.0 1.0 </color>`
/// * If `<transparency>` does not exist then it has no effect on the equation's result. This is
///   equivalent to a factor that is 1.0: `transparency = <float> 1.0 </float>`
/// * If both `<transparent>` and `<transparency>` exist then both are honored.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum TransparencyMode {
    /// Takes the transparency information from the color's alpha channel,
    /// where the value 1.0 is opaque. This is the default.
    ///
    /// ```text
    /// result.r = fb.r * (1.0f - transparent.a * transparency) + mat.r * (transparent.a * transparency)
    /// result.g = fb.g * (1.0f - transparent.a * transparency) + mat.g * (transparent.a * transparency)
    /// result.b = fb.b * (1.0f - transparent.a * transparency) + mat.b * (transparent.a * transparency)
    /// result.a = fb.a * (1.0f - transparent.a * transparency) + mat.a * (transparent.a * transparency)
    /// ```
    #[default]
    AOne,
    /// Takes the transparency information from the color's red, green, and blue channels,
    /// where the value 0.0 is opaque, with each channel modulated independently.
    ///
    /// ```text
    /// result.r = fb.r * (transparent.r * transparency) + mat.r * (1.0f - transparent.r * transparency)
    /// result.g = fb.g * (transparent.g * transparency) + mat.g * (1.0f - transparent.g * transparency)
    /// result.b = fb.b * (transparent.b * transparency) + mat.b * (1.0f - transparent.b * transparency)
    /// result.a = fb.a * (luminance(transparent.rgb) * transparency)
    ///          + mat.a * (1.0f - luminance(transparent.rgb) * transparency)
    /// ```
    ///
    /// where `luminance` is the function, based on the ISO/CIE color standards
    /// (see ITU-R Recommendation BT.709-4), that averages the color channels into one value:
    ///
    /// ```text
    /// luminance = (color.r * 0.212671) + (color.g * 0.715160) + (color.b * 0.072169)
    /// ```
    RgbZero,
}

impl FromStr for TransparencyMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A_ONE" => Ok(Self::AOne),
            "RGB_ZERO" => Ok(Self::RgbZero),
            _ => Err(()),
        }
    }
}

impl TransparencyMode {
    /// The XML name of a value in this enumeration.
    pub fn to_str(self) -> &'static str {
        match self {
            Self::AOne => "A_ONE",
            Self::RgbZero => "RGB_ZERO",
        }
    }
}

impl Display for TransparencyMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.to_str(), f)
    }
}

/// Parse the `opaque` attribute off the upcoming `<transparent>` element, if there is one,
/// without consuming it. Absent `<transparent>` and absent attribute both yield the default.
fn parse_transparency_mode(it: &mut ElementIter<'_>) -> Result<TransparencyMode> {
    let mode = match it.peek() {
        Some(e) if e.name() == "transparent" => parse_attr(e.attr("opaque"))?,
        _ => None,
    };
    Ok(mode.unwrap_or_default())
}

fn write_transparent_opt(
    transparent: &Option<WithSid<ColorParam>>,
    transparency_mode: TransparencyMode,
    w: &mut XWriter<impl Write>,
) -> Result<()> {
    opt(transparent, |param| {
        let mut elem = ElemBuilder::new("transparent");
        elem.def_print_attr("opaque", transparency_mode, Default::default());
        let elem = elem.start(w)?;
        param.write_to(w)?;
        elem.end(w)
    })
}

/// Produces a specularly shaded surface with a Blinn BRDF approximation.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Blinn {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<WithSid<ColorParam>>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<WithSid<ColorParam>>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<WithSid<ColorParam>>,
    /// Declares the color of light specularly reflected from the surface of this object.
    pub specular: Option<WithSid<ColorParam>>,
    /// Declares the specularity or roughness of the specular reflection lobe.
    pub shininess: Option<WithSid<FloatParam>>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<WithSid<FloatParam>>,
    /// Specifies from which channel to take transparency information,
    /// for the [`transparent`](Self::transparent) color parameter.
    /// The default is [`TransparencyMode::AOne`].
    pub transparency_mode: TransparencyMode,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<WithSid<FloatParam>>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<WithSid<FloatParam>>,
}

impl XNode for Blinn {
    const NAME: &'static str = "blinn";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Blinn {
            emission: parse_opt("emission", &mut it, WithSid::parse)?,
            ambient: parse_opt("ambient", &mut it, WithSid::parse)?,
            diffuse: parse_opt("diffuse", &mut it, WithSid::parse)?,
            specular: parse_opt("specular", &mut it, WithSid::parse)?,
            shininess: parse_opt("shininess", &mut it, WithSid::parse)?,
            reflective: parse_opt("reflective", &mut it, WithSid::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, WithSid::parse)?,
            transparency_mode: parse_transparency_mode(&mut it)?,
            transparent: parse_opt("transparent", &mut it, WithSid::parse)?,
            transparency: parse_opt("transparency", &mut it, WithSid::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, WithSid::parse)?,
        })
    }
}

impl XNodeWrite for Blinn {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        WithSid::write_opt(&self.emission, "emission", w)?;
        WithSid::write_opt(&self.ambient, "ambient", w)?;
        WithSid::write_opt(&self.diffuse, "diffuse", w)?;
        WithSid::write_opt(&self.specular, "specular", w)?;
        WithSid::write_opt(&self.shininess, "shininess", w)?;
        WithSid::write_opt(&self.reflective, "reflective", w)?;
        WithSid::write_opt(&self.reflectivity, "reflectivity", w)?;
        write_transparent_opt(&self.transparent, self.transparency_mode, w)?;
        WithSid::write_opt(&self.transparency, "transparency", w)?;
        WithSid::write_opt(&self.index_of_refraction, "index_of_refraction", w)?;
        e.end(w)
    }
}

impl Blinn {
    /// Run the function `f` on all arguments of type [`Texture`] in the parameters to this shader.
    pub fn on_textures<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
    ) -> Result<(), E> {
        on_color_as_texture(&self.emission, f)?;
        on_color_as_texture(&self.ambient, f)?;
        on_color_as_texture(&self.diffuse, f)?;
        on_color_as_texture(&self.specular, f)?;
        on_color_as_texture(&self.reflective, f)?;
        on_color_as_texture(&self.transparent, f)
    }
}

/// Produces a constantly shaded surface that is independent of lighting.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct ConstantFx {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<WithSid<ColorParam>>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<WithSid<FloatParam>>,
    /// Specifies from which channel to take transparency information,
    /// for the [`transparent`](Self::transparent) color parameter.
    /// The default is [`TransparencyMode::AOne`].
    pub transparency_mode: TransparencyMode,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<WithSid<FloatParam>>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<WithSid<FloatParam>>,
}

impl XNode for ConstantFx {
    const NAME: &'static str = "constant";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(ConstantFx {
            emission: parse_opt("emission", &mut it, WithSid::parse)?,
            reflective: parse_opt("reflective", &mut it, WithSid::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, WithSid::parse)?,
            transparency_mode: parse_transparency_mode(&mut it)?,
            transparent: parse_opt("transparent", &mut it, WithSid::parse)?,
            transparency: parse_opt("transparency", &mut it, WithSid::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, WithSid::parse)?,
        })
    }
}

impl XNodeWrite for ConstantFx {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        WithSid::write_opt(&self.emission, "emission", w)?;
        WithSid::write_opt(&self.reflective, "reflective", w)?;
        WithSid::write_opt(&self.reflectivity, "reflectivity", w)?;
        write_transparent_opt(&self.transparent, self.transparency_mode, w)?;
        WithSid::write_opt(&self.transparency, "transparency", w)?;
        WithSid::write_opt(&self.index_of_refraction, "index_of_refraction", w)?;
        e.end(w)
    }
}

impl ConstantFx {
    /// Run the function `f` on all arguments of type [`Texture`] in the parameters to this shader.
    pub fn on_textures<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
    ) -> Result<(), E> {
        on_color_as_texture(&self.emission, f)?;
        on_color_as_texture(&self.reflective, f)?;
        on_color_as_texture(&self.transparent, f)
    }
}

/// Produces a diffuse shaded surface that is independent of lighting.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Lambert {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<WithSid<ColorParam>>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<WithSid<ColorParam>>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<WithSid<ColorParam>>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<WithSid<FloatParam>>,
    /// Specifies from which channel to take transparency information,
    /// for the [`transparent`](Self::transparent) color parameter.
    /// The default is [`TransparencyMode::AOne`].
    pub transparency_mode: TransparencyMode,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<WithSid<FloatParam>>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<WithSid<FloatParam>>,
}

impl XNode for Lambert {
    const NAME: &'static str = "lambert";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Lambert {
            emission: parse_opt("emission", &mut it, WithSid::parse)?,
            ambient: parse_opt("ambient", &mut it, WithSid::parse)?,
            diffuse: parse_opt("diffuse", &mut it, WithSid::parse)?,
            reflective: parse_opt("reflective", &mut it, WithSid::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, WithSid::parse)?,
            transparency_mode: parse_transparency_mode(&mut it)?,
            transparent: parse_opt("transparent", &mut it, WithSid::parse)?,
            transparency: parse_opt("transparency", &mut it, WithSid::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, WithSid::parse)?,
        })
    }
}

impl XNodeWrite for Lambert {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        WithSid::write_opt(&self.emission, "emission", w)?;
        WithSid::write_opt(&self.ambient, "ambient", w)?;
        WithSid::write_opt(&self.diffuse, "diffuse", w)?;
        WithSid::write_opt(&self.reflective, "reflective", w)?;
        WithSid::write_opt(&self.reflectivity, "reflectivity", w)?;
        write_transparent_opt(&self.transparent, self.transparency_mode, w)?;
        WithSid::write_opt(&self.transparency, "transparency", w)?;
        WithSid::write_opt(&self.index_of_refraction, "index_of_refraction", w)?;
        e.end(w)
    }
}

impl Lambert {
    /// Run the function `f` on all arguments of type [`Texture`] in the parameters to this shader.
    pub fn on_textures<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
    ) -> Result<(), E> {
        on_color_as_texture(&self.emission, f)?;
        on_color_as_texture(&self.ambient, f)?;
        on_color_as_texture(&self.diffuse, f)?;
        on_color_as_texture(&self.reflective, f)?;
        on_color_as_texture(&self.transparent, f)
    }
}

/// Produces a specularly shaded surface where the specular reflection is shaded
/// according the Phong BRDF approximation.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Phong {
    /// Declares the amount of light emitted from the surface of this object.
    pub emission: Option<WithSid<ColorParam>>,
    /// Declares the amount of ambient light emitted from the surface of this object.
    pub ambient: Option<WithSid<ColorParam>>,
    /// Declares the amount of light diffusely reflected from the surface of this object.
    pub diffuse: Option<WithSid<ColorParam>>,
    /// the surface of this object.  the surface of this object.
    pub specular: Option<WithSid<ColorParam>>,
    /// reflection lobe.reflection lobe.
    pub shininess: Option<WithSid<FloatParam>>,
    /// Declares the color of a perfect mirror reflection.
    pub reflective: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfect mirror reflection to be added
    /// to the reflected light as a value between 0.0 and 1.0.
    pub reflectivity: Option<WithSid<FloatParam>>,
    /// Specifies from which channel to take transparency information,
    /// for the [`transparent`](Self::transparent) color parameter.
    /// The default is [`TransparencyMode::AOne`].
    pub transparency_mode: TransparencyMode,
    /// Declares the color of perfectly refracted light.
    pub transparent: Option<WithSid<ColorParam>>,
    /// Declares the amount of perfectly refracted light added
    /// to the reflected color as a scalar value between 0.0 and 1.0.
    pub transparency: Option<WithSid<FloatParam>>,
    /// Declares the index of refraction for perfectly refracted light
    /// as a single scalar index.
    pub index_of_refraction: Option<WithSid<FloatParam>>,
}

impl XNode for Phong {
    const NAME: &'static str = "phong";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Phong {
            emission: parse_opt("emission", &mut it, WithSid::parse)?,
            ambient: parse_opt("ambient", &mut it, WithSid::parse)?,
            diffuse: parse_opt("diffuse", &mut it, WithSid::parse)?,
            specular: parse_opt("specular", &mut it, WithSid::parse)?,
            shininess: parse_opt("shininess", &mut it, WithSid::parse)?,
            reflective: parse_opt("reflective", &mut it, WithSid::parse)?,
            reflectivity: parse_opt("reflectivity", &mut it, WithSid::parse)?,
            transparency_mode: parse_transparency_mode(&mut it)?,
            transparent: parse_opt("transparent", &mut it, WithSid::parse)?,
            transparency: parse_opt("transparency", &mut it, WithSid::parse)?,
            index_of_refraction: parse_opt("index_of_refraction", &mut it, WithSid::parse)?,
        })
    }
}

impl XNodeWrite for Phong {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        WithSid::write_opt(&self.emission, "emission", w)?;
        WithSid::write_opt(&self.ambient, "ambient", w)?;
        WithSid::write_opt(&self.diffuse, "diffuse", w)?;
        WithSid::write_opt(&self.specular, "specular", w)?;
        WithSid::write_opt(&self.shininess, "shininess", w)?;
        WithSid::write_opt(&self.reflective, "reflective", w)?;
        WithSid::write_opt(&self.reflectivity, "reflectivity", w)?;
        write_transparent_opt(&self.transparent, self.transparency_mode, w)?;
        WithSid::write_opt(&self.transparency, "transparency", w)?;
        WithSid::write_opt(&self.index_of_refraction, "index_of_refraction", w)?;
        e.end(w)
    }
}

impl Phong {
    /// Run the function `f` on all arguments of type [`Texture`] in the parameters to this shader.
    pub fn on_textures<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
    ) -> Result<(), E> {
        on_color_as_texture(&self.emission, f)?;
        on_color_as_texture(&self.ambient, f)?;
        on_color_as_texture(&self.diffuse, f)?;
        on_color_as_texture(&self.specular, f)?;
        on_color_as_texture(&self.reflective, f)?;
        on_color_as_texture(&self.transparent, f)
    }
}

/// A struct that attaches an optional SID to a shader parameter.
#[derive(Clone, Default, Debug)]
pub struct WithSid<T> {
    sid: Option<String>,
    data: T,
}

impl<T> Deref for WithSid<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: PartialEq> PartialEq for WithSid<T> {
    fn eq(&self, other: &Self) -> bool {
        self.sid == other.sid && self.data == other.data
    }
}

impl<T: Eq> Eq for WithSid<T> {}

pub(crate) use private::CanWithSid;
pub(crate) mod private {
    use super::*;
    pub trait CanWithSid: XNodeWrite + Sized {
        fn parse(element: &Element) -> Result<Option<Self>>;

        fn write_with_sid<W: Write>(&self, sid: &Option<String>, w: &mut XWriter<W>) -> Result<()>;
    }
}

impl<T> From<T> for WithSid<T> {
    fn from(data: T) -> Self {
        Self::new(data)
    }
}

impl<T> WithSid<T> {
    /// Construct a new `WithSid` with no sid.
    pub fn new(data: T) -> Self {
        Self { sid: None, data }
    }

    /// Construct a new `WithSid` with a sid.
    #[allow(clippy::self_named_constructors)]
    pub fn with_sid(sid: impl Into<String>, data: T) -> Self {
        Self {
            sid: Some(sid.into()),
            data,
        }
    }
}

impl<T: CanWithSid> WithSid<T> {
    /// Parse a [`WithSid<T>`] from an XML element.
    pub fn parse(element: &Element) -> Result<Self> {
        let mut it = element.children().peekable();
        parse_one_many(&mut it, |e| {
            Ok(T::parse(e)?.map(|data| Self {
                sid: e.attr("sid").map(Into::into),
                data,
            }))
        })
    }

    fn write_opt(this: &Option<Self>, name: &str, w: &mut XWriter<impl Write>) -> Result<()> {
        opt(this, |this| {
            let elem = ElemBuilder::new(name).start(w)?;
            this.write_to(w)?;
            elem.end(w)
        })
    }
}

impl<T: CanWithSid> XNodeWrite for WithSid<T> {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        self.data.write_with_sid(&self.sid, w)
    }
}

/// A type that describes color attributes of fixed-function shader elements inside
/// [`ProfileCommon`] effects.
#[derive(Clone, Debug, PartialEq)]
pub enum ColorParam {
    /// The value is a literal color, specified by four floating-point numbers in RGBA order.
    Color(Box<[f32; 4]>),
    /// The value is specified by a reference to a previously defined parameter
    /// in the current scope that can be cast directly to a `float4`.
    Param(Box<str>),
    /// The value is specified by a reference to a previously defined `sampler2D` object.
    Texture(Box<Texture>),
}

impl From<[f32; 4]> for ColorParam {
    fn from(rgba: [f32; 4]) -> Self {
        Self::color(rgba)
    }
}

impl From<[f32; 4]> for WithSid<ColorParam> {
    fn from(rgba: [f32; 4]) -> Self {
        WithSid::new(rgba.into())
    }
}

impl From<Texture> for ColorParam {
    fn from(tex: Texture) -> Self {
        Self::Texture(Box::new(tex))
    }
}

impl From<Texture> for WithSid<ColorParam> {
    fn from(tex: Texture) -> Self {
        WithSid::new(tex.into())
    }
}

impl ColorParam {
    /// Construct a new `ColorParam` from a color.
    pub fn color(rgba: [f32; 4]) -> Self {
        Self::Color(Box::new(rgba))
    }
}

impl CanWithSid for ColorParam {
    fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            "color" => Self::Color(parse_array_n(e)?),
            Param::NAME => Self::Param(e.attr("ref").ok_or("expected ref attr")?.into()),
            Texture::NAME => Self::Texture(Texture::parse_box(e)?),
            _ => return Ok(None),
        }))
    }

    fn write_with_sid<W: Write>(&self, sid: &Option<String>, w: &mut XWriter<W>) -> Result<()> {
        match self {
            Self::Color(arr) => {
                let mut e = ElemBuilder::new("color");
                e.opt_attr("sid", sid);
                let e = e.start(w)?;
                print_arr(&**arr, w)?;
                e.end(w)
            }
            Self::Param(ref_) => {
                let mut e = ElemBuilder::new(Param::NAME);
                e.opt_attr("sid", sid);
                e.attr("ref", ref_);
                e.end(w)
            }
            Self::Texture(e) => e.write_to(w),
        }
    }
}

impl XNodeWrite for ColorParam {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        self.write_with_sid(&None, w)
    }
}

impl ColorParam {
    /// Convert this parameter to a texture reference, if it is one.
    pub fn as_texture(&self) -> Option<&Texture> {
        match self {
            ColorParam::Texture(tex) => Some(tex),
            _ => None,
        }
    }

    /// Get the color literal of this parameter, if it is a literal.
    pub fn as_color(&self) -> Option<&[f32; 4]> {
        match self {
            ColorParam::Color(c) => Some(c),
            _ => None,
        }
    }
}

/// A type that describes the scalar attributes of fixed-function shader elements inside
/// [`ProfileCommon`] effects.
#[derive(Clone, Debug, PartialEq)]
pub enum FloatParam {
    /// The value is represented by a literal floating-point scalar.
    Float(f32),
    /// The value is represented by a reference to a previously
    /// defined parameter that can be directly cast to a floating-point scalar.
    Param(Box<str>),
}

impl From<f32> for FloatParam {
    fn from(val: f32) -> Self {
        Self::Float(val)
    }
}

impl From<f32> for WithSid<FloatParam> {
    fn from(val: f32) -> Self {
        WithSid::new(val.into())
    }
}

impl CanWithSid for FloatParam {
    fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(Some(match e.name() {
            "float" => Self::Float(parse_elem(e)?),
            Param::NAME => Self::Param(e.attr("ref").ok_or("expected ref attr")?.into()),
            _ => return Ok(None),
        }))
    }

    fn write_with_sid<W: Write>(&self, sid: &Option<String>, w: &mut XWriter<W>) -> Result<()> {
        match self {
            Self::Float(val) => {
                let mut e = ElemBuilder::new("float");
                e.opt_attr("sid", sid);
                let e = e.start(w)?;
                print_elem(val, w)?;
                e.end(w)
            }
            Self::Param(ref_) => {
                let mut e = ElemBuilder::new(Param::NAME);
                e.opt_attr("sid", sid);
                e.attr("ref", ref_);
                e.end(w)
            }
        }
    }
}

impl XNodeWrite for FloatParam {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        self.write_with_sid(&None, w)
    }
}

/// A color parameter referencing a texture.
#[derive(Clone, Debug, PartialEq)]
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

impl Texture {
    /// Construct a new `Texture` from the mandatory data.
    pub fn new(texture: impl Into<String>, texcoord: impl Into<String>) -> Self {
        Self {
            texture: texture.into(),
            texcoord: texcoord.into(),
            extra: None,
        }
    }

    fn write_with_sid<W: Write>(&self, sid: &Option<String>, w: &mut XWriter<W>) -> Result<()> {
        let mut e = Self::elem();
        e.opt_attr("sid", sid);
        e.attr("texture", &self.texture);
        e.attr("texcoord", &self.texcoord);
        if let Some(extra) = &self.extra {
            let e = e.start(w)?;
            extra.write_to(w)?;
            e.end(w)
        } else {
            e.end(w)
        }
    }
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

impl XNodeWrite for Texture {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        self.write_with_sid(&None, w)
    }
}

fn on_color_as_texture<'a, E>(
    opt: &'a Option<WithSid<ColorParam>>,
    f: &mut impl FnMut(&'a Texture) -> Result<(), E>,
) -> Result<(), E> {
    if let Some(WithSid {
        data: ColorParam::Texture(tex),
        ..
    }) = opt
    {
        f(tex)?
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn parse_phong(xml: &str) -> Phong {
        let mut reader = XReader::from_reader(Cursor::new(xml.as_bytes()));
        Phong::parse(&Element::from_reader(&mut reader).unwrap()).unwrap()
    }

    fn write_phong(phong: &Phong) -> String {
        let mut w = XWriter::new(Vec::new());
        phong.write_to(&mut w).unwrap();
        String::from_utf8(w.into_inner()).unwrap()
    }

    #[test]
    fn transparent_parsing() {
        let phong = parse_phong(
            r#"<phong xmlns="http://www.collada.org/2005/11/COLLADASchema">
              <transparent opaque="RGB_ZERO"><color sid="tc">1 0 0 1</color></transparent>
            </phong>"#,
        );
        assert_eq!(phong.transparency_mode, TransparencyMode::RgbZero);
        assert_eq!(
            phong.transparent,
            Some(WithSid::with_sid(
                "tc",
                ColorParam::Color(Box::new([1.0, 0.0, 0.0, 1.0]))
            ))
        );
        assert_eq!(
            write_phong(&phong),
            r#"<phong><transparent opaque="RGB_ZERO"><color sid="tc">1 0 0 1</color></transparent></phong>"#
        );
    }

    #[test]
    fn transparent_default_mode() {
        // The default is A_ONE, both when `<transparent>` is absent...
        let phong = parse_phong(r#"<phong xmlns="http://www.collada.org/2005/11/COLLADASchema"/>"#);
        assert_eq!(phong.transparency_mode, TransparencyMode::AOne);
        // ... and when it is present without an `opaque` attribute.
        let phong = parse_phong(
            r#"<phong xmlns="http://www.collada.org/2005/11/COLLADASchema">
              <transparent><color>1 0 0 1</color></transparent>
            </phong>"#,
        );
        assert_eq!(phong.transparency_mode, TransparencyMode::AOne);
        // The default is not written back out, as for other defaulted attributes.
        assert_eq!(
            write_phong(&phong),
            r#"<phong><transparent><color>1 0 0 1</color></transparent></phong>"#
        );
    }
}
