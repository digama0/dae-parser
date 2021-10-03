use crate::*;

/// Describes one effect pass to evaluate a scene.
#[derive(Clone, Debug)]
pub struct Render {
    /// Refers to a node that contains a camera describing the viewpoint
    /// from which to render this compositing step.
    pub camera_node: Url,
    /// Specifies which layer or layers to render in this compositing step
    /// while evaluating the scene.
    pub layers: Vec<String>,
    /// Instantiates a COLLADA material resource. See [`InstanceEffectData`]
    /// for the additional instance effect data.
    pub instance_effect: Option<Instance<Effect>>,
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

/// A type that describes color attributes of fixed-function shader elements inside
/// [`ProfileCommon`] effects.
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
/// [`ProfileCommon`] effects.
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
