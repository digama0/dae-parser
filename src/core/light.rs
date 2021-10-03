use crate::*;

/// Declares a light source that illuminates a scene.
#[derive(Clone, Debug)]
pub struct Light {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The kind of light being described.
    pub kind: LightKind,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
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

/// The kind of light being described.
#[derive(Clone, Debug)]
pub enum LightKind {
    /// Describes an ambient light source.
    Ambient(AmbientLight),
    /// Describes a directional light source.
    Directional(DirectionalLight),
    /// Describes a point light source.
    Point(Box<PointLight>),
    /// Describes a spot light source.
    Spot(Box<SpotLight>),
}

impl LightKind {
    /// Parse a [`LightKind`] from an XML element.
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

/// Describes an ambient light source.
#[derive(Clone, Debug)]
pub struct AmbientLight {
    /// Contains three floating-point numbers specifying the color of the light.
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

/// Describes a directional light source.
///
/// The light’s default direction vector in local coordinates is [0,0,-1],
/// pointing down the negative z axis.
/// The actual direction of the light is defined by the transform of the node
/// where the light is instantiated.
#[derive(Clone, Debug)]
pub struct DirectionalLight {
    /// Contains three floating-point numbers specifying the color of the light.
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

/// Describes a point light source.
///
/// The position of the light is defined by the transform of the node in which it is instantiated.
///
/// The `constant_attenuation`, `linear_attenuation`, and `quadratic_attenuation` are
/// used to calculate the total attenuation of this light given a distance.
/// The equation used is:
/// ```text
/// A = constant_attenuation + Dist * linear_attenuation + Dist^2 * quadratic_attenuation
/// ```
#[derive(Clone, Debug)]
pub struct PointLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
    /// The constant term in the attentuation equation, see [`PointLight`].
    pub constant_attenuation: f32,
    /// The linear term in the attentuation equation, see [`PointLight`].
    pub linear_attenuation: f32,
    /// The quadratic term in the attentuation equation, see [`PointLight`].
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

/// Describes a spot light source.
///
/// The light’s default direction vector in local coordinates is [0,0,-1],
/// pointing down the negative z axis.
/// The actual direction of the light is defined by the transform of the node
/// where the light is instantiated.
///
/// The `constant_attenuation`, `linear_attenuation`, and `quadratic_attenuation` are
/// used to calculate the total attenuation of this light given a distance.
/// The equation used is:
/// ```text
/// A = constant_attenuation + Dist * linear_attenuation + Dist^2 * quadratic_attenuation
/// ```
///
/// The `falloff_angle` and `falloff_exponent` are used to specify
/// the amount of attenuation based on the direction of the light.
#[derive(Clone, Debug)]
pub struct SpotLight {
    /// Contains three floating-point numbers specifying the color of the light.
    pub color: Box<[f32; 3]>,
    /// The constant term in the attentuation equation, see [`SpotLight`].
    pub constant_attenuation: f32,
    /// The linear term in the attentuation equation, see [`SpotLight`].
    pub linear_attenuation: f32,
    /// The quadratic term in the attentuation equation, see [`SpotLight`].
    pub quadratic_attenuation: f32,
    /// The directional attenuation of the light.
    pub falloff_angle: f32,
    /// A term in the directional attenuation equation of the light.
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