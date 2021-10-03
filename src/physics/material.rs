use crate::*;

/// Defines the physical properties of an object.
///
/// It contains a technique/profile with parameters.
/// The `COMMON` profile defines the built-in names, such as `static_friction`.
#[derive(Clone, Debug)]
pub struct PhysicsMaterial {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Specifies physics-material information for the common
    /// profile that all COLLADA implementations must support.
    pub common: PhysicsMaterialCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for PhysicsMaterial {
    const NAME: &'static str = "physics_material";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(PhysicsMaterial {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            common: parse_one(Technique::COMMON, &mut it, PhysicsMaterialCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies physics-material information for the common
/// profile that all COLLADA implementations must support.
#[derive(Clone, Debug)]
pub struct PhysicsMaterialCommon {
    /// Contains a floating-point number that specifies the dynamic friction coefficient.
    pub dynamic_friction: f32,
    /// Contains a floating-point number that is the proportion
    /// of the kinetic energy preserved in the impact (typically ranges from 0.0 to 1.0).
    /// Also known as "bounciness" or "elasticity."
    pub restitution: f32,
    /// Contains a floating-point number that specifies the static friction coefficient.
    pub static_friction: f32,
}

impl PhysicsMaterialCommon {
    fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = PhysicsMaterialCommon {
            dynamic_friction: parse_opt("dynamic_friction", &mut it, parse_elem)?.unwrap_or(0.),
            restitution: parse_opt("restitution", &mut it, parse_elem)?.unwrap_or(0.),
            static_friction: parse_opt("static_friction", &mut it, parse_elem)?.unwrap_or(0.),
        };
        finish(res, it)
    }
}
