use crate::*;

/// Defines asset-management information regarding its parent element.
#[derive(Clone, Debug)]
pub struct Asset {
    /// Defines data related to a contributor that worked on the parent element.
    pub contributor: Vec<Contributor>,
    /// Contains date and time that the parent element was created.
    /// Represented in an ISO 8601 format as per the XML Schema
    /// `dateTime` primitive type.
    pub created: String,
    /// Contains a list of words used as search criteria for the parent element.
    pub keywords: Vec<String>,
    /// Contains date and time that the parent element was last
    /// modified. Represented in an ISO 8601 format as per the
    /// XML Schema `dateTime` primitive type.
    pub modified: String,
    /// Contains revision information for the parent element.
    pub revision: Option<String>,
    /// Contains a description of the topical subject of the parent element.
    pub subject: Option<String>,
    /// Contains title information for the parent element.
    pub title: Option<String>,
    /// Defines unit of distance for COLLADA elements and objects.
    pub unit: Option<Unit>,
    /// Contains descriptive information about the coordinate system of the geometric data.
    pub up_axis: UpAxis,
}

impl XNode for Asset {
    const NAME: &'static str = "asset";

    fn parse_box(element: &Element) -> Result<Box<Self>> {
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

    fn parse(element: &Element) -> Result<Self> {
        Ok(*Self::parse_box(element)?)
    }
}

/// Defines authoring information for asset management.
#[derive(Clone, Default, Debug)]
pub struct Contributor {
    /// The author’s name
    pub author: Option<String>,
    /// The name of the authoring tool
    pub authoring_tool: Option<String>,
    /// Comments from this contributor
    pub comments: Option<String>,
    /// Copyright information
    pub copyright: Option<String>,
    /// A reference to the source data used for this asset
    pub source_data: Option<Url>,
}

impl XNode for Contributor {
    const NAME: &'static str = "contributor";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Contributor {
            author: parse_opt("author", &mut it, parse_text)?,
            authoring_tool: parse_opt("authoring_tool", &mut it, parse_text)?,
            comments: parse_opt("comments", &mut it, parse_text)?,
            copyright: parse_opt("copyright", &mut it, parse_text)?,
            source_data: parse_opt("source_data", &mut it, parse_elem)?,
        };
        finish(res, it)
    }
}

/// Defines unit of distance for COLLADA elements and objects.
/// This unit of distance applies to all spatial measurements
/// within the scope of `Asset`’s parent element, unless
/// overridden by a more local `Asset` / `Unit`.
///
/// The value of the unit is self-describing and does not have to be consistent
/// with any real-world measurement.
#[derive(Clone, Debug)]
pub struct Unit {
    /// The name of the distance unit. For example, "meter", "centimeter", "inches",
    /// or "parsec". This can be the real name of a measurement, or an imaginary name.
    pub name: Option<String>,
    /// How many real-world meters in one
    /// distance unit as a floating-point number. For
    /// example, 1.0 for the name "meter"; 1000 for the
    /// name "kilometer"; 0.3048 for the name "foot".
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
    fn parse(element: &Element) -> Result<Self> {
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

/// Descriptive information about the coordinate system of the geometric data.
/// All coordinates are right-handed by definition.
#[derive(Clone, Debug)]
pub enum UpAxis {
    /// Right: `-y`, Up: `+x`, In: `+z`
    XUp,
    /// Right: `+x`, Up: `+y`, In: `+z`
    YUp,
    /// Right: `+x`, Up: `+z`, In: `-y`
    ZUp,
}

impl Default for UpAxis {
    fn default() -> Self {
        Self::YUp
    }
}

impl XNode for UpAxis {
    const NAME: &'static str = "up_axis";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(match get_text(element) {
            Some("X_UP") => UpAxis::XUp,
            Some("Y_UP") => UpAxis::YUp,
            Some("Z_UP") => UpAxis::ZUp,
            _ => return Err("invalid <up_axis> value".into()),
        })
    }
}
