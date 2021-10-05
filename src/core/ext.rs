use crate::*;

/// Provides arbitrary additional information about or related to its parent element.
#[derive(Clone, Default, Debug)]
pub struct Extra {
    /// The unique identifier of the `Extra` element.
    /// This value must be unique within the document.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// A hint as to the type of information that the particular `Extra` element represents.
    /// This text string must be understood by the application.
    pub ty: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Declares the information used to process some portion of the content.
    /// This field is always nonempty, because the spec provides no common data
    /// for `Extra` elements.
    pub technique: Vec<Technique>,
}

impl XNode for Extra {
    const NAME: &'static str = "extra";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Extra {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            ty: element.attr("type").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            technique: Technique::parse_list_n::<1>(&mut it)?,
        };
        finish(res, it)
    }
}

impl Extra {
    pub(crate) fn parse_many<'a>(it: impl Iterator<Item = &'a Element>) -> Result<Vec<Extra>> {
        let mut extras = vec![];
        for e in it {
            match e.name() {
                "extra" => extras.push(Extra::parse(e)?),
                k => return Err(format!("unexpected element {}", k).into()),
            }
        }
        Ok(extras)
    }
}

/// Declares the information used to process some portion of the content.
/// Each technique conforms to an associated profile.
/// In the COLLADA spec, this element is called "`<technique>` (core)".
#[derive(Clone, Debug)]
pub struct Technique {
    /// The `<technique>` element can contain any well-formed XML data.
    pub element: Element,
}

impl XNode for Technique {
    const NAME: &'static str = "technique";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        element.attr("profile").ok_or("expected 'profile' attr")?;
        Ok(Technique {
            element: element.clone(),
        })
    }
}
impl Technique {
    /// The name of the `<technique_common>` element.
    pub const COMMON: &'static str = "technique_common";
}
