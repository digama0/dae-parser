use crate::*;

/// Categorizes the declaration of animation information.
#[derive(Clone, Debug)]
pub struct Animation {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Allows the formation of a hierarchy of related animations.
    pub children: Vec<Animation>,
    /// The data repository that provides values according to the semantics of an
    /// [`Input`] element that refers to it.
    pub source: Vec<Source>,
    /// Describes the interpolation sampling function for the animation.
    pub sampler: Vec<Sampler>,
    /// Describes an output channel for the animation.
    pub channel: Vec<Channel>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl HasId for Animation {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
}

impl XNode for Animation {
    const NAME: &'static str = "animation";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Animation {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            children: Animation::parse_list(&mut it)?,
            source: Source::parse_list(&mut it)?,
            sampler: Sampler::parse_list(&mut it)?,
            channel: Channel::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        if res.children.is_empty() && res.sampler.is_empty() {
            return Err("animation: no sampler/channel or children".into());
        }
        if res.sampler.is_empty() != res.channel.is_empty() {
            return Err("animation: sampler and channel must be used together".into());
        }
        Ok(res)
    }
}

/// Defines a section of a set of animation curves to be used together as an animation clip.
#[derive(Clone, Debug)]
pub struct AnimationClip {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The time in seconds of the beginning of the clip. This time is the same as that used in the
    /// key-frame data and is used to determine which set of key frames will be included in the
    /// clip. The start time does not specify when the clip will be played. If the time falls between
    /// two key frames of a referenced animation, an interpolated value should be used.
    pub start: f32,
    /// The time in seconds of the end of the clip. This is used in the same way as the start time.
    /// If `end` is not specified, the value is taken to be the end time of the longest animation.
    pub end: Option<f32>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Instantiates an [`Animation`] object.
    pub instance_animation: Vec<Instance<Animation>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl HasId for AnimationClip {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
}

impl XNode for AnimationClip {
    const NAME: &'static str = "animation_clip";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(AnimationClip {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            start: parse_attr(element.attr("start"))?.unwrap_or(0.),
            end: parse_attr(element.attr("end"))?,
            asset: Asset::parse_opt_box(&mut it)?,
            instance_animation: Instance::parse_list_n::<1>(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Declares an output channel of an animation.
#[derive(Clone, Debug)]
pub struct Channel {
    /// The location of the animation sampler using a URL expression.
    pub source: Url,
    /// The location of the element bound to the output of the sampler.
    pub target: Address,
}

impl XNode for Channel {
    const NAME: &'static str = "channel";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let target = element.attr("target").ok_or("expecting target attr")?;
        Ok(Channel {
            source: parse_attr(element.attr("source"))?.ok_or("missing source attr")?,
            target: Address(target.into()),
        })
    }
}

/// Declares an interpolation sampling function for an animation.
#[derive(Clone, Debug)]
pub struct Sampler {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// Assigns semantics to each [`Source`]. See the COLLADA spec for details.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Interpolation`] input (which must exist).
    pub interpolation: usize,
}

impl HasId for Sampler {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
}

impl XNode for Sampler {
    const NAME: &'static str = "sampler";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        let res = Sampler {
            id: element.attr("id").map(Into::into),
            interpolation: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Interpolation)
                .ok_or("sampler: missing INTERPOLATION input")?,
            inputs,
        };
        finish(res, it)
    }
}

impl Sampler {
    /// The input with [`Semantic::Interpolation`].
    pub fn interpolation_input(&self) -> &Input {
        &self.inputs[self.interpolation]
    }
}
