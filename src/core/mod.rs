mod anim;
mod camera;
mod control;
mod data;
mod ext;
mod geom;
mod light;
mod meta;
mod scene;
mod transform;

use crate::*;
pub use {
    anim::*, camera::*, control::*, data::*, ext::*, geom::*, light::*, meta::*, scene::*,
    transform::*,
};

/// An unparsed COLLADA target address.
/// See the "Address Syntax" section in Chapter 3: Schema concepts of the
/// [COLLADA spec](https://www.khronos.org/files/collada_spec_1_4.pdf).
#[derive(Clone, Debug)]
pub struct Address(pub String);

/// A trait for nodes that can be placed in a library element.
pub trait ParseLibrary: XNode {
    /// The name of the library element. For example, the [`Geometry`] element has
    /// `LIBRARY = "library_geometries"`,
    /// and the corresponding library type is [`Library`]`<Geometry>`.
    const LIBRARY: &'static str;

    /// Extract the library from a single [`LibraryElement`].
    fn extract_element(e: &LibraryElement) -> Option<&Library<Self>>;
}

/// Declares a module of elements of type `T`.
#[derive(Clone, Debug)]
pub struct Library<T> {
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The individual items in the module.
    pub items: Vec<T>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl<T: ParseLibrary> XNode for Library<T> {
    const NAME: &'static str = T::LIBRARY;
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Library {
            asset: Asset::parse_opt_box(&mut it)?,
            items: T::parse_list(&mut it)?, // should be 1 or more but blender disagrees
            extra: Extra::parse_many(it)?,
        })
    }
}

macro_rules! mk_libraries {
    ($($(#[$doc:meta])* $name:ident($arg:ident) = $s:literal,)*) => {
        $(impl ParseLibrary for $arg {
            const LIBRARY: &'static str = $s;

            fn extract_element(e: &LibraryElement) -> Option<&Library<Self>> {
                if let LibraryElement::$name(arg) = e {
                    Some(arg)
                } else {
                    None
                }
            }
        })*

        /// A library element, which can be a module of any of the kinds supported by COLLADA.
        #[derive(Clone, Debug)]
        pub enum LibraryElement {
            $(#[doc = concat!("Declares a module of [`", stringify!($arg), "`] elements.")]
                $name(Library<$arg>),)*
        }

        impl LibraryElement {
            /// Parse a [`LibraryElement`] from an XML element.
            pub fn parse(e: &Element) -> Result<Option<Self>> {
                Ok(Some(match e.name() {
                    $($arg::LIBRARY => Self::$name(Library::parse(e)?),)*
                    _ => return Ok(None),
                }))
            }
        }
    }
}

mk_libraries! {
    Animations(Animation) = "library_animations",
    AnimationClips(AnimationClip) = "library_animation_clips",
    Cameras(Camera) = "library_cameras",
    Controllers(Controller) = "library_controllers",
    Effects(Effect) = "library_effects",
    ForceFields(ForceField) = "library_force_fields",
    Geometries(Geometry) = "library_geometries",
    Images(Image) = "library_images",
    Lights(Light) = "library_lights",
    Materials(Material) = "library_materials",
    Nodes(Node) = "library_nodes",
    PhysicsMaterials(PhysicsMaterial) = "library_physics_materials",
    PhysicsModels(PhysicsModel) = "library_physics_models",
    PhysicsScenes(PhysicsScene) = "library_physics_scenes",
    VisualScenes(VisualScene) = "library_visual_scenes",
}

/// Instantiates a COLLADA material resource,
/// possibly applying transformations or effects to the object.
///
/// The `data` field depends on the type of object being instantiated.
/// Most types use `()` for this field but some types have additional data:
/// * `Instance<`[`Geometry`]>: [`InstanceGeometryData`]
/// * `Instance<`[`Controller`]>: [`InstanceControllerData`]
/// * `Instance<`[`Effect`]>: [`InstanceEffectData`]
/// * `Instance<`[`PhysicsModel`]>: [`InstancePhysicsModelData`]
///
/// Additionally, some instance nodes are even more different and have their own types:
/// * [`InstanceMaterial`], not `Instance<`[`Material`]`>`
/// * [`InstanceRigidBody`], not `Instance<`[`RigidBody`]`>`
/// * [`InstanceRigidConstraint`], not `Instance<`[`RigidConstraint`]`>`
#[derive(Clone, Debug)]
pub struct Instance<T: Instantiate> {
    /// A text string containing the scoped identifier of this element.
    /// This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The URL of the location of the `T` element to instantiate.
    /// Can refer to a local instance or external reference.
    pub url: Url,
    /// The additional data associated with the instantiation, if any.
    pub data: T::Data,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

/// The trait for types that can be used in [`Instance<T>`].
pub trait Instantiate {
    /// The name of the instance node.
    /// For example `Geometry::INSTANCE = "instance_geometry"`.
    const INSTANCE: &'static str;

    /// The type of additional data associated with instantiations, possibly `()`.
    type Data;

    /// Parse the [`Self::Data`] given an element iterator,
    /// and a reference to the parent element.
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data>;
}

impl<T: Instantiate> XNode for Instance<T> {
    const NAME: &'static str = T::INSTANCE;
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Instance {
            sid: element.attr("sid").map(Into::into),
            url: parse_attr(element.attr("url"))?.ok_or("missing url attribute")?,
            data: T::parse_data(element, &mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Either an instance of a `T`, or a directly inlined `T` object.
pub enum DefInstance<T: Instantiate> {
    /// A definition of a `T`.
    Def(T),
    /// An instantiation of a `T`.
    Ref(Instance<T>),
}

impl<T: Instantiate + Debug> Debug for DefInstance<T>
where
    T::Data: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Def(t) => f.debug_tuple("Def").field(t).finish(),
            Self::Ref(t) => f.debug_tuple("Ref").field(t).finish(),
        }
    }
}

impl<T: Instantiate + Clone> Clone for DefInstance<T>
where
    T::Data: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Def(t) => Self::Def(t.clone()),
            Self::Ref(t) => Self::Ref(t.clone()),
        }
    }
}

impl<T: Instantiate + XNode> DefInstance<T> {
    pub(crate) fn parse(e: &Element) -> Result<Option<Self>> {
        Ok(if e.name() == T::NAME {
            Some(Self::Def(T::parse(e)?))
        } else if e.name() == T::INSTANCE {
            Some(Self::Ref(Instance::parse(e)?))
        } else {
            None
        })
    }
}

macro_rules! basic_instance {
    ($($ty:ty => $val:expr;)*) => {
        $(impl Instantiate for $ty {
            const INSTANCE: &'static str = $val;
            type Data = ();
            fn parse_data(_: &Element, _: &mut ElementIter<'_>) -> Result<Self::Data> {
                Ok(())
            }
        })*
    }
}
basic_instance! {
    Animation => "instance_animation";
    Camera => "instance_camera";
    ForceField => "instance_force_field";
    Light => "instance_light";
    Node => "instance_node";
    PhysicsMaterial => "instance_physics_material";
    PhysicsScene => "instance_physics_scene";
    VisualScene => "instance_visual_scene";
}
