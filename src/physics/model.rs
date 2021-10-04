use crate::*;

/// Allows for building complex combinations of rigid bodies and constraints
/// that may be instantiated multiple times.
#[derive(Clone, Debug)]
pub struct PhysicsModel {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Defines a [`RigidBody`] element and sets its nondefault properties.
    pub rigid_body: Vec<RigidBody>,
    /// Defines a [`RigidConstraint`] element and allows for overriding
    /// some or all of its properties.
    pub rigid_constraint: Vec<RigidConstraint>,
    /// Instantiates a physics model from the given url,
    /// and assigns an sid to it, to distinguish it from other child elements.
    pub instances: Vec<Instance<PhysicsModel>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl HasId for PhysicsModel {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
}

impl XNode for PhysicsModel {
    const NAME: &'static str = "physics_model";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(PhysicsModel {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            rigid_body: RigidBody::parse_list(&mut it)?,
            rigid_constraint: RigidConstraint::parse_list(&mut it)?,
            instances: Instance::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Extra data associated to [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstancePhysicsModelData {
    /// Points to the id of a node in the visual scene. This allows a physics model to be
    /// instantiated under a specific transform node, which will dictate the initial position
    /// and orientation, and could be animated to influence kinematic rigid bodies.
    pub parent: Option<UrlRef<Node>>,
    /// Instantiates a [`ForceField`] element to influence this physics model.
    pub instance_force_field: Vec<Instance<ForceField>>,
    /// Instantiates a [`RigidBody`] element and allows for overriding some or all of its
    /// properties.
    ///
    /// The target attribute defines the [`Node`] element that has its transforms overwritten
    /// by this rigid-body instance.
    pub instance_rigid_body: Vec<InstanceRigidBody>,
    /// Instantiates a [`RigidConstraint`] element to override some of its properties.
    /// This element does not have a `target` field because its [`RigidConstraint`]
    /// children define which [`Node`] elements are targeted.
    pub instance_rigid_constraint: Vec<InstanceRigidConstraint>,
}

impl Instantiate for PhysicsModel {
    const INSTANCE: &'static str = "instance_physics_model";
    type Data = InstancePhysicsModelData;
    fn parse_data(e: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstancePhysicsModelData {
            parent: parse_attr(e.attr("parent"))?,
            instance_force_field: Instance::parse_list(it)?,
            instance_rigid_body: InstanceRigidBody::parse_list(it)?,
            instance_rigid_constraint: InstanceRigidConstraint::parse_list(it)?,
        })
    }
}

/// Describes simulated bodies that do not deform.
///
/// These bodies may or may not be connected by constraints (hinge, ball-joint, and so on).
#[derive(Clone, Debug)]
pub struct RigidBody {
    /// A text string containing the scoped identifier of the [`RigidBody`] element.
    /// This value must be unique among its sibling elements.
    /// Associates each rigid body with a visual [`Node`] when a [`PhysicsModel`]
    /// is instantiated.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Specifies rigid-body information for the common profile
    /// that every COLLADA implmentation must support.
    pub common: RigidBodyCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl Deref for RigidBody {
    type Target = RigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl XNode for RigidBody {
    const NAME: &'static str = "rigid_body";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(RigidBody {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            common: parse_one(Technique::COMMON, &mut it, |e| {
                RigidBodyCommon::parse(e.children().peekable())
            })?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies rigid-body information for the common profile
/// that every COLLADA implmentation must support.
#[derive(Clone, Default, Debug)]
pub struct RigidBodyCommon {
    /// A Boolean that specifies whether the [`RigidBody`] is movable, if present.
    pub dynamic: Option<bool>,
    /// Contains a floating-point value that specifies the total mass of the [`RigidBody`].
    pub mass: Option<f32>,
    /// Defines the center and orientation of mass of the [`RigidBody`] relative
    /// to the local origin of the "root" shape.
    /// This makes the off-diagonal elements of the inertia tensor
    /// (products of inertia) all 0 and allows us to just store the diagonal elements
    /// (moments of inertia).
    pub mass_frame: Vec<RigidTransform>,
    /// Contains three floating-point numbers, which are the diagonal elements of the
    /// inertia tensor (moments of inertia), which is represented in the local frame
    /// of the center of mass. See `mass_frame`.
    pub inertia: Option<Box<[f32; 3]>>,
    /// Defines or references a [`PhysicsMaterial`] for the [`RigidBody`].
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
    /// The components of the [`RigidBody`].
    pub shape: Vec<Shape>,
}

impl RigidBodyCommon {
    fn parse(mut it: ElementIter<'_>) -> Result<Self> {
        let res = Self {
            dynamic: parse_opt("dynamic", &mut it, parse_elem)?,
            mass: parse_opt("mass", &mut it, parse_elem)?,
            mass_frame: parse_opt("mass_frame", &mut it, |e| {
                let mut it = e.children().peekable();
                finish(parse_list_many(&mut it, RigidTransform::parse)?, it)
            })?
            .unwrap_or_default(),
            inertia: parse_opt("inertia", &mut it, parse_array_n)?,
            physics_material: parse_opt_many(&mut it, DefInstance::parse)?.map(Box::new),
            shape: Shape::parse_list(&mut it)?,
        };
        finish(res, it)
    }
}

/// Instantiates an object described by a [`RigidBody`] within an [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstanceRigidBody {
    /// Which [`RigidBody`] to instantiate.
    pub body: NameRef<RigidBody>,
    /// Which [`Node`] is influenced by this [`RigidBody`] instance.
    /// Can refer to a local instance or external reference.
    pub target: UrlRef<Node>,
    /// Specifies the rigid-body information for the common
    /// profile that all COLLADA implementations must support.
    pub common: InstanceRigidBodyCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl Deref for InstanceRigidBody {
    type Target = InstanceRigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl XNode for InstanceRigidBody {
    const NAME: &'static str = "rigid_body";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(InstanceRigidBody {
            body: Ref::new(element.attr("body").ok_or("missing body attribute")?.into()),
            target: parse_attr(element.attr("target"))?.ok_or("missing url attribute")?,
            common: parse_one(Technique::COMMON, &mut it, InstanceRigidBodyCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies the rigid-body information for the common
/// profile that all COLLADA implementations must support.
#[derive(Clone, Default, Debug)]
pub struct InstanceRigidBodyCommon {
    /// Contains three floating-point values that
    /// specify the initial angular velocity of the
    /// rigid_body instance around each axis, in
    /// the form of an x-y-z Euler rotation. The
    /// measurement is in degrees per second.
    pub angular_velocity: [f32; 3],
    /// Contains three floating-point values that specify
    /// the initial linear velocity of the [`RigidBody`] instance.
    pub velocity: [f32; 3],
    /// Additional fields are inherited from [`RigidBodyCommon`].
    pub common: RigidBodyCommon,
}

impl Deref for InstanceRigidBodyCommon {
    type Target = RigidBodyCommon;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl InstanceRigidBodyCommon {
    /// Parse a [`InstanceRigidBodyCommon`] from a
    /// `<instance_rigid_body>/<technique_common>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        Ok(Self {
            angular_velocity: parse_opt("angular_velocity", &mut it, parse_array_n)?
                .map_or([0.; 3], |a| *a),
            velocity: parse_opt("velocity", &mut it, parse_array_n)?.map_or([0.; 3], |a| *a),
            common: RigidBodyCommon::parse(it)?,
        })
    }
}

/// Connects components, such as [`RigidBody`], into complex physics models with moveable parts.
#[derive(Clone, Debug)]
pub struct RigidConstraint {
    /// A text string containing the scoped identifier of the [`RigidConstraint`]
    /// element. This value must be unique within the scope of the parent element.
    pub sid: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Defines the attachment frame of reference (to a [`RigidBody`] or a [`Node`])
    /// within a rigid constraint.
    pub ref_attachment: Attachment,
    /// Defines an attachment frame (to a [`RigidBody`] or a [`Node`])
    /// within a rigid constraint.
    pub attachment: Attachment,
    /// Specifies rigid-constraint information for the common profile that all COLLADA
    /// implementations must support.
    pub common: RigidConstraintCommon,
    /// Declares the information used to process some portion of the content. (optional)
    pub technique: Vec<Technique>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for RigidConstraint {
    const NAME: &'static str = "rigid_constraint";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(RigidConstraint {
            sid: element.attr("sid").map(Into::into),
            name: element.attr("name").map(Into::into),
            ref_attachment: parse_one(Attachment::REF, &mut it, Attachment::parse)?,
            attachment: Attachment::parse_one(&mut it)?,
            common: parse_one(Technique::COMMON, &mut it, RigidConstraintCommon::parse)?,
            technique: Technique::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Specifies rigid-constraint information for the common profile that all COLLADA
/// implementations must support.
#[derive(Clone, Default, Debug)]
pub struct RigidConstraintCommon {
    /// If false, the [`RigidConstraint`] doesn’t exert any force or influence on
    /// the rigid bodies.
    pub enabled: bool,
    /// If true, the attached rigid bodies may interpenetrate.
    pub interpenetrate: bool,
    /// Describes the angular limits along each rotation axis in degrees.
    pub swing_cone_and_twist: Limits,
    /// Describes linear (translational) limits along each axis.
    pub linear: Limits,
    /// Describes angular spring constraints.
    pub spring_angular: Spring,
    /// Describes linear spring constraints.
    pub spring_linear: Spring,
}

impl RigidConstraintCommon {
    /// Parse a [`RigidConstraintCommon`] from a
    /// `<rigid_constraint>/<technique_common>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let enabled = parse_opt("enabled", &mut it, parse_elem)?.unwrap_or(true);
        let interpenetrate = parse_opt("interpenetrate", &mut it, parse_elem)?.unwrap_or(false);
        let (swing_cone_and_twist, linear) = parse_opt("limits", &mut it, |e| {
            let mut it = e.children().peekable();
            let scat =
                parse_opt("swing_cone_and_twist", &mut it, Limits::parse)?.unwrap_or_default();
            let lin = parse_opt("linear", &mut it, Limits::parse)?.unwrap_or_default();
            finish((scat, lin), it)
        })?
        .unwrap_or_default();
        let (spring_angular, spring_linear) = parse_opt("spring", &mut it, |e| {
            let mut it = e.children().peekable();
            let ang = parse_opt("angular", &mut it, Spring::parse)?.unwrap_or_default();
            let lin = parse_opt("linear", &mut it, Spring::parse)?.unwrap_or_default();
            finish((ang, lin), it)
        })?
        .unwrap_or_default();
        let res = Self {
            enabled,
            interpenetrate,
            swing_cone_and_twist,
            linear,
            spring_angular,
            spring_linear,
        };
        finish(res, it)
    }
}

/// Instantiates an object described by a [`RigidConstraint`]
/// within an [`Instance`]<[`PhysicsModel`]>.
#[derive(Clone, Debug)]
pub struct InstanceRigidConstraint {
    /// Which [`RigidConstraint`] to instantiate.
    pub constraint: NameRef<RigidConstraint>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for InstanceRigidConstraint {
    const NAME: &'static str = "instance_rigid_constraint";
    fn parse(e: &Element) -> Result<Self> {
        debug_assert_eq!(e.name(), Self::NAME);
        let constraint = e.attr("constraint").ok_or("missing constraint attr")?;
        Ok(InstanceRigidConstraint {
            constraint: Ref::new(constraint.into()),
            extra: Extra::parse_many(e.children())?,
        })
    }
}

/// A set of min/max limits used for both linear and angular constraints.
#[derive(Clone, Default, Debug)]
pub struct Limits {
    /// The minimum value in each direction.
    pub min: Option<Box<[f32; 3]>>,
    /// The maximum value in each direction.
    pub max: Option<Box<[f32; 3]>>,
}

impl Limits {
    /// Parse a [`Limits`] from a `<swing_cone_and_twist>` or `<linear>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            min: parse_opt("min", &mut it, parse_array_n)?,
            max: parse_opt("max", &mut it, parse_array_n)?,
        };
        finish(res, it)
    }
}

/// A spring constraint, used for both linear and angular constraints.
#[derive(Clone, Copy, Debug)]
pub struct Spring {
    /// The `stiffness` (also called spring coefficient)
    /// has units of force/distance for `spring_linear`
    /// or force/angle in degrees for `spring_angular`.
    pub stiffness: f32,
    /// The damping coefficient of the spring.
    pub damping: f32,
    /// The resting position of the spring.
    pub target_value: f32,
}

impl Default for Spring {
    fn default() -> Self {
        Self {
            stiffness: 1.,
            damping: 0.,
            target_value: 0.,
        }
    }
}

impl Spring {
    /// Parse a [`Spring`] from a `<linear>` or `<angular>` XML element.
    pub fn parse(e: &Element) -> Result<Self> {
        let mut it = e.children().peekable();
        let res = Self {
            stiffness: parse_opt("stiffness", &mut it, parse_elem)?.unwrap_or(1.),
            damping: parse_opt("damping", &mut it, parse_elem)?.unwrap_or(0.),
            target_value: parse_opt("target_value", &mut it, parse_elem)?.unwrap_or(0.),
        };
        finish(res, it)
    }
}
