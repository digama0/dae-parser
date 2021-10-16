use crate::*;

/// Describes components of a [`RigidBody`].
#[derive(Clone, Debug)]
pub struct Shape {
    /// If true, the mass is distributed along the surface of the shape.
    pub hollow: Option<bool>,
    /// The mass of the shape.
    /// If not provided, it is derived from density x shape volume.
    pub mass: Option<f32>,
    /// The density of the shape.
    /// If not provided, it is derived from mass/shape volume.
    pub density: Option<f32>,
    /// The [`PhysicsMaterial`] used for this shape.
    pub physics_material: Option<Box<DefInstance<PhysicsMaterial>>>,
    /// The geometry of the shape ([`Plane`], [`Sphere`], [`Mesh`], etc.).
    pub geom: ShapeGeom,
    /// Transformation for the shape. Any combination of these elements in any order.
    /// See [`Node`] for additional information.
    pub transform: Vec<RigidTransform>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Shape {
    const NAME: &'static str = "shape";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Shape {
            hollow: parse_opt("hollow", &mut it, parse_elem)?,
            mass: parse_opt("mass", &mut it, parse_elem)?,
            density: parse_opt("density", &mut it, parse_elem)?,
            physics_material: parse_opt_many(&mut it, DefInstance::parse)?.map(Box::new),
            transform: parse_list_many(&mut it, RigidTransform::parse)?,
            geom: parse_one_many(&mut it, ShapeGeom::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for Shape {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::opt_print("hollow", &self.hollow, w)?;
        ElemBuilder::opt_print("mass", &self.mass, w)?;
        ElemBuilder::opt_print("density", &self.density, w)?;
        self.physics_material.write_to(w)?;
        self.geom.write_to(w)?;
        self.transform.write_to(w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

impl CollectLocalMaps for Shape {
    fn collect_local_maps<'a>(&'a self, maps: &mut LocalMaps<'a>) {
        self.physics_material.collect_local_maps(maps);
    }
}

/// The geometry of a shape. This can be either an inline definition
/// using [`Plane`], [`Sphere`] etc, or a [`Instance<Geometry>`],
/// which can reference other geometry types.
#[derive(Clone, Debug)]
pub enum ShapeGeom {
    /// Defines an infinite plane primitive.
    Plane(Plane),
    /// Declares an axis-aligned, centered box primitive.
    Box(BoxShape),
    /// Describes a centered sphere primitive.
    Sphere(Sphere),
    /// Declares a cylinder primitive that is centered on, and aligned with, the local y axis.
    Cylinder(Cylinder),
    /// Describes a tapered cylinder primitive that is centered on and aligned with the local y axis.
    TaperedCylinder(TaperedCylinder),
    /// Declares a capsule primitive that is centered on and aligned with the local y axis.
    Capsule(Capsule),
    /// Describes a tapered capsule primitive that is centered on, and aligned with, the local y axis.
    TaperedCapsule(TaperedCapsule),
    /// A geometry instance using the [`Instance<Geometry>`] element, which
    /// references any [`GeometryElement`] ([`Mesh`] or [`Spline`]).
    Geom(Instance<Geometry>),
}

impl ShapeGeom {
    /// Parse a [`ShapeGeom`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Plane::NAME => Ok(Some(Self::Plane(Plane::parse(e)?))),
            BoxShape::NAME => Ok(Some(Self::Box(BoxShape::parse(e)?))),
            Sphere::NAME => Ok(Some(Self::Sphere(Sphere::parse(e)?))),
            Cylinder::NAME => Ok(Some(Self::Cylinder(Cylinder::parse(e)?))),
            TaperedCylinder::NAME => Ok(Some(Self::TaperedCylinder(TaperedCylinder::parse(e)?))),
            Capsule::NAME => Ok(Some(Self::Capsule(Capsule::parse(e)?))),
            TaperedCapsule::NAME => Ok(Some(Self::TaperedCapsule(TaperedCapsule::parse(e)?))),
            Geometry::INSTANCE => Ok(Some(Self::Geom(Instance::parse(e)?))),
            _ => Ok(None),
        }
    }
}

impl XNodeWrite for ShapeGeom {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        match self {
            Self::Plane(e) => e.write_to(w),
            Self::Box(e) => e.write_to(w),
            Self::Sphere(e) => e.write_to(w),
            Self::Cylinder(e) => e.write_to(w),
            Self::TaperedCylinder(e) => e.write_to(w),
            Self::Capsule(e) => e.write_to(w),
            Self::TaperedCapsule(e) => e.write_to(w),
            Self::Geom(e) => e.write_to(w),
        }
    }
}

/// Defines an infinite plane primitive.
#[derive(Clone, Debug)]
pub struct Plane {
    /// The coefficients for the plane’s equation: `Ax + By + Cz + D = 0`.
    pub equation: [f32; 4],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Plane {
    const NAME: &'static str = "plane";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Plane {
            equation: *parse_one("equation", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for Plane {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print_arr("equation", &self.equation, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Declares an axis-aligned, centered box primitive.
/// (Note: The type is not called `Box` to avoid the name clash with the Rust builtin type.)
#[derive(Clone, Debug)]
pub struct BoxShape {
    /// The extents of the box.
    pub half_extents: [f32; 3],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for BoxShape {
    const NAME: &'static str = "box";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(BoxShape {
            half_extents: *parse_one("half_extents", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for BoxShape {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print_arr("half_extents", &self.half_extents, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Describes a centered sphere primitive.
#[derive(Clone, Debug)]
pub struct Sphere {
    /// The radius of the sphere.
    pub radius: f32,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Sphere {
    const NAME: &'static str = "sphere";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Sphere {
            radius: parse_one("radius", &mut it, parse_elem)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for Sphere {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print("radius", &self.radius, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Declares a cylinder primitive that is centered on, and aligned with, the local y axis.
#[derive(Clone, Debug)]
pub struct Cylinder {
    /// The length of the cylinder along the y axis.
    pub height: f32,
    /// The radii of the cylinder (it may be elliptical).
    pub radius: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Cylinder {
    const NAME: &'static str = "cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Cylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: *parse_one("radius", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for Cylinder {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print("height", &self.height, w)?;
        ElemBuilder::print_arr("radius", &self.radius, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Describes a tapered cylinder primitive that is centered on and aligned with the local y axis.
#[derive(Clone, Debug)]
pub struct TaperedCylinder {
    /// The length of the cylinder along the y axis.
    pub height: f32,
    /// The radii of the tapered cylinder at the positive (height/2) Y value.
    /// Both ends of the tapered cylinder may be elliptical.
    pub radius1: [f32; 2],
    /// The radii of the tapered cylinder at the negative (height/2) Y value.
    /// Both ends of the tapered cylinder may be elliptical.
    pub radius2: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCylinder {
    const NAME: &'static str = "tapered_cylinder";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCylinder {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: *parse_one("radius1", &mut it, parse_array_n)?,
            radius2: *parse_one("radius2", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for TaperedCylinder {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print("height", &self.height, w)?;
        ElemBuilder::print_arr("radius1", &self.radius1, w)?;
        ElemBuilder::print_arr("radius2", &self.radius2, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Declares a capsule primitive that is centered on and aligned with the local y axis.
#[derive(Clone, Debug)]
pub struct Capsule {
    /// The length of the line segment connecting the centers of the capping hemispheres.
    pub height: f32,
    /// The radii of the capsule (it may be elliptical).
    pub radius: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Capsule {
    const NAME: &'static str = "capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Capsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius: *parse_one("radius", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for Capsule {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print("height", &self.height, w)?;
        ElemBuilder::print_arr("radius", &self.radius, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}

/// Describes a tapered capsule primitive that is centered on, and aligned with, the local y axis.
#[derive(Clone, Debug)]
pub struct TaperedCapsule {
    /// The length of the line segment connecting the centers of the capping hemispheres.
    pub height: f32,
    /// The radii of the tapered capsule at the positive (height/2) Y value.
    /// Both ends of the tapered capsule may be elliptical.
    pub radius1: [f32; 2],
    /// The radii of the tapered capsule at the negative (height/2) Y value.
    /// Both ends of the tapered capsule may be elliptical.
    pub radius2: [f32; 2],
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for TaperedCapsule {
    const NAME: &'static str = "tapered_capsule";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(TaperedCapsule {
            height: parse_one("height", &mut it, parse_elem)?,
            radius1: *parse_one("radius1", &mut it, parse_array_n)?,
            radius2: *parse_one("radius2", &mut it, parse_array_n)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNodeWrite for TaperedCapsule {
    fn write_to<W: Write>(&self, w: &mut XWriter<W>) -> Result<()> {
        let e = Self::elem().start(w)?;
        ElemBuilder::print("height", &self.height, w)?;
        ElemBuilder::print_arr("radius1", &self.radius1, w)?;
        ElemBuilder::print_arr("radius2", &self.radius2, w)?;
        self.extra.write_to(w)?;
        e.end(w)
    }
}
