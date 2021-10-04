use crate::*;

/// Describes the visual shape and appearance of an object in a scene.
#[derive(Clone, Debug)]
pub struct Geometry {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The element that describes geometric data.
    pub element: GeometryElement,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl HasId for Geometry {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }
}

impl XNode for Geometry {
    const NAME: &'static str = "geometry";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Geometry {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            element: parse_one_many(&mut it, GeometryElement::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Extra data associated to [`Instance`]<[`Geometry`]>.
#[derive(Clone, Debug)]
pub struct InstanceGeometryData {
    /// Binds material symbols to material instances. This allows a
    /// single geometry to be instantiated into a scene multiple times
    /// each with a different appearance.
    pub bind_material: Option<BindMaterial>,
}

impl Instantiate for Geometry {
    const INSTANCE: &'static str = "instance_geometry";
    type Data = InstanceGeometryData;
    fn parse_data(_: &Element, it: &mut ElementIter<'_>) -> Result<Self::Data> {
        Ok(InstanceGeometryData {
            bind_material: BindMaterial::parse_opt(it)?,
        })
    }
}

impl Instance<Geometry> {
    /// Get the list of [`InstanceMaterial`]s bound in this [`Instance<Geometry>`].
    pub fn instance_materials(&self) -> &[InstanceMaterial] {
        match self.data.bind_material {
            Some(ref m) => &m.instance_material,
            None => &[],
        }
    }
}

/// An element that describes geometric data.
#[derive(Clone, Debug)]
pub enum GeometryElement {
    /// The parameter is a URI string of a `Geometry`, and this element
    /// describes the convex hull of the specified mesh.
    ConvexHullOf(Url),
    /// A mesh or convex mesh.
    Mesh(Mesh),
    /// A multisegment spline.
    Spline(Spline),
}

impl GeometryElement {
    /// Parse a [`GeometryElement`] from an XML element.
    pub fn parse(element: &Element) -> Result<Option<Self>> {
        Ok(Some(match element.name() {
            Mesh::CONVEX => Mesh::parse_convex(element)?,
            Mesh::NAME => GeometryElement::Mesh(Mesh::parse(false, element)?),
            Spline::NAME => GeometryElement::Spline(Spline::parse(element)?),
            _ => return Ok(None),
        }))
    }

    /// The `sources` field, which gives the list of [`Source`] elements on this element.
    pub fn sources(&self) -> &[Source] {
        match self {
            GeometryElement::ConvexHullOf(_) => &[],
            GeometryElement::Mesh(mesh) => &mesh.sources,
            GeometryElement::Spline(spline) => &spline.sources,
        }
    }

    /// Returns this element if it is a `<mesh>`.
    pub fn as_mesh(&self) -> Option<&Mesh> {
        match self {
            GeometryElement::Mesh(mesh) if !mesh.convex => Some(mesh),
            _ => None,
        }
    }
}

/// Describes basic geometric meshes using vertex and primitive information.
#[derive(Clone, Debug)]
pub struct Mesh {
    /// If true, this is a `<convex_mesh>` element.
    /// Both elements have the same structure otherwise.
    pub convex: bool,
    /// Provides the bulk of the mesh’s vertex data.
    pub sources: Vec<Source>,
    /// Describes the mesh-vertex attributes and establishes their topological identity.
    pub vertices: Option<Vertices>,
    /// Geometric primitives, which assemble values from the
    /// inputs into vertex attribute data.
    pub elements: Vec<Primitive>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl Mesh {
    /// The name of the XML node: `convex_mesh`
    pub const CONVEX: &'static str = "convex_mesh";

    /// Parse a `<convex_mesh>` XML element.
    pub fn parse_convex(element: &Element) -> Result<GeometryElement> {
        debug_assert_eq!(element.name(), Self::CONVEX);
        if let Some(s) = parse_attr(element.attr("convex_hull_of"))? {
            return Ok(GeometryElement::ConvexHullOf(s));
        }
        Ok(GeometryElement::Mesh(Mesh::parse(true, element)?))
    }

    /// Parse a [`Mesh`] from an XML element of type `<convex_mesh>` or `<mesh>`.
    pub fn parse(convex: bool, element: &Element) -> Result<Self> {
        debug_assert_eq!(
            element.name(),
            if convex { Self::CONVEX } else { Self::NAME }
        );
        let mut it = element.children().peekable();
        Ok(Mesh {
            convex,
            sources: Source::parse_list_n::<1>(&mut it)?,
            vertices: Vertices::parse_opt(&mut it)?,
            elements: parse_list_many(&mut it, Primitive::parse)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl XNode for Mesh {
    const NAME: &'static str = "mesh";
    fn parse(element: &Element) -> Result<Self> {
        Self::parse(false, element)
    }
}

/// Declares the attributes and identity of mesh-vertices.
#[derive(Clone, Debug)]
pub struct Vertices {
    /// A text string containing the unique identifier of the element.
    /// This value must be unique within the document.
    pub id: String,
    /// The text string name of this element.
    pub name: Option<String>,
    /// The list of inputs.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Position`] input (which must exist).
    pub position: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl HasId for Vertices {
    fn id(&self) -> Option<&str> {
        Some(&self.id)
    }
}

impl XNode for Vertices {
    const NAME: &'static str = "vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        Ok(Vertices {
            id: element.attr("id").ok_or("missing 'id' attr")?.into(),
            name: element.attr("name").map(Into::into),
            position: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Position)
                .ok_or("vertices: missing POSITION input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl Vertices {
    /// The input with [`Semantic::Position`].
    pub fn position_input(&self) -> &Input {
        &self.inputs[self.position]
    }
}

/// The common data for the geometry types:
///
/// * [`Lines`]` = Geom<`[`LineGeom`]`>`
/// * [`LineStrips`]` = Geom<`[`LineStripGeom`]`>`
/// * [`Polygons`]` = Geom<`[`PolygonGeom`]`>`
/// * [`PolyList`]` = Geom<`[`PolyListGeom`]`>`
/// * [`Triangles`]` = Geom<`[`TriangleGeom`]`>`
/// * [`TriFans`]` = Geom<`[`TriFanGeom`]`>`
/// * [`TriStrips`]` = Geom<`[`TriStripGeom`]`>`
#[derive(Clone, Default, Debug)]
pub struct Geom<T> {
    /// The text string name of this element.
    pub name: Option<String>,
    /// Declares a symbol for a material.
    /// This symbol is bound to a material at the time of instantiation;
    /// see [`Instance<Geometry>`] and [`BindMaterial`].
    /// If not specified then the lighting and shading results are application defined.
    pub material: Option<String>,
    /// The number of line/triangle/polygon primitives.
    pub count: usize,
    /// The vertex attribute access information.
    pub inputs: InputList,
    /// The specific data for
    pub data: T,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

/// The trait for types that can appear in a [`Geom<T>`].
pub trait ParseGeom: Default {
    /// The name of the element for the enclosing `Geom`, for example
    /// `"lines"` for [`LineGeom`].
    const NAME: &'static str;

    /// Parse the data from an element iterator.
    fn parse(it: &mut ElementIter<'_>) -> Result<Self>;

    /// Perform custom validation on the resulting [`Geom<T>`] before yielding it.
    fn validate(_: &Geom<Self>) -> Result<()>;
}

impl<T: ParseGeom> XNode for Geom<T> {
    const NAME: &'static str = T::NAME;

    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = Geom {
            name: element.attr("name").map(Into::into),
            material: element.attr("material").map(Into::into),
            count: parse_attr(element.attr("count"))?.ok_or("expected 'count' attr")?,
            inputs: InputList::parse::<0>(&mut it)?,
            data: T::parse(&mut it)?,
            extra: Extra::parse_many(it)?,
        };
        T::validate(&res)?;
        Ok(res)
    }
}

macro_rules! mk_primitive {
    ($($(#[$doc:meta])* $name:ident($as:ident),)*) => {
        /// A collection of primitive elements.
        #[derive(Clone, Debug)]
        pub enum Primitive {
            $($(#[$doc])* $name($name),)*
        }

        impl Primitive {
            /// Parse a [`Primitive`] from an XML element.
            pub fn parse(e: &Element) -> Result<Option<Self>> {
                Ok(Some(match e.name() {
                    $($name::NAME => Self::$name(Geom::parse(e)?),)*
                    _ => return Ok(None),
                }))
            }

            $(
                /// An accessor for the variant.
                pub fn $as(&self) -> Option<&$name> {
                    if let Self::$name(x) = self {
                        Some(x)
                    } else {
                        None
                    }
                }
            )*
        }

    }
}

mk_primitive! {
    /// Line primitives.
    Lines(as_lines),
    /// Line-strip primitives.
    LineStrips(as_line_strips),
    /// Polygon primitives which may contain holes.
    Polygons(as_polygons),
    /// Polygon primitives that cannot contain holes.
    PolyList(as_polylist),
    /// Triangle primitives.
    Triangles(as_triangles),
    /// Triangle-fan primitives.
    TriFans(as_trifans),
    /// Triangle-strip primitives.
    TriStrips(as_tristrips),
}

/// The data for a [`Lines`] element.
///
/// Each line described by the mesh has two vertices.
/// The first line is formed from the first and second vertices.
/// The second line is formed from the third and fourth vertices, and so on.
#[derive(Clone, Default, Debug)]
pub struct LineGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of individual lines.
    pub prim: Option<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual lines.
pub type Lines = Geom<LineGeom>;

impl Deref for LineGeom {
    type Target = Option<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for LineGeom {
    const NAME: &'static str = "lines";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineGeom {
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = *res.data {
            if res.inputs.depth * 2 * res.count != data.len() {
                return Err("line count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

/// The data for a [`LineStrips`] element.
///
/// Each line-strip described by the mesh has an arbitrary number of vertices.
/// Each line segment within the line-strip is formed from the current vertex and the preceding vertex.
#[derive(Clone, Default, Debug)]
pub struct LineStripGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected line segments.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed to bind vertex attributes together and then organize those vertices into
/// connected line-strips.
pub type LineStrips = Geom<LineStripGeom>;

impl Deref for LineStripGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for LineStripGeom {
    const NAME: &'static str = "line_strips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(LineStripGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("line strip count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<2>(p)) {
            return Err("incorrect <p> field in line strips".into());
        }
        Ok(())
    }
}

/// The data for an individual polygon-with-hole.
#[derive(Clone, Debug)]
pub struct PolygonHole {
    /// The vertex data for the polygon.
    pub verts: Box<[u32]>,
    /// A list of 0 or more holes, each of which describes a polygonal hole
    /// in the main polygon.
    pub hole: Vec<Box<[u32]>>,
}

/// The data for a [`Polygons`] element.
#[derive(Clone, Default, Debug)]
pub struct PolygonGeom(
    /// The list of polygons, each of which may contain 0 or more holes.
    pub Vec<PolygonHole>,
);

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual polygons.
pub type Polygons = Geom<PolygonGeom>;

impl Deref for PolygonGeom {
    type Target = Vec<PolygonHole>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ParseGeom for PolygonGeom {
    const NAME: &'static str = "polygon";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        let mut polys = parse_list("p", it, |e| {
            Ok(PolygonHole {
                verts: parse_array(e)?,
                hole: vec![],
            })
        })?;
        let more_polys = parse_list("ph", it, |e| {
            let mut it = e.children().peekable();
            let verts = parse_one("p", &mut it, parse_array)?;
            let hole = parse_list("h", &mut it, parse_array)?;
            if hole.is_empty() {
                return Err(
                    "<ph> element can only be used when at least one hole is present".into(),
                );
            }
            finish(PolygonHole { verts, hole }, it)
        })?;
        polys.extend(more_polys);
        Ok(PolygonGeom(polys))
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("polygon count does not match <p> fields".into());
        }
        if !res.data.iter().all(|ph| {
            res.inputs.check_prim::<3>(&ph.verts)
                && ph.hole.iter().all(|h| res.inputs.check_prim::<3>(h))
        }) {
            return Err("incorrect <p> field in polygon".into());
        }
        Ok(())
    }
}

/// The data for a [`PolyList`] element.
#[derive(Clone, Default, Debug)]
pub struct PolyListGeom {
    /// Contains a list of integers, each specifying the number of
    /// vertices for one polygon described by the [`PolyList`] element.
    pub vcount: Box<[u32]>,
    /// Contains a list of integers that specify the vertex attributes
    /// (indices) for an individual polylist.
    /// The winding order of vertices produced is counter-clockwise
    /// and describes the front side of each polygon.
    pub prim: Box<[u32]>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual polygons.
pub type PolyList = Geom<PolyListGeom>;

pub(crate) fn validate_vcount<T>(
    count: usize,
    depth: usize,
    vcount: &[u32],
    prim: &[T],
) -> Result<()> {
    if count != vcount.len() {
        return Err("count does not match <vcount> field".into());
    }
    if depth * vcount.iter().sum::<u32>() as usize != prim.len() {
        return Err("vcount does not match <p>/<v> field".into());
    }
    Ok(())
}

impl ParseGeom for PolyListGeom {
    const NAME: &'static str = "polylist";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(PolyListGeom {
            vcount: parse_opt("vcount", it, parse_array)?.unwrap_or_default(),
            prim: parse_opt("p", it, parse_array)?.unwrap_or_default(),
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        validate_vcount(
            res.count,
            res.inputs.depth,
            &res.data.vcount,
            &res.data.prim,
        )
    }
}

/// The data for a [`Triangles`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// The second triangle is formed from the fourth, fifth, and sixth vertices, and so on.
#[derive(Clone, Default, Debug)]
pub struct TriangleGeom {
    /// Contains indices that describe the vertex attributes for a number of triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Option<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into individual triangles.
pub type Triangles = Geom<TriangleGeom>;

impl Deref for TriangleGeom {
    type Target = Option<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriangleGeom {
    const NAME: &'static str = "triangles";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriangleGeom {
            prim: parse_opt("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if let Some(ref data) = *res.data {
            if res.inputs.depth * 3 * res.count != data.len() {
                return Err("triangle count does not match <p> field".into());
            }
        }
        Ok(())
    }
}

/// The data for a [`TriFans`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// Each subsequent triangle is formed from the current vertex,
/// reusing the first and the previous vertices.
#[derive(Clone, Default, Debug)]
pub struct TriFanGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into connected triangles.
pub type TriFans = Geom<TriFanGeom>;

impl Deref for TriFanGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriFanGeom {
    const NAME: &'static str = "trifans";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriFanGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("triangle fan count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle fans".into());
        }
        Ok(())
    }
}

/// The data for a [`TriStrips`] element.
///
/// Each triangle described by the mesh has three vertices.
/// The first triangle is formed from the first, second, and third vertices.
/// Each subsequent triangle is formed from the current vertex,
/// reusing the previous two vertices.
#[derive(Clone, Default, Debug)]
pub struct TriStripGeom {
    /// Contains indices that describe the vertex attributes for an
    /// arbitrary number of connected triangles.
    /// The indices reference into the parent’s [`Source`] elements that are
    /// referenced by the [`InputS`] elements.
    pub prim: Vec<Box<[u32]>>,
}

/// Provides the information needed for a mesh to bind vertex attributes
/// together and then organize those vertices into connected triangles.
pub type TriStrips = Geom<TriStripGeom>;

impl Deref for TriStripGeom {
    type Target = Vec<Box<[u32]>>;

    fn deref(&self) -> &Self::Target {
        &self.prim
    }
}

impl ParseGeom for TriStripGeom {
    const NAME: &'static str = "tristrips";

    fn parse(it: &mut ElementIter<'_>) -> Result<Self> {
        Ok(TriStripGeom {
            prim: parse_list("p", it, parse_array)?,
        })
    }

    fn validate(res: &Geom<Self>) -> Result<()> {
        if res.count != res.data.len() {
            return Err("triangle strip count does not match <p> fields".into());
        }
        if !res.data.iter().all(|p| res.inputs.check_prim::<3>(p)) {
            return Err("incorrect <p> field in triangle strips".into());
        }
        Ok(())
    }
}

/// Describes a multisegment spline with control vertex (CV) and segment information.
#[derive(Clone, Debug)]
pub struct Spline {
    /// Whether there is a segment connecting the first and last control vertices.
    /// The default is "false", indicating that the spline is open.
    pub closed: bool,
    /// Provides the values for the CVs and segments of the spline.
    pub sources: Vec<Source>,
    /// Describes the CVs of the spline.
    pub controls: ControlVertices,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Spline {
    const NAME: &'static str = "spline";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Spline {
            closed: parse_attr(element.attr("closed"))?.unwrap_or(false),
            sources: Source::parse_list_n::<1>(&mut it)?,
            controls: ControlVertices::parse_one(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Describes the control vertices (CVs) of a spline.
#[derive(Clone, Debug)]
pub struct ControlVertices {
    /// The list of inputs.
    pub inputs: Vec<Input>,
    /// The index into `inputs` for the [`Semantic::Position`] input (which must exist).
    pub position: usize,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for ControlVertices {
    const NAME: &'static str = "control_vertices";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let inputs = Input::parse_list(&mut it)?;
        Ok(ControlVertices {
            position: inputs
                .iter()
                .position(|i| i.semantic == Semantic::Position)
                .ok_or("control_vertices: missing POSITION input")?,
            inputs,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl ControlVertices {
    /// The input with [`Semantic::Position`].
    pub fn position_input(&self) -> &Input {
        &self.inputs[self.position]
    }
}
