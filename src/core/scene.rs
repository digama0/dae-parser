use crate::*;

#[cfg(feature = "nalgebra")]
use nalgebra::Matrix4;

/// Embodies the entire set of information that can be visualized
/// from the contents of a COLLADA resource.
#[derive(Clone, Default, Debug)]
pub struct Scene {
    /// The instantiated [`PhysicsScene`] elements describe
    /// any physics being applied to the scene.
    pub instance_physics_scene: Vec<Instance<PhysicsScene>>,
    /// The scene graph is built from the [`VisualScene`] elements instantiated under [`Scene`].
    pub instance_visual_scene: Option<Instance<VisualScene>>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Scene {
    const NAME: &'static str = "scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Scene {
            instance_physics_scene: Instance::parse_list(&mut it)?,
            instance_visual_scene: Instance::parse_opt(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Embodies the entire set of information that can be visualized
/// from the contents of a COLLADA resource.
#[derive(Clone, Debug)]
pub struct VisualScene {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// The scene graph subtrees.
    pub nodes: Vec<Node>,
    /// The [`EvaluateScene`] element declares information
    /// specifying how to evaluate this [`VisualScene`].
    pub evaluate_scene: Vec<EvaluateScene>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for VisualScene {
    const NAME: &'static str = "visual_scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(VisualScene {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            nodes: Node::parse_list_n::<1>(&mut it)?,
            evaluate_scene: EvaluateScene::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

/// Embodies the hierarchical relationship of elements in a scene.
///
/// The [`Node`] element declares a point of interest in a scene.
/// A node denotes one point on a branch of the scene graph.
/// The [`Node`] element is essentially the root of a subgraph of the entire scene graph.
#[derive(Clone, Debug)]
pub struct Node {
    /// A text string containing the unique identifier of the element.
    pub id: Option<String>,
    /// The text string name of this element.
    pub name: Option<String>,
    /// Asset management information about this element.
    pub asset: Option<Box<Asset>>,
    /// Any combination of geometric transforms.
    pub transforms: Vec<Transform>,
    /// Allows the node to instantiate a camera object.
    pub instance_camera: Vec<Instance<Camera>>,
    /// Allows the node to instantiate a controller object.
    pub instance_controller: Vec<Instance<Controller>>,
    /// Allows the node to instantiate a geometry object.
    pub instance_geometry: Vec<Instance<Geometry>>,
    /// Allows the node to instantiate a light object.
    pub instance_light: Vec<Instance<Light>>,
    /// Allows the node to instantiate a hierarchy of other nodes.
    pub instance_node: Vec<Instance<Node>>,
    /// Allows the node to recursively define hierarchy.
    pub children: Vec<Node>,
    /// Provides arbitrary additional information about this element.
    pub extra: Vec<Extra>,
}

impl XNode for Node {
    const NAME: &'static str = "node";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        Ok(Node {
            id: element.attr("id").map(Into::into),
            name: element.attr("name").map(Into::into),
            asset: Asset::parse_opt_box(&mut it)?,
            transforms: parse_list_many(&mut it, Transform::parse)?,
            instance_camera: Instance::parse_list(&mut it)?,
            instance_controller: Instance::parse_list(&mut it)?,
            instance_geometry: Instance::parse_list(&mut it)?,
            instance_light: Instance::parse_list(&mut it)?,
            instance_node: Instance::parse_list(&mut it)?,
            children: Node::parse_list(&mut it)?,
            extra: Extra::parse_many(it)?,
        })
    }
}

impl CollectLocalMaps for Node {
    fn collect_local_maps<'a>(&'a self, maps: &mut LocalMaps<'a>) {
        maps.insert(self);
        self.children.collect_local_maps(maps);
    }
}

impl Node {
    fn on_children<'a, E>(
        &'a self,
        f: &mut impl FnMut(&'a Self) -> Result<(), E>,
    ) -> Result<(), E> {
        f(self)?;
        for child in &self.children {
            child.on_children(f)?
        }
        Ok(())
    }
}

impl Traversable for Node {
    fn traverse<'a, E>(
        doc: &'a Document,
        mut f: impl FnMut(&'a Node) -> Result<(), E>,
    ) -> Result<(), E> {
        doc.library.iter().try_for_each(|elem| match elem {
            LibraryElement::Nodes(lib) => lib.items.iter().try_for_each(|e| e.on_children(&mut f)),
            LibraryElement::VisualScenes(lib) => lib
                .items
                .iter()
                .try_for_each(|e| e.nodes.iter().try_for_each(|e| e.on_children(&mut f))),
            _ => Ok(()),
        })
    }
}

#[cfg(feature = "nalgebra")]
impl Node {
    /// Apply the transformation stack on this node to a [`Matrix4`].
    /// If `mat` beforehand is the transformation applying at the parent of the
    /// current node, then `mat` after this call is the transformation that
    /// applies to everything in this node and in the children.
    pub fn prepend_transforms(&self, mat: &mut Matrix4<f32>) {
        for t in &self.transforms {
            t.prepend_to_matrix(mat)
        }
    }

    /// Apply the transformation stack on this node to a [`Matrix4`].
    /// If `mat` before this call is a transformation applying to a child of this node,
    /// then `mat` afterward is the transformation situating the child in the parent frame.
    pub fn append_transforms(&self, mat: &mut Matrix4<f32>) {
        for t in self.transforms.iter().rev() {
            t.append_to_matrix(mat)
        }
    }

    /// Convert this node's transformation stack to a [`Matrix4`].
    pub fn transform_as_matrix(&self) -> Matrix4<f32> {
        let mut mat = Matrix4::identity();
        self.prepend_transforms(&mut mat);
        mat
    }
}

/// Declares information specifying how to evaluate a [`VisualScene`].
#[derive(Clone, Debug)]
pub struct EvaluateScene {
    /// The text string name of this element.
    pub name: Option<String>,
    /// Describes the effect passes to evaluate a scene.
    pub render: Vec<Render>,
}

impl XNode for EvaluateScene {
    const NAME: &'static str = "evaluate_scene";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        let mut it = element.children().peekable();
        let res = EvaluateScene {
            name: element.attr("name").map(Into::into),
            render: Render::parse_list_n::<1>(&mut it)?,
        };
        finish(res, it)
    }
}
