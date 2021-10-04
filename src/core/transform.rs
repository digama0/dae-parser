use crate::*;

#[cfg(feature = "nalgebra")]
use nalgebra::{Matrix4, Point3, Vector3};

/// A transformation, that can be represented as a matrix
/// (but may be expressed in another way for convenience).
#[derive(Clone, Debug)]
pub enum Transform {
    /// Contains a position and orientation transformation suitable for aiming a camera.
    LookAt(LookAt),
    /// A generic 4x4 matrix.
    Matrix(Matrix),
    /// An axis-angle rotation.
    Rotate(Rotate),
    /// A scale along the three dimentions.
    Scale(Scale),
    /// A skew deformation.
    Skew(Skew),
    /// A translation by a vector.
    Translate(Translate),
}

impl Transform {
    /// Parse a [`Transform`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            LookAt::NAME => Ok(Some(Self::LookAt(LookAt::parse(e)?))),
            Matrix::NAME => Ok(Some(Self::Matrix(Matrix::parse(e)?))),
            Rotate::NAME => Ok(Some(Self::Rotate(Rotate::parse(e)?))),
            Scale::NAME => Ok(Some(Self::Scale(Scale::parse(e)?))),
            Skew::NAME => Ok(Some(Self::Skew(Skew::parse(e)?))),
            Translate::NAME => Ok(Some(Self::Translate(Translate::parse(e)?))),
            _ => Ok(None),
        }
    }

    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        match self {
            Transform::Translate(tr) => tr.as_matrix(),
            Transform::Rotate(tr) => tr.as_matrix(),
            Transform::LookAt(tr) => tr.as_matrix(),
            Transform::Matrix(tr) => tr.as_matrix(),
            Transform::Scale(tr) => tr.as_matrix(),
            Transform::Skew(tr) => tr.as_matrix(),
        }
    }
}

/// A [`RigidBody`] transform is a subset of the full set of [`Transform`]s
/// which restricts to euclidean transformations (translation and rotation).
#[derive(Clone, Debug)]
pub enum RigidTransform {
    /// A translation.
    Translate(Translate),
    /// A rotation.
    Rotate(Rotate),
}

impl RigidTransform {
    /// Parse a [`RigidTransform`] from an XML element.
    pub fn parse(e: &Element) -> Result<Option<Self>> {
        match e.name() {
            Translate::NAME => Ok(Some(Self::Translate(Translate::parse(e)?))),
            Rotate::NAME => Ok(Some(Self::Rotate(Rotate::parse(e)?))),
            _ => Ok(None),
        }
    }

    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        match self {
            RigidTransform::Translate(tr) => tr.as_matrix(),
            RigidTransform::Rotate(tr) => tr.as_matrix(),
        }
    }
}

/// Contains a position and orientation transformation suitable for aiming a camera.
#[derive(Clone, Debug)]
pub struct LookAt(
    /// A list of 9 floating-point values.
    /// These values are organized into three vectors as follows:
    /// 1.  Eye position is given as Px, Py, Pz.
    /// 2.  Interest point is given as Ix, Iy, Iz.
    /// 3.  Up-axis direction is given as UPx, UPy, UPz.
    pub Box<[f32; 9]>,
);

impl XNode for LookAt {
    const NAME: &'static str = "lookat";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(LookAt(parse_array_n(element)?))
    }
}

impl LookAt {
    /// The eye position for this look-at orientation.
    #[inline]
    pub fn eye(&self) -> &[f32; 3] {
        self.0[0..3].try_into().unwrap()
    }

    /// The target position (interest point) for this look-at orientation.
    #[inline]
    pub fn target(&self) -> &[f32; 3] {
        self.0[3..6].try_into().unwrap()
    }

    /// The up direction for this look-at orientation.
    #[inline]
    pub fn up(&self) -> &[f32; 3] {
        self.0[6..9].try_into().unwrap()
    }

    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        let eye = Point3::from_slice(self.eye());
        let target = Point3::from_slice(self.target());
        let up = Vector3::from_column_slice(self.up());
        Matrix4::look_at_rh(&eye, &target, &up)
    }
}

/// Describes transformations that embody mathematical changes to points
/// within a coordinate system or the coordinate system itself.
#[derive(Clone, Debug)]
pub struct Matrix(
    /// A list of 16 floating-point values.
    /// These values are organized into a 4-by-4
    /// column-order matrix suitable for matrix composition.
    pub Box<[f32; 16]>,
);

impl XNode for Matrix {
    const NAME: &'static str = "matrix";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Matrix(parse_array_n(element)?))
    }
}

impl Matrix {
    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        Matrix4::from_column_slice(&*self.0)
    }
}

/// Specifies how to rotate an object around an axis.
#[derive(Clone, Debug)]
pub struct Rotate(
    /// A list of four floating-point values.
    /// These values are organized into a column vector `[X, Y, Z]`
    /// specifying the axis of rotation, followed by an angle in degrees.
    pub Box<[f32; 4]>,
);

impl XNode for Rotate {
    const NAME: &'static str = "rotate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Rotate(parse_array_n(element)?))
    }
}

impl Rotate {
    /// The rotation axis of this rotation transform.
    pub fn axis(&self) -> &[f32; 3] {
        self.0[0..3].try_into().unwrap()
    }

    /// The magnitude of this rotation transform in degrees.
    pub fn angle(&self) -> f32 {
        self.0[3]
    }

    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        let axis = Vector3::from_column_slice(self.axis()).normalize();
        Matrix4::from_axis_angle(&nalgebra::Unit::new_normalize(axis), self.angle())
    }
}

/// Specifies how to change an object’s size.
#[derive(Clone, Debug)]
pub struct Scale(
    /// A list of three floating-point values.
    /// These values are organized into a column vector suitable for matrix composition.
    pub Box<[f32; 3]>,
);

impl XNode for Scale {
    const NAME: &'static str = "scale";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Scale(parse_array_n(element)?))
    }
}

impl Scale {
    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        Matrix4::new_nonuniform_scaling(&Vector3::from_row_slice(&*self.0))
    }
}

/// Specifies how to deform an object along one axis.
#[derive(Clone, Debug)]
pub struct Skew(
    /// A list of seven floating-point values.
    /// These values are organized into an angle in degrees
    /// followed by two column vectors specifying the axes of rotation and translation.
    pub Box<[f32; 7]>,
);

impl XNode for Skew {
    const NAME: &'static str = "skew";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Skew(parse_array_n(element)?))
    }
}

impl Skew {
    /// The angle of this skew transformation.
    #[inline]
    pub fn angle(&self) -> f32 {
        self.0[0]
    }

    /// The axis of rotation for this skew transformation.
    #[inline]
    pub fn axis(&self) -> &[f32; 3] {
        self.0[1..4].try_into().unwrap()
    }

    /// The axis of translation for this skew transformation.
    #[inline]
    pub fn trans(&self) -> &[f32; 3] {
        self.0[4..7].try_into().unwrap()
    }

    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        // From the RenderMan spec (on which the COLLADA operation is based):
        //
        // This operation shifts all points along lines
        // parallel to the axis vector (dx2, dy2, dz2).
        // Points along the axis vector (dx1, dy1, dz1)
        // are mapped onto the vector (x, y, z),
        // where angle specifies the angle (in degrees)
        // between the vectors (dx1, dy1, dz1) and (x, y, z),
        // The two axes are not required to be perpendicular,
        // however it is an error to specify an angle that is greater than
        // or equal to the angle between them.
        // A negative angle can be specified,
        // but it must be greater than 180 degrees minus the angle between the two axes.
        unimplemented!("<skew> transforms are not supported")
    }
}

/// Changes the position of an object in a local coordinate system.
#[derive(Clone, Debug)]
pub struct Translate(
    /// A list of three floating-point values.
    /// These values are organized into a column vector suitable for a matrix composition.
    pub Box<[f32; 3]>,
);

impl XNode for Translate {
    const NAME: &'static str = "translate";
    fn parse(element: &Element) -> Result<Self> {
        debug_assert_eq!(element.name(), Self::NAME);
        Ok(Translate(parse_array_n(element)?))
    }
}

impl Translate {
    #[cfg(feature = "nalgebra")]
    /// Convert this transformation to a [`nalgebra::Matrix4`].
    pub fn as_matrix(&self) -> Matrix4<f32> {
        Matrix4::new_translation(&Vector3::from_row_slice(&*self.0))
    }
}
