use std::{collections::HashMap, marker::PhantomData, ops::Index};

use super::*;

/// A generic ID getter function.
pub trait HasId {
    /// Get the ID of the node.
    fn id(&self) -> Option<&str>;
}

/// A generic scoped ID getter function.
pub trait HasSId {
    /// Get the sid of the node.
    fn sid(&self) -> Option<&str>;
}

/// A wrapper around a base type `A` to indicate that the value is pointing to
/// a value of type `T`.
///
/// This is not a strict type safety barrier; it is possible to convert a `Ref`
/// to and from its raw version. However this can help in API documentation,
/// as well as to assist type inference in functions like [`LocalMap::get`].
#[derive(Copy, Clone)]
pub struct Ref<A, T: ?Sized> {
    /// The underlying storage or "raw" value.
    pub val: A,
    _marker: PhantomData<T>,
}

impl<A: Debug, T: ?Sized> Debug for Ref<A, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

/// A strongly typed URL referencing a `T`.
pub type UrlRef<T> = Ref<Url, T>;

/// A strongly typed string referencing a `T`.
pub type NameRef<T> = Ref<String, T>;

impl<A, T: ?Sized> Deref for Ref<A, T> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl<A, T: ?Sized> Ref<A, T> {
    /// Construct a new `Ref` by wrapping a raw value.
    pub fn new(val: A) -> Self {
        Self {
            val,
            _marker: PhantomData,
        }
    }
}

impl<A: FromStr, T: ?Sized> FromStr for Ref<A, T> {
    type Err = A::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        A::from_str(s).map(Ref::new)
    }
}

impl Document {
    /// Returns an iterator over `Library<T>` elements.
    pub fn library_iter<T>(&self) -> LibraryIter<'_, T> {
        LibraryIter {
            iter: self.library.iter(),
            _marker: PhantomData,
        }
    }
    /// Returns an iterator over all elements of type `T`.
    pub fn iter<T>(&self) -> ItemIter<'_, T> {
        ItemIter {
            iter: self.library_iter(),
            item: [].iter(),
        }
    }
}

/// An iterator over all `Library<T>` elements for a particular `T`,
/// returned by [`Document::library_iter`].
#[derive(Debug)]
pub struct LibraryIter<'a, T> {
    iter: std::slice::Iter<'a, LibraryElement>,
    _marker: PhantomData<T>,
}

impl<'a, T: ParseLibrary + 'a> Iterator for LibraryIter<'a, T> {
    type Item = &'a Library<T>;

    fn next(&mut self) -> Option<Self::Item> {
        for lib in &mut self.iter {
            if let Some(lib) = T::extract_element(lib) {
                return Some(lib);
            }
        }
        None
    }
}

/// An iterator over all `T` elements for a `ParseLibrary` type `T`,
/// returned by [`Document::iter`].
#[derive(Debug)]
pub struct ItemIter<'a, T> {
    iter: LibraryIter<'a, T>,
    item: std::slice::Iter<'a, T>,
}

impl<'a, T: ParseLibrary + 'a> Iterator for ItemIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(e) = self.item.next() {
                return Some(e);
            }
            if let Some(lib) = self.iter.next() {
                self.item = lib.items.iter();
            } else {
                return None;
            }
        }
    }
}

/// A trait for types that can be enumerated in a [`Document`]. This is used to power the
/// [`for_each`](Document::for_each) and [`try_for_each`](Document::try_for_each) functions.
pub trait Traversable {
    /// Run the function `f` on all elements of type `Self` in the document `doc`.
    fn traverse<'a, E>(
        doc: &'a Document,
        f: impl FnMut(&'a Self) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a;
}

impl Traversable for Sampler {
    fn traverse<'a, E>(
        doc: &'a Document,
        mut f: impl FnMut(&'a Self) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        doc.iter()
            .try_for_each(|lib: &Animation| lib.sampler.iter().try_for_each(&mut f))
    }
}

impl Traversable for Source {
    fn traverse<'a, E>(
        doc: &'a Document,
        mut f: impl FnMut(&'a Self) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        doc.library.iter().try_for_each(|elem| match elem {
            LibraryElement::Animations(lib) => lib
                .items
                .iter()
                .try_for_each(|anim| anim.source.iter().try_for_each(&mut f)),
            LibraryElement::Controllers(lib) => lib
                .items
                .iter()
                .try_for_each(|con| con.element.sources().iter().try_for_each(&mut f)),
            LibraryElement::Geometries(lib) => lib
                .items
                .iter()
                .try_for_each(|geom| geom.element.sources().iter().try_for_each(&mut f)),
            _ => Ok(()),
        })
    }
}

impl Animation {
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

impl Traversable for Animation {
    fn traverse<'a, E>(
        doc: &'a Document,
        mut f: impl FnMut(&'a Animation) -> Result<(), E>,
    ) -> Result<(), E> {
        doc.iter()
            .try_for_each(|e: &Animation| e.on_children(&mut f))
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

impl Traversable for Vertices {
    fn traverse<'a, E>(
        doc: &'a Document,
        mut f: impl FnMut(&'a Self) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        for lib in doc.iter::<Geometry>() {
            if let GeometryElement::Mesh(Mesh {
                vertices: Some(v), ..
            }) = &lib.element
            {
                f(v)?
            }
        }
        Ok(())
    }
}

/// A map for looking up elements of type `T` in the document by ID.
#[derive(Debug)]
pub struct LocalMap<'a, T>(pub HashMap<&'a str, &'a T>);

impl<'a, T> Default for LocalMap<'a, T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<'a, T> LocalMap<'a, T> {
    /// Look up an element by ID.
    pub fn push(&mut self, v: &'a T) -> Result<()>
    where
        T: HasId,
    {
        if let Some(id) = v.id() {
            if self.0.insert(id, v).is_some() {
                return Err(format!("duplicate id {}", id).into());
            }
        }
        Ok(())
    }

    /// Look up an element by ID.
    pub fn get_str(&self, n: &str) -> Option<&'a T> {
        self.0.get(n).copied()
    }

    /// Look up an element by URL reference.
    ///
    /// This is a local map, meaning that it does not support references to URLs which are not
    /// of the special form `#ref`, referring to an element with ID `ref` in the same document.
    pub fn get_raw(&self, url: &Url) -> Option<&'a T> {
        match url {
            Url::Fragment(n) => self.get_str(n),
            Url::Other(_) => None,
        }
    }

    /// Look up an element by URL reference.
    ///
    /// This is a simple wrapper around [`get_raw`](Self::get_raw),
    /// but it has better type safety, since it ensures that the reference is a reference to a `T`.
    ///
    /// This is a local map, meaning that it does not support references to URLs which are not
    /// of the special form `#ref`, referring to an element with ID `ref` in the same document.
    pub fn get(&self, url: &UrlRef<T>) -> Option<&'a T> {
        self.get_raw(&url.val)
    }
}

impl<'a, T> Index<&UrlRef<T>> for LocalMap<'a, T> {
    type Output = T;

    fn index(&self, index: &UrlRef<T>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl Document {
    /// Run a function over all elements of type `T` in the document.
    pub fn try_for_each<'a, T: Traversable + 'a, E, F: FnMut(&'a T) -> Result<(), E>>(
        &'a self,
        f: F,
    ) -> Result<(), E> {
        T::traverse(self, f)
    }

    /// Run a function over all elements of type `T` in the document.
    pub fn for_each<'a, T: Traversable + 'a>(&'a self, mut f: impl FnMut(&'a T)) {
        #[allow(clippy::unit_arg)]
        self.try_for_each(|e| Ok(f(e)))
            .unwrap_or_else(|e: std::convert::Infallible| match e {})
    }

    /// Construct an ID -> element mapping for node type `T`.
    /// This can be used to look up ID references.
    pub fn local_map<T: Traversable + HasId>(&self) -> Result<LocalMap<'_, T>> {
        let mut map = LocalMap::default();
        self.try_for_each(|v: &T| map.push(v))?;
        Ok(map)
    }

    /// Convenience function, to return the main [`VisualScene`]
    /// referred to in the `scene` field.
    pub fn get_visual_scene(&self) -> Option<&VisualScene> {
        let scene = self.scene.as_ref()?.instance_visual_scene.as_ref()?;
        self.local_map().ok()?.get(&scene.url)
    }
}
