macro_rules! mk_extensible_enum {
    ($(#[$tydoc:meta])* pub enum $ty:ident { $($(#[$doc:meta])* $n:ident = $t:literal,)* }) => {
        $(#[$tydoc])*
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum $ty {
            $($(#[$doc])* $n,)*
            /// Any value not covered above
            Other(Box<str>),
        }

        impl $ty {
            fn parse(s: &str) -> Self {
                match s {
                    $($t => Self::$n,)*
                    _ => Self::Other(s.into()),
                }
            }
        }

        impl FromStr for $ty {
            type Err = std::convert::Infallible;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(Self::parse(s))
            }
        }
    };
}
