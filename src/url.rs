use std::{fmt::Debug, str::FromStr};

use percent_encoding::{percent_decode_str, percent_encode, AsciiSet, CONTROLS};

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

#[derive(Clone)]
pub enum Url {
    Fragment(String),
    Other(String),
}

impl FromStr for Url {
    type Err = std::str::Utf8Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.chars();
        Ok(if it.next() == Some('#') {
            Url::Fragment(percent_decode_str(it.as_str()).decode_utf8()?.into())
        } else {
            Url::Other(s.into())
        })
    }
}

impl Debug for Url {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fragment(s) => format!("#{}", percent_encode(s.as_bytes(), FRAGMENT)).fmt(f),
            Self::Other(s) => s.fmt(f),
        }
    }
}
