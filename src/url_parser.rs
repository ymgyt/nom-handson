#[derive(Debug, PartialEq, Eq)]
pub struct URI<'a> {
    scheme: Scheme,
    authority: Option<Authority<'a>>,
    host: Host,
    port: Option<u16>,
    path: Option<Vec<&'a str>>,
    query: Option<QueryParams<'a>>,
    fragment: Option<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scheme {
    HTTP,
    HTTPS,
}

pub type Authority<'a> = (&'a str, Option<&'a str>);

#[derive(Debug, PartialEq, Eq)]
pub enum Host {
    HOST(String),
    IP([u8; 4]),
}

pub type QueryParam<'a> = (&'a str, &'a str);

pub type QueryParams<'a> = Vec<QueryParam<'a>>;

impl From<&str> for Scheme {
    fn from(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "http://" => Scheme::HTTP,
            "https://" => Scheme::HTTPS,
            _ => unimplemented!("unexpected scheme"),
        }
    }
}

use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag_no_case, tag};
use nom::error::{context, VerboseError};
use nom::sequence::{terminated, separated_pair};
use nom::character::complete::alphanumeric1;
use nom::combinator::opt;

type Res<T, U> = IResult<T, U, VerboseError<T>>;

fn scheme(input: &str) -> Res<&str, Scheme> {
    context(
        "scheme",
        alt((tag_no_case("HTTP://"), tag_no_case("HTTPS://"))),
    )(input)
        .map(|(remain, res)| (remain, res.into()))
}

fn authority(input: &str) -> Res<&str, (&str, Option<&str>)> {
    context(
        "authority",
        terminated(
            separated_pair(alphanumeric1, opt(tag(":")), opt(alphanumeric1)),
            tag("@"),
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{
        error::{ErrorKind, VerboseError, VerboseErrorKind},
        Err as NomError,
    };

    #[test]
    fn test_scheme() {
        assert_eq!(scheme("https://yay"), Ok(("yay", Scheme::HTTPS)));
        assert_eq!(scheme("http://yay"), Ok(("yay", Scheme::HTTP)));
        assert_eq!(
            scheme("file://yay"),
            Err(NomError::Error(VerboseError {
                errors: vec![
                    ("file://yay", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("file://yay", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("file://yay", VerboseErrorKind::Context("scheme")),
                ]
            }))
        );
    }

    #[test]
    fn test_authority() {
        assert_eq!(
            authority("username:password@example.org"),
            Ok(("example.org", ("username", Some("password"))))
        );
        assert_eq!(
            authority("username@example.org"),
            Ok(("example.org", ("username", None)))
        );
        assert_eq!(
            authority("example.org"),
            Err(NomError::Error(VerboseError {
                errors: vec![
                    (".org", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("example.org", VerboseErrorKind::Context("authority")),
                ]
            }))
        );
    }
}
