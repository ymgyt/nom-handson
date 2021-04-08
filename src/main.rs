#![allow(dead_code)]

mod url_parser;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input: &str| match input.find(expected) {
        Some(idx) if idx == 0 => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut identifier = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => identifier.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            identifier.push(next);
        } else {
            break;
        }
    }

    Ok((&input[identifier.len()..], identifier))
}

fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
    where
        P1: Fn(&str) -> Result<(&str, R1), &str>,
        P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    move |input| {
        parser1(input).and_then(|(remain, result1)| match parser2(remain) {
            Ok((remain, result2)) => Ok((remain, (result1, result2))),
            Err(err) => Err(err),
        })
    }
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            NextParser: Parser<'a, NewOutput> + 'a,
            F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
    where
        F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(remain, result)| (remain, map_fn(result)))
    }
}

fn pair2<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    move |input| {
        parser1
            .parse(input)
            .and_then(|(remain, result1)| {
                parser2.parse(remain)
                    .map(|(remain, result2)| (remain, (result1, result2)))
            })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair2(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair2(parser1, parser2), |(_left, right)| right)
}

fn match_literal2<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

fn identifier2(input: &str) -> ParseResult<String> {
    let mut identifier = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => identifier.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            identifier.push(next);
        } else {
            break;
        }
    }

    Ok((&input[identifier.len()..], identifier))
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        match parser.parse(input) {
            Ok((remain, first)) => {
                input = remain;
                result.push(first);
            }
            Err(err) => return Err(err),
        }

        while let Ok((remain, item)) = parser.parse(input) {
            input = remain;
            result.push(item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where P: Parser<'a, A>
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((remain, item)) = parser.parse(input) {
            input = remain;
            result.push(item);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
        F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((remain, result)) = parser.parse(input) {
            if predicate(&result) {
                return Ok((remain, result));
            }
        }
        Err(input)
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            match_literal2("\""),
            left(
                zero_or_more(pred(any_char, |c| *c != '"')),
                match_literal2("\""),
            ),
        )
        , |chars: Vec<char>| chars.into_iter().collect(),
    )
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair2(identifier2, right(match_literal2("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal2("<"), pair2(identifier2, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    map(
        left(element_start(), match_literal2("/>")),
        |(name, attributes)| Element {
            name,
            attributes,
            children: Vec::new(),
        },
    )
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    map(
        left(element_start(), match_literal2(">")),
        |(name, attributes)| Element {
            name,
            attributes,
            children: Vec::new(),
        })
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
    where
        P1: Parser<'a, A>,
        P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

fn element<'a>() -> impl Parser<'a, Element> {
    whitespace(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    pred(
        right(match_literal2("</"), left(identifier, match_literal2(">"))),
        move |identifier: &String| *identifier == expected_name,
    )
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        NextP: Parser<'a, B>,
        F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((remain, result)) => f(result).parse(remain),
        Err(err) => Err(err),
    }
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        map(
            left(zero_or_more(element()), close_element(el.name.clone())),
            move |children| {
                let mut el = el.clone();
                el.children = children;
                el
            },
        )
    },
    )
}

fn whitespace<'a, P, A>(parser: P) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
        where
            P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}


fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse = match_literal("ymgyt");
        assert_eq!(Ok(("", ())), parse("ymgyt"), );
        assert_eq!(Ok((" foo bar", ())), parse("ymgyt foo bar"), );
        assert_eq!(Err("hello feris"), parse("hello feris"));
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok(("", "identifier-x-ok".to_owned())),
            identifier("identifier-x-ok"),
        );
        assert_eq!(
            Ok((" entirely an identifier", "not".to_owned())),
            identifier("not entirely an identifier"),
        );
        assert_eq!(
            Err("!invalid identifier"),
            identifier("!invalid identifier"),
        )
    }

    #[test]
    fn pair_combinator() {
        let open_tag = pair(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", ((), "first-element".to_owned()))),
            open_tag("<first-element/>"),
        );
        assert_eq!(Err("err"), open_tag("err"));
        assert_eq!(Err("!err"), open_tag("<!err"));
    }

    #[test]
    fn right_combinator() {
        let open_tag = right(match_literal2("<"), identifier2);
        assert_eq!(
            Ok(("/>", "first-element".to_owned())),
            open_tag.parse("<first-element/>"),
        );
        assert_eq!(Err("foo"), open_tag.parse("foo"));
        assert_eq!(Err("!foo"), open_tag.parse("<!foo"));
    }

    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal2("ok"));
        assert_eq!(Ok(("", vec!((), (), ()))), parser.parse("okokok"));
        assert_eq!(Err("kokok"), parser.parse("kokok"));
        assert_eq!(Err(""), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal2("ok"));
        assert_eq!(Ok(("", vec!((), (), ()))), parser.parse("okokok"));
        assert_eq!(Ok(("kokok", vec![])), parser.parse("kokok"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'x');
        assert_eq!(Ok(("yz", 'x')), parser.parse("xyz"));
        assert_eq!(Err("abc"), parser.parse("abc"));
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "Hello ymgyt!".to_owned())),
            quoted_string().parse("\"Hello ymgyt!\""),
        )
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok(("", vec![
                ("one".to_owned(), "1".to_owned()),
                ("two".to_owned(), "2".to_owned()),
            ])),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok(("", Element {
                name: "div".to_owned(),
                attributes: vec![("class".to_owned(), "xxx".to_owned()), ("style".to_owned(), "yyy".to_owned())],
                children: Vec::new(),
            })),
            single_element().parse("<div class=\"xxx\" style=\"yyy\"/>")
        );
    }

    #[test]
    fn html_parser() {
        let h = r#"
        <div class="container">
            <br/>
            <article class="post">
                <p label="hello"></p>
            </article>
        </div>
        "#;

        let parsed = Element {
            name: "div".to_owned(),
            attributes: vec![("class".to_owned(), "container".to_owned())],
            children: vec![
                Element {
                    name: "br".to_owned(),
                    attributes: Vec::new(),
                    children: Vec::new(),
                },
                Element {
                    name: "article".to_owned(),
                    attributes: vec![("class".to_owned(), "post".to_owned())],
                    children: vec![
                        Element {
                            name: "p".to_owned(),
                            attributes: vec![("label".to_owned(), "hello".to_owned())],
                            children: Vec::new(),
                        },
                    ],
                },
            ],
        };

        assert_eq!(Ok(("", parsed)), element().parse(h));
    }
}
