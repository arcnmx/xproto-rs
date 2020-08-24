use core::str::FromStr;
use core::num::ParseIntError;
use std::env::{self, VarError};
use std::{error, fmt, io};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseDisplayError {
    ParseInt(ParseIntError),
    Var(VarError),
    MissingColon,
}

impl ParseDisplayError {
    pub fn is_var_not_present(&self) -> bool {
        match self {
            Self::Var(VarError::NotPresent) => true,
            _ => false,
        }
    }
}

impl From<ParseIntError> for ParseDisplayError {
    fn from(e: ParseIntError) -> Self {
        Self::ParseInt(e)
    }
}

impl From<VarError> for ParseDisplayError {
    fn from(e: VarError) -> Self {
        Self::Var(e)
    }
}

impl From<ParseDisplayError> for io::Error {
    fn from(e: ParseDisplayError) -> Self {
        io::Error::new(io::ErrorKind::InvalidData, e)
    }
}

impl fmt::Display for ParseDisplayError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseInt(e) => fmt::Display::fmt(e, f),
            Self::Var(e) => fmt::Display::fmt(e, f),
            Self::MissingColon => fmt::Display::fmt("DISPLAY missing colon", f),
        }
    }
}

impl error::Error for ParseDisplayError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::ParseInt(e) => Some(e),
            Self::Var(e) => Some(e),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Display {
    pub protocol: Option<String>,
    pub host: String,
    pub display: u16,
    pub screen: u16,
}

impl Display {
    pub fn new(display: Option<&str>) -> Result<Self, ParseDisplayError> {
        match display {
            Some(display) => display.parse(),
            None => Self::with_env(),
        }
    }

    pub fn with_env() -> Result<Self, ParseDisplayError> {
        env::var("DISPLAY")
            .map_err(From::from)
            .and_then(|d| d.parse())
    }
}

impl fmt::Display for Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(protocol) = &self.protocol {
            write!(f, "{}/", protocol)?;
        }
        write!(f, "{}:{}", self.host, self.display)?;
        if self.screen > 0 {
            write!(f, ".{}", self.screen)
        } else {
            Ok(())
        }
    }
}

impl FromStr for Display {
    type Err = ParseDisplayError;

    fn from_str(display: &str) -> Result<Self, Self::Err> {
        // Everything up to the last '/' is the protocol. This part is optional.
        let (protocol, remaining) = if let Some(pos) = display.rfind('/') {
            (Some(&display[..pos]), &display[pos + 1..])
        } else {
            (None, display)
        };

        // Everything up to the last ':' is the host. This part is required.
        let pos = remaining.rfind(':')
            .ok_or(ParseDisplayError::MissingColon)?;
        let (host, remaining) = (&remaining[..pos], &remaining[pos + 1..]);

        // The remaining part is display.screen. The display is required and the screen optional.
        let (display, screen) = match remaining.find('.') {
            Some(pos) => (&remaining[..pos], &remaining[pos + 1..]),
            None => (remaining, "0"),
        };

        // Parse the display and screen number
        let (display, screen) = (
            display.parse()?,
            screen.parse()?,
        );

        let host = host.to_string();
        let protocol = protocol.map(|p| p.to_string());
        Ok(Self {
            host,
            protocol,
            display,
            screen,
        })
    }
}

#[cfg(test)]
mod test {
    use super::{Display, ParseDisplayError};

    fn do_parse_display(input: &str) -> Result<Display, ParseDisplayError> {
        std::env::set_var("DISPLAY", input);
        let result1 = Display::new(None);

        std::env::remove_var("DISPLAY");
        let result2 = Display::new(Some(input));

        assert_eq!(result1, result2);

        if let Ok(display) = &result1 {
            assert_eq!(input, &display.to_string());
        }

        result1
    }

    // The tests modify environment variables. This is process-global. Thus, the tests in this
    // module cannot be run concurrently. We achieve this by having only a single test functions
    // that calls all other functions.
    #[test]
    fn test_parsing() {
        test_missing_input();
        xcb_good_cases();
        xcb_bad_cases();
        own_good_cases();
    }

    fn test_missing_input() {
        std::env::remove_var("DISPLAY");
        assert!(Display::new(None).unwrap_err().is_var_not_present());
    }

    fn own_good_cases() {
        // The XCB test suite does not test protocol parsing
        for (input, output) in &[
            (
                "foo/bar:1",
                Display {
                    host: "bar".to_string(),
                    protocol: Some("foo".to_string()),
                    display: 1,
                    screen: 0,
                },
            ),
            (
                "foo/bar:1.2",
                Display {
                    host: "bar".to_string(),
                    protocol: Some("foo".to_string()),
                    display: 1,
                    screen: 2,
                },
            ),
            (
                "a:b/c/foo:bar:1.2",
                Display {
                    host: "foo:bar".to_string(),
                    protocol: Some("a:b/c".to_string()),
                    display: 1,
                    screen: 2,
                },
            ),
            ] {
                assert_eq!(
                    do_parse_display(input).as_ref(),
                    Ok(output),
                    "Failed parsing correctly: {}",
                    input
                );
            }
    }

    // Based on libxcb's test suite; (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett
    fn xcb_good_cases() {
        for (input, output) in &[
            // unix
            (
                ":0",
                Display {
                    host: "".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                ":1",
                Display {
                    host: "".to_string(),
                    protocol: None,
                    display: 1,
                    screen: 0,
                },
            ),
            (
                ":0.1",
                Display {
                    host: "".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            // ip
            (
                "x.org:0",
                Display {
                    host: "x.org".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "expo:0",
                Display {
                    host: "expo".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "bigmachine:1",
                Display {
                    host: "bigmachine".to_string(),
                    protocol: None,
                    display: 1,
                    screen: 0,
                },
            ),
            (
                "hydra:0.1",
                Display {
                    host: "hydra".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            // ipv4
            (
                "198.112.45.11:0",
                Display {
                    host: "198.112.45.11".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "198.112.45.11:0.1",
                Display {
                    host: "198.112.45.11".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            // ipv6
            (
                ":::0",
                Display {
                    host: "::".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "1:::0",
                Display {
                    host: "1::".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "::1:0",
                Display {
                    host: "::1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "::1:0.1",
                Display {
                    host: "::1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            (
                "::127.0.0.1:0",
                Display {
                    host: "::127.0.0.1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "::ffff:127.0.0.1:0",
                Display {
                    host: "::ffff:127.0.0.1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "2002:83fc:3052::1:0",
                Display {
                    host: "2002:83fc:3052::1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "2002:83fc:3052::1:0.1",
                Display {
                    host: "2002:83fc:3052::1".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            (
                "[::]:0",
                Display {
                    host: "[::]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "[1::]:0",
                Display {
                    host: "[1::]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "[::1]:0",
                Display {
                    host: "[::1]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "[::1]:0.1",
                Display {
                    host: "[::1]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            (
                "[::127.0.0.1]:0",
                Display {
                    host: "[::127.0.0.1]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "[2002:83fc:d052::1]:0",
                Display {
                    host: "[2002:83fc:d052::1]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "[2002:83fc:d052::1]:0.1",
                Display {
                    host: "[2002:83fc:d052::1]".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
            // decnet
            (
                "myws::0",
                Display {
                    host: "myws:".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "big::0",
                Display {
                    host: "big:".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 0,
                },
            ),
            (
                "hydra::0.1",
                Display {
                    host: "hydra:".to_string(),
                    protocol: None,
                    display: 0,
                    screen: 1,
                },
            ),
        ] {
            assert_eq!(
                do_parse_display(input).as_ref(),
                Ok(output),
                "Failed parsing correctly: {}",
                input
            );
        }
    }

    // Based on libxcb's test suite; (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett
    fn xcb_bad_cases() {
        for input in &[
            "",
            ":",
            "::",
            ":::",
            ":.",
            ":a",
            ":a.",
            ":0.",
            ":.a",
            ":.0",
            ":0.a",
            ":0.0.",
            "127.0.0.1",
            "127.0.0.1:",
            "127.0.0.1::",
            "::127.0.0.1",
            "::127.0.0.1:",
            "::127.0.0.1::",
            "::ffff:127.0.0.1",
            "::ffff:127.0.0.1:",
            "::ffff:127.0.0.1::",
            "localhost",
            "localhost:",
            "localhost::",
            ] {
                assert!(
                    do_parse_display(input).is_err(),
                    "Unexpectedly parsed: {}",
                    input
                );
            }
    }
}
