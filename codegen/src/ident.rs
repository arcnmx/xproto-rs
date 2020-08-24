/*fn needs_match_by_value(field: &xcbdefs::FieldDef) -> bool {
    match field {
        xcbdefs::FieldDef::Normal(normal_field) => {
            match normal_field.type_.type_.get_resolved().get_original_type() {
                xcbdefs::TypeRef::BuiltIn(_) => true,
                xcbdefs::TypeRef::Xid(_) => true,
                xcbdefs::TypeRef::XidUnion(_) => true,
                _ => false,
            }
        }
        _ => false,
    }
}*/

/// Converts a type name from the XML to a rust
/// type name (in CamelCase).
///
/// If the name is all uppercase, all but the first
/// letter are converter to lowercase.
pub fn to_rust_type_name(name: &str) -> String {
    let mut name = String::from(name);
    if name.bytes().all(|c| !c.is_ascii_lowercase()) {
        name.make_ascii_lowercase();
    }

    // Convert to camel case
    let mut r = String::new();
    for chunk in name.split('_') {
        r.push_str(&chunk[..1]);
        let r_len = r.len();
        r[(r_len - 1)..].make_ascii_uppercase();
        r.push_str(&chunk[1..]);
    }
    r
}

/// Converts a type name from the XML to a rust
/// enum type name (in CamelCase).
///
/// If the name is not snake_case and is all uppercase,
/// it is left as is.
pub fn to_rust_enum_type_name(name: &str) -> String {
    if name.contains('_') {
        to_rust_type_name(name)
    } else {
        name.into()
    }
}

/// Converts a name from the XML to a Rust variable
/// name (snake_case).
pub fn to_rust_variable_name(name: &str) -> String {
    if name == "type" {
        "type_".into()
    } else if name == "match" {
        "match_".into()
    } else if name.bytes().any(|c| c.is_ascii_uppercase()) {
        // Deal with CamelCase
        camel_case_to_lower_snake(name)
    } else {
        name.into()
    }
}

/// Converts the name of a enum value from the XML
/// to a Rust name.
pub fn ename_to_rust(name: &str) -> String {
    let mut name = String::from(name);
    if name.contains('_') && name.bytes().any(|c| c.is_ascii_lowercase()) {
        // xf86vidmode has a ModeFlag enum with items like
        // Positive_HSync. Turn this into PositiveHSync.
        name = name.replace('_', "");
    }
    if name.as_bytes()[0].is_ascii_digit() {
        name.insert(0, '_');
    }
    name[..1].make_ascii_uppercase();
    name
}

pub fn camel_case_to_snake(arg: &str) -> String {
    assert!(
        arg.bytes().all(|c| c.is_ascii_alphanumeric() || c == b'_'),
        "{:?}",
        arg
    );

    // Matches "[A-Z][a-z0-9]+|[A-Z]+(?![a-z0-9])|[a-z0-9]+"
    struct Matcher<'a> {
        remaining: &'a str,
    }

    impl<'a> Matcher<'a> {
        fn new(s: &'a str) -> Self {
            Self { remaining: s }
        }
    }

    impl<'a> Iterator for Matcher<'a> {
        type Item = &'a str;

        fn next(&mut self) -> Option<&'a str> {
            enum State {
                Begin,
                // "[A-Z]"
                OneUpper(usize),
                // "[A-Z][a-z0-9]+"
                OneUpperThenLowerOrDigit(usize),
                // "[A-Z][A-Z]+"
                ManyUpper(usize),
                // "[a-z0-9]+"
                LowerOrDigit(usize),
            }

            let s = self.remaining;
            let mut chr_iter = s.char_indices();
            let mut state = State::Begin;
            let next_match = loop {
                let (chr_i, chr) = chr_iter
                    .next()
                    .map(|(chr_i, chr)| (chr_i, Some(chr)))
                    .unwrap_or((s.len(), None));
                match state {
                    State::Begin => match chr {
                        None => break None,
                        Some('A'..='Z') => state = State::OneUpper(chr_i),
                        Some('a'..='z') | Some('0'..='9') => state = State::LowerOrDigit(chr_i),
                        Some(_) => state = State::Begin,
                    },
                    State::OneUpper(begin_i) => match chr {
                        Some('A'..='Z') => state = State::ManyUpper(begin_i),
                        Some('a'..='z') | Some('0'..='9') => {
                            state = State::OneUpperThenLowerOrDigit(begin_i)
                        }
                        _ => break Some((&s[begin_i..chr_i], &s[chr_i..])),
                    },
                    State::OneUpperThenLowerOrDigit(begin_i) => match chr {
                        Some('a'..='z') | Some('0'..='9') => {
                            state = State::OneUpperThenLowerOrDigit(begin_i)
                        }
                        _ => break Some((&s[begin_i..chr_i], &s[chr_i..])),
                    },
                    State::ManyUpper(begin_i) => match chr {
                        Some('A'..='Z') => state = State::ManyUpper(begin_i),
                        Some('a'..='z') | Some('0'..='9') => {
                            break Some((&s[begin_i..(chr_i - 1)], &s[(chr_i - 1)..]));
                        }
                        _ => break Some((&s[begin_i..chr_i], &s[chr_i..])),
                    },
                    State::LowerOrDigit(begin_i) => match chr {
                        Some('a'..='z') | Some('0'..='9') => state = State::LowerOrDigit(begin_i),
                        _ => break Some((&s[begin_i..chr_i], &s[chr_i..])),
                    },
                }
            };

            if let Some((match_str, remaining)) = next_match {
                self.remaining = remaining;
                Some(match_str)
            } else {
                None
            }
        }
    }

    let mut r = String::new();
    for match_str in Matcher::new(arg) {
        if !r.is_empty() {
            r.push('_');
        }
        r.push_str(match_str);
    }
    r
}

pub fn camel_case_to_lower_snake(arg: &str) -> String {
    let mut r = camel_case_to_snake(arg);
    r.make_ascii_lowercase();
    r
}

pub fn camel_case_to_upper_snake(arg: &str) -> String {
    let mut r = camel_case_to_snake(arg);
    r.make_ascii_uppercase();
    r
}
