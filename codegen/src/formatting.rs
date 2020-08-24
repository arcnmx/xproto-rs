use std::fmt;
use xcbgen::defs::{SwitchKind, AlignBody};
use super::{HeaderKind, Header,
    Encoded, EncodedExpr, EncodedStruct, EncodedEnum, EncodedField, EncodedAlias, EncodedUnion, EncodedSwitch, EncodedEnumValue, EncodedSwitchCase,
    TypeName,
};

impl fmt::Display for HeaderKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            HeaderKind::Request => "RequestHeader",
            HeaderKind::ExtensionRequest => "ExtensionRequestHeader",
            HeaderKind::Reply => "ReplyHeader",
            HeaderKind::Event => "EventHeader",
            HeaderKind::PlainEvent => "PlainEventHeader",
            HeaderKind::GenericEvent => "GenericEventHeader",
            HeaderKind::Error => "ErrorHeader",
        };
        f.write_str(name)
    }
}

struct HeaderDisplay<'a> {
    name: &'a TypeName,
    header: &'a Header,
}

impl<'a> fmt::Display for HeaderDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.header {
            Header::Request { reply, opcode, combine_adjacent } => {
                writeln!(f, "impl Request for {} {{", self.name)?;
                writeln!(f, "\ttype Reply = {};", reply)?;
                writeln!(f, "\tconst COMBINE_ADJACENT: bool = {:?};", combine_adjacent)?;
                writeln!(f, "\tconst INFO: RequestInfo = RequestInfo::Core {{")?;
                writeln!(f, "\t\topcode: {},", opcode)?;
                writeln!(f, "\t}};")?;
                writeln!(f, "}}")?;
            },
            Header::ExtensionRequest { reply, minor_opcode, combine_adjacent, ext } => {
                writeln!(f, "impl Request for {} {{", self.name)?;
                writeln!(f, "\ttype Reply = {};", reply)?;
                writeln!(f, "\tconst COMBINE_ADJACENT: bool = {:?};", combine_adjacent)?;
                writeln!(f, "\tconst INFO: RequestInfo = RequestInfo::Extension {{")?;
                writeln!(f, "\t\tminor_opcode: {},", minor_opcode)?;
                writeln!(f, "\t\textension: ExtensionInfo {{")?;
                writeln!(f, "\t\t\txname = Cow::Borrowed(\"{}\"),", ext.xname)?;
                writeln!(f, "\t\t\tname = Cow::Borrowed(\"{}\"),", ext.name)?;
                writeln!(f, "\t\t\tmultiword = {:?},", ext.multiword)?;
                writeln!(f, "\t\t\tmajor_version = {},", ext.major_version)?;
                writeln!(f, "\t\t\tminor_version = {},", ext.minor_version)?;
                writeln!(f, "\t\t}},")?;
                writeln!(f, "\t}};")?;
                writeln!(f, "}}")?;
            },
            Header::Reply { request } => {
                writeln!(f, "impl Reply for {} {{", self.name)?;
                writeln!(f, "\ttype Request = {};", request)?;
                writeln!(f, "}}")?;
            },
            Header::Error { number } => {
                writeln!(f, "impl Error for {} {{", self.name)?;
                writeln!(f, "\tconst NUMBER: i8 = {};", number)?;
                writeln!(f, "}}")?;
            },
            Header::Event { number, is_plain, is_generic } => {
                writeln!(f, "impl Event for {} {{", self.name)?;
                writeln!(f, "\tconst NUMBER: u16 = {};", number)?;
                writeln!(f, "\tconst IS_PLAIN: bool = {:?};", is_plain)?;
                writeln!(f, "\tconst IS_GENERIC: bool = {:?};", is_generic)?;
                writeln!(f, "}}")?;
            },
        }

        Ok(())
    }
}

impl fmt::Display for EncodedExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_const {
            write!(f, "\tpub const {}: {} = {};", self.name, self.ty, self.expr)
        } else {
            writeln!(f, "\tpub fn {}(&self) -> {} {{", self.name, self.ty)?;
            writeln!(f, "\t\t{}", self.expr)?;
            writeln!(f, "\t}}")
        }
    }
}

impl fmt::Display for EncodedStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let alignment = &self.alignment;
        assert!(alignment.internal_align <= alignment.begin.align()); // TODO: what is this for?

        let repr = if self.copy {
            format!("#[repr(C, align({}))]", alignment.begin.align())
        } else {
            "".into()
        };
        // TODO: Copy/Clone if type is POD
        let copy = if self.copy {
            ", Copy,/* Pod,*/"
        } else {
            ""
        };
        writeln!(f, "
{}#[derive(Debug{} Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct {} {{", repr, copy, self.name)?;
        if alignment.begin.offset() > 0 && self.copy {
            writeln!(f, "\tpub padding_offset: [u8; {}],", alignment.begin.offset())?;
        }
        for field in &self.fields {
            writeln!(f, "{},", field)?;
        }

        writeln!(f, "}}")?;

        if !self.exprs.is_empty() {
            writeln!(f, "impl {} {{", self.name)?;
            for expr in &self.exprs {
                writeln!(f, "{}", expr)?;
            }
            writeln!(f, "}}")?;
        }

        if let Some(header) = &self.header {
            writeln!(f, "{}", HeaderDisplay {
                name: &self.name,
                header,
            })?;
        }

        writeln!(f, "impl Message for {} {{", self.name)?;
        writeln!(f, "\tconst ALIGNMENT: MessageAlignment = MessageAlignment::with_offset({}, {});",
            self.alignment.begin.align(), self.alignment.begin.offset())?;
        writeln!(f, "\t}};")?;
        match self.alignment.body {
            AlignBody::EndAlign(alignment) => {
                writeln!(f, "\tconst SIZE: MessageSize = MessageSize::Variable {{")?;
                writeln!(f, "\t\tminimum: {},", 0)?;
                write!(f, "\t\talignment_end: ")?;
                writeln!(f, "MessageAlignment::with_offset({}, {}),",
                    alignment.align(), alignment.offset())?;
            },
            AlignBody::Size(size) if size.incr() > 0 => {
                writeln!(f, "\tconst SIZE: MessageSize = MessageSize::Variable {{")?;
                writeln!(f, "\t\tminimum: {},", size.base())?;
                write!(f, "\t\talignment_end: ")?;
                writeln!(f, "MessageAlignment::with_offset({}, {}),",
                    size.incr(), size.base())?;
            },
            AlignBody::Size(size) => {
                writeln!(f, "\tconst SIZE: MessageSize = MessageSize::Fixed {{")?;
                writeln!(f, "\t\tsize: {},", size.base())?;
            },
        }
        writeln!(f, "\t}};")?;
        writeln!(f, "}}")?;

        Ok(())
    }
}

impl fmt::Display for EncodedField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\tpub {}: {}", self.name, self.ty)
    }
}

impl fmt::Display for EncodedEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let derive = if self.bitmask {
            ", BitFlags"
        } else {
            ""
        };
        writeln!(f, "
#[derive(Debug{}, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum {} {{", derive, self.name)?;
        for value in &self.values {
            // TODO: consider bitflag types, are these ever mixed? do they ever go over 32bit?
            // TODO: consider union types to allow invalid values to exist too
            // TODO: check if there are ever gaps, otherwise we can use range to validate them all
            // TODO: use a trait
            writeln!(f, "\t{} = {},", value.name, value.value)?;
        }
        writeln!(f, "}}")?;

        if !self.consts.is_empty() {
            writeln!(f, "impl {} {{", self.name)?;
            for expr in &self.consts {
                writeln!(f, "{}", expr)?;
            }
            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

impl fmt::Display for EncodedSwitch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let alignment = &self.alignment;
        assert!(alignment.internal_align <= alignment.begin.align()); // TODO: what is this for?
        if alignment.begin.offset() > 0 {
            eprintln!("WARN: offset alignment for {}", self.name);
        }

        let repr = if self.copy {
            format!("#[repr(C, align({}))]", alignment.begin.align())
        } else {
            "".into()
        };
        let copy = if self.copy {
            ", Copy"
        } else {
            ""
        };
        match self.kind {
            SwitchKind::Case => {
                writeln!(f, "
{}#[derive(Debug{}, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum {} {{", repr, copy, self.name)?;
                for case in &self.cases {
                    for (i, variant) in case.variants.iter().enumerate() {
                        let name = if i == 0 {
                            &case.name
                        } else {
                            &variant.variant
                        };
                        writeln!(f, "\t{}({}),", case.name, case.ty)?;
                    }
                }
                writeln!(f, "}}")
            },
            SwitchKind::BitCase => {
                eprintln!("TODO: impl bitflag thing");
                writeln!(f, "
#[derive(Debug{}, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct {} {{", copy, self.name)?;
                for case in &self.cases {
                    writeln!(f, "\tpub {}: Option<{}>,", case.name, case.ty)?;
                }
                writeln!(f, "}}")
            },
        }
    }
}


impl fmt::Display for EncodedUnion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (kind, copy, repr) = if self.copy {
            (
                "union",
                ", Copy",
                format!("#[repr(C, align({}))]", self.alignment.begin.align()),
            )
        } else {
            (
                "enum",
                "",
                String::new(),
            )
        };
        writeln!(f, "
{}#[derive(Debug{}, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub {} {} {{", repr, copy, kind, self.name)?;
        for field in &self.fields {
            writeln!(f, "\t{},", field)?;
        }
        writeln!(f, "}}")
    }
}

impl fmt::Display for EncodedAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "pub type {} = {};", self.name, self.ty)
    }
}

impl fmt::Display for Encoded {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.enums {
            writeln!(f, "{}", item)?;
        }

        for item in &self.aliases {
            writeln!(f, "{}", item)?;
        }

        for item in &self.structs {
            writeln!(f, "{}", item)?;
        }

        for item in &self.switches {
            writeln!(f, "{}", item)?;
        }

        for item in &self.unions {
            writeln!(f, "{}", item)?;
        }

        Ok(())
    }
}
