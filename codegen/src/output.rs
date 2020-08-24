use std::fmt;
use std::collections::{HashSet, HashMap};
use std::convert::TryInto;
use std::borrow::Cow;
use std::rc::Weak;
use xcbgen::defs::{self, NamedTypeRef, TypeRef, TypeDef, FieldDef, FieldValueSet, EnumValue, Expression, BuiltInType};
use enumflags2::BitFlags;
use super::ident;
use super::header::{Header, HeaderKind};
use super::deduction::*;

const TYPE_FD: &'static str = "crate::fd::Fd";

pub struct OutputEnum<'a> {
    pub def: &'a defs::EnumDef,
}

impl OutputEnum<'_> {
    pub fn is_bitmask(&self) -> bool {
        self.def.items.iter().any(|item| matches!(item.value, EnumValue::Bit(..)))
    }

    pub fn consts(&self) -> Vec<(&defs::EnumItem, u32)> {
        let mut seen = HashSet::new();
        if self.is_bitmask() {
            // TODO: special-case empty values?
            self.def.items.iter().filter_map(|item| match &item.value {
                &EnumValue::Value(v) => Some((item, v)),
                &EnumValue::Bit(..) => None,
            }).collect()
        } else {
            self.def.items.iter().filter_map(|item| match &item.value {
                &EnumValue::Value(v) if !seen.insert(v) => Some((item, v)),
                _ => None,
            }).collect()
        }
    }

    pub fn values(&self) -> Vec<(&defs::EnumItem, u32)> {
        let bitmask = self.is_bitmask();
        let mut seen = HashSet::new();
        self.def.items.iter().filter_map(|item| match &item.value {
            &EnumValue::Value(..) if bitmask => None,
            &EnumValue::Value(v) =>
                Some((item, v)),
            &EnumValue::Bit(shl) =>
                Some((item, 1u32.checked_shl(shl as u32).unwrap())),
        }).filter(|&(_, v)| seen.insert(v))
        .collect()
    }

    pub fn repr(&self) -> BuiltInType {
        let max_value = self.values().into_iter().map(|(_, value)| value).max().unwrap_or(0);
        match max_value {
            0..=1 => BuiltInType::Bool,
            2..=0xff => BuiltInType::Card8,
            0x100..=0xffff => BuiltInType::Card16,
            _ => BuiltInType::Card32,
        }
    }

    pub fn is_complete(&self) -> bool {
        let count = match self.repr() {
            BuiltInType::Bool => 2,
            BuiltInType::Card8 => 0x100,
            BuiltInType::Card16 => 0x1000,
            _ => return false, // 32bit seems unlikely
        };
        self.values().iter().count() == count
    }
}

impl fmt::Display for OutputEnum<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bitmask = self.is_bitmask();
        let is_complete = self.is_complete();
        let typename = ident::to_rust_enum_type_name(&self.def.name);
        let typename = if enum_name_conflicts(&typename, &self.def.namespace.upgrade().unwrap()) {
            format!("{}Enum", typename)
        } else {
            typename
        };
        let repr = self.repr();
        let enum_repr = match repr {
            BuiltInType::Bool => BuiltInType::Card8,
            repr => repr,
        };
        let repr_name = OutputTypeRef::builtin_type(repr);

        let mut derives = ImplDerive::defaults();
        if !is_complete || repr == BuiltInType::Bool {
            derives.remove(ImplDerive::FromBytes);
        }
        derives.remove(ImplDerive::Default);
        if bitmask {
            derives.insert(ImplDerive::BitFlags);
        }
        let impl_ = OutputImpl {
            name: typename.clone().into(),
            kind: ImplKind::Enum { repr: Some(enum_repr) },
            derives: derives.into(),
            generics: Default::default(),
            repr_trans: false,
            alignment: None,
            namespace: self.def.namespace.clone(),
        };
        impl_.item_begin(&mut *f)?;
        for (item, value) in self.values() {
            // TODO: check if there are ever gaps, otherwise we can use range to validate them all
            // TODO: use a trait
            writeln!(f, "\t{} = {},", ident::ename_to_rust(&item.name), value)?;
        }
        impl_.item_end(&mut *f)?;

        let consts = self.consts();
        if !consts.is_empty() {
            impl_.impl_begin(&mut *f)?;
            for (item, value) in consts {
                if bitmask {
                    let ty = defs::FieldValueType {
                        type_: NamedTypeRef::resolved(Default::default(), TypeRef::BuiltIn(repr).into()),
                        value_set: FieldValueSet::Mask(NamedTypeRef::unresolved("Self".into())),
                    };
                    let bittypename = OutputValueType {
                        ref_: &ty,
                        namespace: self.def.namespace.clone(),
                    };

                    // TODO: BitFlags really needs some const fns, or at least ::EMPTY associated const :(
                    writeln!(f, "\tpub fn {}() -> {} {{ enum_backing!(@new {} {}) }}", ident::to_rust_variable_name(&item.name), bittypename, typename, value)?;
                } else {
                    let (value, _) = self.values().into_iter().find(|&(_, v)| v == value).unwrap();
                    writeln!(f, "\tpub const {}: Self = {}::{};", ident::camel_case_to_upper_snake(&item.name), typename, ident::ename_to_rust(&value.name))?;
                };
            }
            impl_.impl_end(&mut *f)?;
        }

        let prefix = if is_complete {
            "@complete"
        } else {
            "@incomplete"
        };
        writeln!(f, "enum_backing! {{ {} {} => {} }}", prefix, typename, repr_name)?;

        if self.values().iter().count() == 1 {
            let &(first, _) = self.values().first().unwrap();
            writeln!(f, "impl Default for {} {{", typename)?;
            writeln!(f, "\tfn default() -> Self {{")?;
            writeln!(f, "\t\tSelf::{}", ident::ename_to_rust(&first.name))?;
            writeln!(f, "\t}}")?;
            writeln!(f, "}}")?;
        }

        if !is_complete {
            impl_.impl_trait_begin(&mut *f, &format!("core::convert::TryFrom<{}>", repr_name), &[])?;
            writeln!(f, "\ttype Error = DecodeError;")?;
            writeln!(f)?;
            writeln!(f, "\tfn try_from(v: {}) -> Result<Self, Self::Error> {{", repr_name)?;
            writeln!(f, "\t\tmatch v {{")?;
            write!(f, "\t\t\t")?;
            for (i, (_, v)) in self.values().into_iter().enumerate() {
                if i > 0 {
                    write!(f, "| ")?;
                }
                match repr {
                    BuiltInType::Bool =>
                        write!(f, "{:?} ", v != 0)?,
                    _ =>
                        write!(f, "{} ", v)?,
                }
            }
            writeln!(f, "=> Ok(unsafe {{ Self::new_unchecked(v) }}),")?;
            writeln!(f, "\t\t\tv => Err(DecodeError::InvalidEnum(crate::conversion::AsPrimitive::as_(v))),")?;
            writeln!(f, "\t\t}}")?;
            writeln!(f, "\t}}")?;
            impl_.impl_end(&mut *f)?;
        }

        if bitmask {
            let reprs_remaining = [BuiltInType::Card8, BuiltInType::Card16, BuiltInType::Card32]
                .iter().cloned().filter(|&v| v != enum_repr);
            for repr in reprs_remaining {
                let repr = OutputTypeRef::builtin_type(repr);
                writeln!(f, "impl crate::enums::WithRepresentation<{}> for enumflags2::BitFlags<{}> {{", repr, typename)?;
                writeln!(f, "\ttype Output = crate::enums::ResizeMask<{}, {}>;", typename, repr)?;
                writeln!(f, "}}")?;
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct OutputNamespaceHeader<'a> {
    pub header: &'a str,
    pub current: Weak<defs::Namespace>,
}

impl<'a> OutputNamespaceHeader<'a> {
    pub fn with_namespace(namespace: &'a defs::Namespace) -> Self {
        Self::new(&namespace.header)
    }

    pub fn from_parts(namespace: &'a defs::Namespace, current: Weak<defs::Namespace>) -> Self {
        Self {
            header: &namespace.header,
            current,
        }
    }

    pub fn new(header: &'a str) -> Self {
        Self {
            header,
            current: Weak::new(),
        }
    }
}

impl fmt::Display for OutputNamespaceHeader<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.current.upgrade().map(|ns| self.header == ns.header).unwrap_or(false) {
            Ok(())
        } else if self.header == "xproto" {
            write!(f, "protocol::xcore::")
        } else {
            write!(f, "protocol::{}::", self.header)
        }
    }
}

pub struct OutputNamespaceRef<'a> {
    pub name: &'a str,
    pub namespace: OutputNamespaceHeader<'a>,
}

impl fmt::Display for OutputNamespaceRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.namespace, self.name)
    }
}

pub struct OutputTypeRef<'a> {
    pub ref_: &'a TypeRef,
    pub namespace: Weak<defs::Namespace>,
}

impl OutputTypeRef<'_> {
    pub fn builtin_type(ty: BuiltInType) -> &'static str {
        match ty {
            BuiltInType::Card8 | BuiltInType::Byte | BuiltInType::Char => "u8",
            BuiltInType::Card16 => "u16",
            BuiltInType::Card32 => "u32",
            BuiltInType::Card64 => "u64",
            BuiltInType::Int8 => "i8",
            BuiltInType::Int16 => "i16",
            BuiltInType::Int32 => "i32",
            BuiltInType::Int64 => "i64",
            BuiltInType::Float => "f32",
            BuiltInType::Double => "f64",
            BuiltInType::Bool => "bool",
            BuiltInType::Void => unimplemented!(),
        }
    }
}

fn enum_name_conflicts(name: &str, namespace: &defs::Namespace) -> bool {
    if name == "Option" {
        return true
    }
    namespace.type_defs.borrow().iter().any(|(_, def)| match def {
        TypeDef::Enum(..) => false,
        TypeDef::Struct(def) => ident::to_rust_type_name(&def.name) == name,
        TypeDef::Union(def) => ident::to_rust_type_name(&def.name) == name,
        TypeDef::EventStruct(def) => ident::to_rust_type_name(&def.name) == name,
        TypeDef::Xid(def) => ident::to_rust_type_name(&def.name) == name,
        TypeDef::XidUnion(def) => ident::to_rust_type_name(&def.name) == name,
        TypeDef::Alias(def) => ident::to_rust_type_name(&def.new_name) == name,
    })
}

impl fmt::Display for OutputTypeRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ref_ {
            &TypeRef::BuiltIn(ty) => f.write_str(Self::builtin_type(ty)),
            TypeRef::Enum(def) => {
                let def = def.upgrade().unwrap();
                let namespace = def.namespace.upgrade().unwrap();
                let header = OutputNamespaceHeader {
                    header: &namespace.header,
                    current: self.namespace.clone(),
                };
                let output = OutputNamespaceRef {
                    name: &ident::to_rust_enum_type_name(&def.name),
                    namespace: header,
                };
                if enum_name_conflicts(&output.name, &namespace) {
                    write!(f, "{}Enum", output)
                } else {
                    write!(f, "{}", output)
                }
            },
            TypeRef::Struct(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
            TypeRef::EventStruct(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
            TypeRef::Union(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
            TypeRef::Alias(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().new_name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
            TypeRef::Xid(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
            TypeRef::XidUnion(def) => write!(f, "{}", OutputNamespaceRef {
                name: &ident::to_rust_type_name(&def.upgrade().unwrap().name),
                namespace: OutputNamespaceHeader::from_parts(
                    &def.upgrade().unwrap().namespace.upgrade().unwrap(),
                    self.namespace.clone(),
                ),
            }),
        }
    }
}

pub struct OutputNamedTypeRef<'a> {
    pub ref_: &'a NamedTypeRef,
    pub namespace: Weak<defs::Namespace>,
}

impl OutputNamedTypeRef<'_> {
    pub fn is_copy_ref(field: &NamedTypeRef) -> BitFlags<ImplDerive> {
        match field.try_get_resolved() {
            Some(TypeRef::BuiltIn(ty)) => match ty {
                BuiltInType::Bool =>
                    ImplDerive::defaults() & !ImplDerive::FromBytes,
                BuiltInType::Float | BuiltInType::Double =>
                    ImplDerive::defaults() & !(ImplDerive::Ord | ImplDerive::Eq | ImplDerive::Hash),
                _ => ImplDerive::defaults(),
            },
            Some(TypeRef::Xid(_)) | Some(TypeRef::XidUnion(_)) => ImplDerive::defaults(),
            Some(TypeRef::Struct(ty)) => {
                let ty = ty.upgrade().unwrap();
                let fields = ty.fields.borrow();
                OutputStruct::is_copy(fields.iter(), ty.alignment.get().unwrap().begin)
            },
            Some(TypeRef::Alias(ty)) => Self::is_copy_ref(&ty.upgrade().unwrap().old_name),
            Some(TypeRef::Enum(_)) => ImplDerive::defaults() & !ImplDerive::Default,
            _ => ImplDerive::conservative(),
        }
    }

    pub fn is_copy(field: &defs::FieldValueType) -> BitFlags<ImplDerive> {
        match field.value_set {
            FieldValueSet::Enum(..) =>
                ImplDerive::defaults() & !(ImplDerive::FromBytes | ImplDerive::Default),
            FieldValueSet::Mask(..) =>
                ImplDerive::defaults() & !(
                    ImplDerive::FromBytes // TODO: only if is_complete
                    //| ImplDerive::Ord | ImplDerive::PartialOrd, // TODO: only if resized
                ),
            _ => Self::is_copy_ref(&field.type_),
        }
    }
}

impl fmt::Display for OutputNamedTypeRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ref_.try_get_resolved() {
            Some(ref_) => write!(f, "{}", OutputTypeRef {
                ref_,
                namespace: self.namespace.clone(),
            }),
            None =>
                write!(f, "{}", self.ref_.name()),
        }
    }
}

pub struct OutputValueType<'a> {
    pub ref_: &'a defs::FieldValueType,
    pub namespace: Weak<defs::Namespace>,
}

impl OutputValueType<'_> {
    pub fn builtin_type(&self) -> Option<BuiltInType> {
        match self.ref_.type_.try_get_resolved() {
            Some(TypeRef::BuiltIn(ty)) => Some(*ty),
            _ => None,
        }
    }
}

impl fmt::Display for OutputValueType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let repr = OutputNamedTypeRef {
            ref_: &self.ref_.type_,
            namespace: self.namespace.clone(),
        };

        match &self.ref_.value_set {
            FieldValueSet::None => write!(f, "{}", repr),
            FieldValueSet::Enum(ref_) => {
                let ty = OutputNamedTypeRef {
                    ref_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "crate::enums::Enum<{}, {}>", ty, repr)
            },
            FieldValueSet::AltEnum(ref_) => {
                let ty = OutputNamedTypeRef {
                    ref_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "crate::enums::AltEnum<{}, {}>", ty, repr)
            },
            FieldValueSet::Mask(ref_) => {
                let ty = OutputNamedTypeRef {
                    ref_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "crate::enums::Mask<{}, {}>", ty, repr)
            },
            FieldValueSet::AltMask(ref_) => {
                let ty = OutputNamedTypeRef {
                    ref_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "crate::enums::AltMask<{}, {}>", ty, repr)
            },
        }
    }
}

pub struct OutputFieldType<'a> {
    pub namespace: Weak<defs::Namespace>,
    pub parent: &'a NamedTypeRef,
    pub def: &'a FieldDef,
}

impl OutputFieldType<'_> {
    pub fn is_field(def: &FieldDef, repr: BitFlags<ImplDerive>) -> bool {
        match def {
            FieldDef::Normal(..) | FieldDef::List(..) | FieldDef::Switch(..) | FieldDef::Fd(..) | FieldDef::FdList(..) => true,
            FieldDef::Pad(..) => repr.intersects(ImplDerive::AsBytes | ImplDerive::FromBytes),
            FieldDef::Expr(..) => false,
            FieldDef::VirtualLen(..) => false,
        }
    }

    pub fn is_copy(field: &FieldDef) -> BitFlags<ImplDerive> {
        match field {
            FieldDef::Normal(field) => OutputNamedTypeRef::is_copy(&field.type_),
            FieldDef::Pad(pad) => match pad.kind {
                defs::PadKind::Bytes(len) => ImplDerive::defaults() & if len > 32 { !ImplDerive::Default } else { BitFlags::all() },
                defs::PadKind::Align(..) => ImplDerive::defaults() & !(ImplDerive::AsBytes | ImplDerive::FromBytes),
            },
            FieldDef::List(list) => if list.has_fixed_length() {
                OutputNamedTypeRef::is_copy(&list.element_type)
            } else {
                // TODO: technically if you have a list with a maximum length you could use a backing array..?
                ((ImplDerive::defaults() & OutputNamedTypeRef::is_copy(&list.element_type)) | ImplDerive::Default) & !(
                    ImplDerive::AsBytes | ImplDerive::FromBytes | ImplDerive::Copy
                )
            },
            FieldDef::Switch(switch) =>
                (switch.cases.iter().fold(ImplDerive::defaults(), |derives, case|
                    derives & OutputStruct::is_copy(case.fields.borrow().iter(), switch.alignment.get().unwrap().begin)
                ) & !(ImplDerive::FromBytes | ImplDerive::AsBytes | ImplDerive::Default)) | match switch.kind {
                    defs::SwitchKind::BitCase => ImplDerive::Default.into(),
                    _ => BitFlags::empty(),
                },
            _ => ImplDerive::conservative(),
        }
    }
}

impl fmt::Display for OutputFieldType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.def {
            FieldDef::Normal(field) => {
                let type_ = OutputValueType {
                    ref_: &field.type_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "{}", type_)?;
            },
            FieldDef::Expr(field) => {
                let type_ = OutputValueType {
                    ref_: &field.type_,
                    namespace: self.namespace.clone(),
                };
                write!(f, "{}", type_)?;
            },
            FieldDef::List(list) => {
                let type_ = OutputValueType {
                    ref_: &list.element_type,
                    namespace: self.namespace.clone(),
                };
                match list.length() {
                    Some(len) =>
                        write!(f, "[{}; {}]", type_, len),
                    None => match list.element_type.type_.try_get_resolved() {
                        Some(TypeRef::BuiltIn(BuiltInType::Char)) =>
                            write!(f, "protocol::XString"),
                        _ => write!(f, "Vec<{}>", type_), // TODO: consider Cow<[T]> or even SmallVec<[T]> when you know the size is limited
                    },
                }?
            },
            FieldDef::Switch(switch) => {
                write!(f, "{}{}", ident::to_rust_type_name(self.parent.name()), ident::to_rust_type_name(&switch.name))?;
            },
            FieldDef::Fd(fd) => {
                write!(f, "{}", TYPE_FD)?;
            },
            FieldDef::Pad(padding) => match padding.kind {
                defs::PadKind::Bytes(len) =>
                    write!(f, "[u8; {}]", len)?,
                _ => unimplemented!(),
            },
            FieldDef::FdList(fds) => {
                match fds.length() {
                    Some(len) =>
                        write!(f, "[{}; {}]", TYPE_FD, len),
                    None =>
                        write!(f, "Vec<{}>", TYPE_FD),
                }?
            },
            _ => unimplemented!(),
        }

        Ok(())
    }
}

pub struct OutputField<'a> {
    pub namespace: Weak<defs::Namespace>,
    pub parent: &'a NamedTypeRef,
    pub def: &'a FieldDef,
    pub kind: ImplKind,
    pub index: usize,
}

impl OutputField<'_> {
    pub fn generic_def(def: &FieldDef, index: usize, namespace: Weak<defs::Namespace>) -> Option<FieldGeneric> {
        match def {
            FieldDef::List(f) if OutputValueType { ref_: &f.element_type, namespace }.builtin_type() == Some(BuiltInType::Void) =>
                Some(FieldGeneric::VoidList),
            /*TODO FieldDef::List(f) =>
                Some(FieldGeneric::List {
                    ty: &f.element_type,
                    index,
                }),*/
            _ => None,
        }
    }

    pub fn generic(&self, index: usize, namespace: Weak<defs::Namespace>) -> Option<FieldGeneric> {
        Self::generic_def(self.def, index, namespace)
    }
}

impl fmt::Display for OutputField<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_ = OutputFieldType {
            namespace: self.namespace.clone(),
            parent: self.parent,
            def: self.def,
        };
        let name = OutputFieldName {
            def: self.def,
            kind: self.kind,
            index: self.index,
        };
        write!(f, "{}: {}", name, type_)
    }
}

pub struct OutputFieldName<'a> {
    pub def: &'a FieldDef,
    pub kind: ImplKind,
    pub index: usize,
}

impl OutputFieldName<'_> {
    pub fn format_ident(&self, name: &str) -> String {
        match self.kind {
            ImplKind::Struct | ImplKind::Union =>
                ident::to_rust_variable_name(name),
            ImplKind::Enum { .. } =>
                ident::ename_to_rust(name),
            ImplKind::TupleStruct =>
                self.index.to_string(),
        }
    }
}

impl fmt::Display for OutputFieldName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.def {
            FieldDef::Normal(field) => {
                write!(f, "{}", self.format_ident(&field.name))?;
            },
            FieldDef::List(list) => {
                write!(f, "{}", self.format_ident(&list.name))?;
            },
            FieldDef::Switch(switch) => {
                write!(f, "{}", self.format_ident(&switch.name))?;
            },
            FieldDef::Fd(fd) => {
                write!(f, "{}", self.format_ident(&fd.name))?;
            },
            FieldDef::Pad(..) => {
                write!(f, "padding{}", self.index)?;
                // TODO: insert if the struct is Copy and POD and stuff
            },
            FieldDef::Expr(..) => (),
            FieldDef::FdList(fds) => {
                write!(f, "{}", self.format_ident(&fds.name))?;
            },
            FieldDef::VirtualLen(..) => (),
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ImplKind {
    Struct,
    TupleStruct,
    Enum {
        repr: Option<BuiltInType>,
    },
    Union,
}

impl fmt::Display for ImplKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            ImplKind::Struct => "struct",
            ImplKind::TupleStruct => "struct",
            ImplKind::Union => "union",
            ImplKind::Enum { .. } => "enum",
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, BitFlags)]
pub enum ImplDerive {
    Debug = 1,
    Copy = 2,
    Clone = 4,
    PartialEq = 8,
    Eq = 0x10,
    PartialOrd = 0x20,
    Ord = 0x40,
    Hash = 0x80,
    AsBytes = 0x100,
    FromBytes = 0x200,
    BitFlags = 0x400,
    Default = 0x800,
}

impl ImplDerive {
    pub fn defaults() -> BitFlags<Self> {
        BitFlags::all() & !Self::BitFlags
    }

    pub fn conservative() -> BitFlags<Self> {
        Self::defaults() & !(
            Self::Copy | Self::PartialOrd | Self::Ord | Self::AsBytes | Self::FromBytes | Self::Default
        )
    }
}

impl fmt::Display for ImplDerive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            ImplDerive::Debug => "Debug",
            ImplDerive::Default => "Default",
            ImplDerive::Copy => "Copy",
            ImplDerive::Clone => "Clone",
            ImplDerive::PartialEq => "PartialEq",
            ImplDerive::Eq => "Eq",
            ImplDerive::PartialOrd => "PartialOrd",
            ImplDerive::Ord => "Ord",
            ImplDerive::Hash => "Hash",
            ImplDerive::AsBytes => "zerocopy::AsBytes",
            ImplDerive::FromBytes => "zerocopy::FromBytes",
            ImplDerive::BitFlags => "enumflags2::BitFlags",
        })
    }
}

#[derive(Clone)]
pub enum ImplBound<'a> {
    Self_(&'a str),
    Fields(&'a str),
    External(&'a str, &'a str),
}

#[derive(Clone)]
pub struct OutputImpl<'a> {
    pub name: Cow<'a, str>,
    pub kind: ImplKind,
    pub derives: BitFlags<ImplDerive>,
    pub repr_trans: bool,
    pub generics: Cow<'a, [FieldGeneric<'a>]>,
    pub alignment: Option<defs::Alignment>,
    pub namespace: Weak<defs::Namespace>,
}

impl OutputImpl<'_> {
    pub fn fmt_derive<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        if !self.derives.is_empty() {
            write!(w, "#[derive(")?;
            for (i, derive) in self.derives.iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "{}", derive)?;
            }
            writeln!(w, ")]")?;
        }

        Ok(())
    }

    pub fn type_name(&self) -> String {
        match self.kind {
            ImplKind::Enum { .. } =>
                ident::to_rust_enum_type_name(&self.name), // TODO: this might be wrong for switches?
            _ =>
                ident::to_rust_type_name(&self.name),
        }
    }

    pub fn fmt_repr<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        let repr_ty = match self.kind {
            ImplKind::Enum { repr } => repr.map(|repr| OutputTypeRef::builtin_type(repr)),
            _ => None,
        };
        let repr_align = None; // TODO
        let repr_c = if self.derives.contains(ImplDerive::AsBytes) && !self.repr_trans && !matches!(self.kind, ImplKind::Enum { .. }) {
            Some("C")
        } else {
            None
        };
        let repr_trans = if self.repr_trans {
            Some("transparent")
        } else {
            None
        };

        let reprs = [repr_ty, repr_trans, repr_align, repr_c];
        let mut reprs = reprs.iter().filter_map(|r| r.as_ref());
        if let Some(repr) = reprs.next() {
            write!(w, "#[repr({}", repr)?;
            for repr in reprs {
                write!(w, ", {}", repr)?;
            }
            writeln!(w, ")]")?;
        }

        Ok(())
    }

    pub fn fmt_item_begin<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        let generics_impl = FieldGeneric::generics_impl(self.generics.iter().copied());
        match self.kind {
            ImplKind::TupleStruct =>
                write!(w, "pub {} {}{}(", self.kind, self.type_name(), generics_impl),
            _ =>
                writeln!(w, "pub {} {}{} {{", self.kind, self.type_name(), generics_impl),
        }?;

        Ok(())
    }

    pub fn fmt_item_end<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        match self.kind {
            ImplKind::TupleStruct =>
                writeln!(w, ");")?,
            _ => writeln!(w, "}}")?,
        }
        if self.derives.contains(ImplDerive::AsBytes) {
            writeln!(w, "impl MessageAsBytes for {} {{ }}", self.type_name())?;
        }
        if self.derives.contains(ImplDerive::FromBytes) {
            writeln!(w, "impl MessageFromBytes for {} {{ }}", self.type_name())?;
        }

        Ok(())
    }

    pub fn fmt_impl_begin<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        let generics_impl = FieldGeneric::generics_impl(self.generics.iter().copied());
        writeln!(w, "impl{} {}{} {{", generics_impl, self.type_name(), generics_impl)?;

        Ok(())
    }

    pub fn fmt_impl_trait_begin<W: fmt::Write>(&self, mut w: W, trait_: &str, bounds: &[ImplBound]) -> fmt::Result {
        let generics_impl = FieldGeneric::generics_impl(self.generics.iter().copied());
        let where_clause = FieldGeneric::generics_where(self.generics.iter().copied(), bounds);
        writeln!(w, "impl{} {} for {}{} {}{{", generics_impl, trait_, self.type_name(), generics_impl, where_clause)?;

        Ok(())
    }

    pub fn fmt_impl_end<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        writeln!(w, "}}")
    }

    pub fn item_begin<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        self.fmt_derive(&mut w)?;
        self.fmt_repr(&mut w)?;

        self.fmt_item_begin(&mut w)?;

        Ok(())
    }

    pub fn item_end<W: fmt::Write>(&self, w: W) -> fmt::Result {
        self.fmt_item_end(w)
    }

    pub fn impl_begin<W: fmt::Write>(&self, mut w: W) -> fmt::Result {
        self.fmt_impl_begin(&mut w)?;

        Ok(())
    }

    pub fn impl_trait_begin<W: fmt::Write>(&self, mut w: W, trait_: &str, bounds: &[ImplBound]) -> fmt::Result {
        self.fmt_impl_trait_begin(&mut w, trait_, bounds)?;

        Ok(())
    }

    pub fn impl_end<W: fmt::Write>(&self, w: W) -> fmt::Result {
        self.fmt_impl_end(w)
    }
}

#[derive(Clone)]
pub struct OutputStruct<'a> {
    pub name: &'a str,
    pub fields: &'a [&'a FieldDef],
    pub parent: Option<&'a OutputStruct<'a>>,
    pub external_params: &'a [defs::ExternalParam],
    pub kind: ImplKind,
    pub variant: Option<&'a OutputSwitch<'a>>,
    pub alignment: defs::ComplexAlignment,
    pub namespace: Weak<defs::Namespace>,
    // derives?
    //def: &'a defs::StructDef,
}

#[derive(Copy, Clone, Debug)]
pub enum FieldGeneric<'a> {
    VoidList,
    List {
        ty: &'a defs::FieldValueType,
        index: usize,
    },
}

impl FieldGeneric<'_> {
    pub fn name(&self) -> String {
        match self {
            FieldGeneric::VoidList => "T".into(),
            FieldGeneric::List { index, .. } => format!("L{}", index),
        }
    }

    pub fn generics_impl<'a, I: IntoIterator<Item=FieldGeneric<'a>>>(generics: I) -> String {
        let mut out = String::new();
        for (i, generic) in generics.into_iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&generic.name());
        }
        if out.is_empty() {
            out
        } else {
            format!("<{}>", out)
        }
    }

    pub fn generics_where<'a, I: IntoIterator<Item=FieldGeneric<'a>>>(generics: I, bounds: &[ImplBound]) -> String {
        let generics: Vec<_> = generics.into_iter().collect();

        let mut out = String::new();
        for bound in bounds {
            if !out.is_empty() {
                out.push_str(", ");
            }

            match bound {
                ImplBound::Self_(bound) => {
                    out.push_str("Self: ");
                    out.push_str(bound);
                },
                ImplBound::Fields(bound) => {
                    for generic in &generics {
                        out.push_str(&generic.name());
                        out.push_str(": ");
                        out.push_str(bound);
                    }
                },
                ImplBound::External(ty, bound) => {
                    out.push_str(ty);
                    out.push_str(": ");
                    out.push_str(bound);
                },
            }
        }
        if out.is_empty() {
            out
        } else {
            format!("where {} ", out)
        }
    }

    pub fn generics_where_decode<'a, I: IntoIterator<Item=FieldGeneric<'a>>>(generics: I) -> Vec<String> {
        unimplemented!()
    }

    pub fn generics_where_encode<'a, I: IntoIterator<Item=FieldGeneric<'a>>>(generics: I) -> Vec<String> {
        unimplemented!()
    }
}

impl OutputStruct<'_> {
    pub fn generics(&self) -> impl Iterator<Item=FieldGeneric> {
        self.fields.iter().enumerate().filter_map(move |(i, def)| OutputField::generic_def(def, i, self.namespace.clone()))
    }

    pub fn generics_impl(&self) -> String {
        FieldGeneric::generics_impl(self.generics())
    }

    pub fn impl_(&self) -> OutputImpl {
        let mut derives = match self.kind {
            ImplKind::Enum { repr: None } => ImplDerive::defaults() & !(
                ImplDerive::AsBytes | ImplDerive::FromBytes | ImplDerive::Default
            ),
            _ => BitFlags::all(),
        } & Self::is_copy(self.fields.iter().copied(), self.alignment.begin);
        if let Some(switch) = self.variant {
            derives = OutputFieldType::is_copy(switch.switch_field());
        }
        match self.kind {
            ImplKind::Enum { repr: None } | ImplKind::Struct | ImplKind::TupleStruct =>
                derives &= !(ImplDerive::PartialOrd | ImplDerive::Ord),
            _ => (),
        }

        OutputImpl {
            name: self.name.into(),
            kind: self.kind,
            derives,
            generics: self.generics().collect(),
            repr_trans: self.fields.iter()
                .filter(|f| match f {
                    defs::FieldDef::Expr(..) | defs::FieldDef::VirtualLen(..) => false,
                    _ => true,
                }).count() == 1,
            alignment: None,
            namespace: self.namespace.clone(),
        }
    }

    pub fn is_copy<'a, F: IntoIterator<Item=&'a FieldDef>>(fields: F, alignment: defs::Alignment) -> BitFlags<ImplDerive> {
        let init = match alignment.offset() {
            0 => ImplDerive::defaults(),
            _ => ImplDerive::defaults() & !(ImplDerive::FromBytes | ImplDerive::AsBytes),
        };
        fields.into_iter()
            .fold(init, |repr, field| repr & OutputFieldType::is_copy(field))
    }

    pub fn params_ref(ty: &TypeRef, namespace: Weak<defs::Namespace>) -> Vec<defs::ParamRefExpr> {
        match ty {
            TypeRef::Struct(def) =>
                Self::params(def.upgrade().unwrap().fields.borrow().iter(), namespace),
            TypeRef::Alias(def) =>
                Self::params_ref(&def.upgrade().unwrap().get_original_type(), namespace),
            _ => Vec::new(),
        }
    }

    pub fn params<'a, F: IntoIterator<Item=&'a FieldDef>>(fields: F, namespace: Weak<defs::Namespace>) -> Vec<defs::ParamRefExpr> {
        fields.into_iter().flat_map(|f| match f {
            /*FieldDef::Normal(field) =>
                field.type_.type_.try_get_resolved()
                    .map(|ty| Self::params_ref(ty)).unwrap_or_default(),*/
            FieldDef::Switch(field) =>
                field.cases.iter().flat_map(|case|
                    Self::params(case.fields.borrow().iter(), namespace.clone()).into_iter()
                    .chain(case.exprs.iter().flat_map(|e| OutputExpr::new(e,
                        ExprContext::new(&[]/*case.fields.borrow()*/, None, namespace.clone())
                    ).params()))
                ).chain(OutputExpr::new(&field.expr, ExprContext::new(&[], None, namespace.clone())).params())
                .collect(),
            FieldDef::Expr(expr) =>
                OutputExpr {
                    expr: &expr.expr,
                    ty: None,
                    context: ExprContext::new(&[], None, namespace.clone()),
                }.params(),
            FieldDef::List(list) => match &list.length_expr {
                /*list.element_type.type_.try_get_resolved()
                    .map(|ty| Self::params_ref(ty)).unwrap_or_default(),*/
                Some(length_expr) => OutputExpr {
                    expr: length_expr,
                    ty: None,
                    context: ExprContext::new(&[], None, namespace.clone()),
                }.params(),
                None => Vec::new(),
            },
            _ => Vec::new(),
        }).collect()
    }
}

impl fmt::Display for OutputStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut switch_fields = Vec::new();
        let deducible = gather_deducible_fields(self.fields);
        let mut special_fields = false;
        let impl_ = self.impl_();
        //let name = ident::to_rust_type_name(self.name);
        let parent = NamedTypeRef::unresolved(impl_.type_name());
        impl_.item_begin(&mut *f)?;
        for (index, &def) in self.fields.iter().enumerate() {
            match def {
                FieldDef::Switch(switch) => {
                    switch_fields.push(switch);
                },
                FieldDef::Expr(..) =>
                    special_fields = true,
                FieldDef::Normal(field) if deducible.contains_key(&field.name) => {
                    assert!(!impl_.derives.contains(ImplDerive::AsBytes));
                    special_fields = true;
                    continue
                },
                _ => (),
            }
            let field = OutputField {
                namespace: self.namespace.clone(),
                parent: &parent,
                def,
                kind: self.kind,
                index,
            };
            if OutputFieldType::is_field(def, impl_.derives) {
                match self.kind {
                    ImplKind::TupleStruct => {
                        let type_ = OutputFieldType {
                            namespace: self.namespace.clone(),
                            parent: &parent,
                            def,
                        };
                        write!(f, "pub {}, ", type_)?;
                    },
                    ImplKind::Enum { .. } => {
                        let type_ = OutputFieldType {
                            namespace: self.namespace.clone(),
                            parent: &parent,
                            def,
                        };
                        let name = OutputFieldName {
                            def,
                            kind: self.kind,
                            index,
                        };
                        writeln!(f, "\t{}({}),", name, type_)?
                    },
                    _ =>
                        writeln!(f, "\tpub {},", field)?,
                }
            }
        }

        impl_.item_end(&mut *f)?;

        let context = ExprContext::new(self.fields, Some(&deducible), self.namespace.clone());

        if special_fields {
            impl_.impl_begin(&mut *f)?;
            for &def in self.fields {
                let expr_store;
                let expr = match def {
                    FieldDef::Normal(field) => match deducible.get(&field.name) {
                        Some(ded) => {
                            expr_store = OutputDeduction {
                                ded,
                            }.to_expr();
                            Some((&expr_store, &field.type_, &field.name, context.to_local()))
                        },
                        _ => None,
                    },
                    FieldDef::Expr(expr) =>
                        Some((&expr.expr, &expr.type_, &expr.name, context.clone())),
                    _ => None,
                };
                if let Some((expr, type_, expr_name, context)) = expr {
                    let ty = OutputFieldType {
                        namespace: self.namespace.clone(),
                        parent: &parent,
                        def,
                    };
                    let output = OutputExpr {
                        expr,
                        context,
                        ty: Some(&type_),
                    };
                    let params = output.params();
                    let is_const = output.is_const();
                    if params.is_empty() && is_const {
                        writeln!(f, "\tpub const {}: {} = {};", ident::camel_case_to_upper_snake(expr_name), ty, output)?;
                    } else {
                        let fn_prefix = if is_const {
                            "const "
                        } else {
                            ""
                        };
                        write!(f, "\tpub {}fn {}(", fn_prefix, ident::to_rust_variable_name(expr_name))?;
                        if !is_const {
                            write!(f, "&self,")?;
                        }
                        for param in params {
                            let param_name = "";
                            let param_ty = "";
                            write!(f, "param_{}: {},", param_name, param_ty)?;
                        }
                        writeln!(f, ") -> {} {{", ty)?;
                        writeln!(f, "\t\t{}", output)?;
                        writeln!(f, "\t}}")?;
                    }
                }
            }
            impl_.impl_end(&mut *f)?;
        }

        writeln!(f, "{}", OutputMessage {
            impl_: impl_,
            fields: self.fields,
            context,
            parent: self.parent.cloned(),
            external_params: self.external_params,
            variant: self.variant,
            alignment: self.alignment,
        })?;

        for field in switch_fields {
            let output = OutputSwitch {
                parent: self.clone(),
                def: field,
            };
            writeln!(f, "{}", output)?;
        }

        Ok(())
    }
}

pub struct OutputMessage<'a> {
    impl_: OutputImpl<'a>,
    fields: &'a [&'a FieldDef],
    context: ExprContext<'a>,
    variant: Option<&'a OutputSwitch<'a>>,
    external_params: &'a [defs::ExternalParam],
    parent: Option<OutputStruct<'a>>,
    alignment: defs::ComplexAlignment,
}

impl fmt::Display for OutputMessage<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.impl_.derives.contains(ImplDerive::AsBytes) {
            self.impl_.impl_trait_begin(&mut *f, "Message", &[ImplBound::Fields("Message")])?;

            writeln!(f, "\tconst ALIGNMENT: MessageAlignment = MessageAlignment::with_offset({}, {});", self.alignment.begin.align(), self.alignment.begin.offset())?;
            let fixed_size = match self.alignment.body {
                defs::AlignBody::Size(sz) if sz.incr() == 0 => {
                    writeln!(f, "\tconst SIZE: MessageSize = MessageSize::new({});", sz.base())?;

                    Some(sz.base())
                },
                defs::AlignBody::Size(sz) => {
                    writeln!(f, "\tconst SIZE: MessageSize = MessageSize::Variable {{")?;
                    writeln!(f, "\t\tminimum: {},", sz.base())?;
                    writeln!(f, "\t\talignment_end: MessageAlignment::new({}),", sz.incr())?;
                    writeln!(f, "\t}};")?;

                    None
                },
                defs::AlignBody::EndAlign(align) => {
                    writeln!(f, "\tconst SIZE: MessageSize = MessageSize::Variable {{")?;
                    writeln!(f, "\t\tminimum: 0,")?;
                    writeln!(f, "\t\talignment_end: MessageAlignment::with_offset({}, {}),", align.align(), align.offset())?;
                    writeln!(f, "\t}};")?;

                    None
                },
            };
            if let Some(size) = fixed_size {
                writeln!(f, "\tfn size(&self) -> usize {{ {} }}", size)?;
            } else {
                writeln!(f, "\tfn size(&self) -> usize {{")?;
                writeln!(f, "\t\tlet size = ")?;
                match self.impl_.kind {
                    ImplKind::Enum { .. } => {
                        writeln!(f, "\t\tmatch self {{")?;
                        for field in self.fields {
                            if let Some(name) = field.name() {
                                writeln!(f, "\t\t\t{}::{}(f) => f.size(),", self.impl_.type_name(), ident::ename_to_rust(name))?;
                            }
                        }
                        writeln!(f, "\t\t}}")?;
                    },
                    _ => for (i, field) in self.fields.iter().enumerate() {
                        write!(f, "\t\t")?;
                        if i > 0 {
                            write!(f, "+ ")?;
                        }
                        match field {
                            FieldDef::Switch(def) => {
                                writeln!(f, "self.{}.size()", ident::to_rust_variable_name(&def.name))?;
                            },
                            FieldDef::Expr(def) => {
                                writeln!(f, "{}.size()", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?;
                            },
                            FieldDef::Normal(def) => {
                                match def.type_.value_set {
                                    FieldValueSet::Mask(..) =>
                                        writeln!(f, "{}.size()", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?,
                                    _ =>
                                        writeln!(f, "{}.size()", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?,
                                }
                            },
                            FieldDef::Pad(def) => match def.kind {
                                defs::PadKind::Bytes(len) => {
                                    writeln!(f, "{}", len)?;
                                },
                                defs::PadKind::Align(alignment) => {
                                    let align1 = alignment - 1;
                                    writeln!(f, "0;\n\t\tlet size = size + ({} - (size + {}) % {})", align1, align1, alignment)?;
                                },
                            },
                            FieldDef::List(def) => {
                                writeln!(f, "self.{}.size()", ident::to_rust_variable_name(&def.name))?;
                            },
                            FieldDef::VirtualLen(..) | FieldDef::Fd(..) | FieldDef::FdList(..) =>
                                writeln!(f, "0")?,
                        }
                    },
                }
                writeln!(f, "\t\t; size")?;
                writeln!(f, "\t}}")?;
            }

            writeln!(f, "\tfn encode<B: bytes::BufMut>(&self, b: &mut B) {{")?;
            writeln!(f, "\t\tlet _before = b.remaining_mut();")?; // TODO: + start offset
            match self.impl_.kind {
                ImplKind::Enum { .. } => {
                    writeln!(f, "\t\tmatch self {{")?;
                    for field in self.fields {
                        if let Some(name) = field.name() {
                            writeln!(f, "\t\t\t{}::{}(f) => f.encode(&mut *b),", self.impl_.type_name(), ident::ename_to_rust(name))?;
                        }
                    }
                    writeln!(f, "\t\t}}")?;
                },
                _ => for field in self.fields {
                    match field {
                        FieldDef::Switch(def) => {
                            writeln!(f, "\t\tself.{}.encode(&mut *b);", ident::to_rust_variable_name(&def.name))?;
                        },
                        FieldDef::Expr(def) => {
                            writeln!(f, "\t\t{}.encode(&mut *b);", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?;
                        },
                        FieldDef::Normal(def) => {
                            match def.type_.value_set {
                                FieldValueSet::Mask(..) =>
                                    writeln!(f, "\t\t{}.encode(&mut *b);", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?,
                                _ =>
                                    writeln!(f, "\t\t{}.encode(&mut *b);", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?,
                            }
                        },
                        FieldDef::Fd(def) => {
                            writeln!(f, "\t\t{}.encode(&mut *b);", self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?
                        },
                        FieldDef::FdList(def) => {
                            writeln!(f, "\t\tself.{}.encode(&mut *b);", ident::to_rust_variable_name(&def.name))?;
                        },
                        FieldDef::Pad(def) => match def.kind {
                            defs::PadKind::Bytes(len) => {
                                writeln!(f, "\t\t[0u8; {}].encode(&mut *b);", len)?;
                            },
                            defs::PadKind::Align(alignment) => {
                                let align1 = alignment - 1;
                                writeln!(f, "\t\t(0..({} - (_before - b.remaining_mut() + {}) % {})).for_each(|_| b.put_u8(0));", align1, align1, alignment)?;
                            },
                        },
                        FieldDef::List(def) => {
                            writeln!(f, "\t\tself.{}.encode(&mut *b);", ident::to_rust_variable_name(&def.name))?;
                        },
                        FieldDef::VirtualLen(..) => (),
                    }
                },
            }
            writeln!(f, "\t}}")?;

            self.impl_.impl_end(&mut *f)?;

            if let Some(size) = fixed_size {
                self.impl_.impl_trait_begin(&mut *f, "FixedSizeMessage", &[])?;
                writeln!(f, "\tconst FIXED_SIZE: usize = {};", size)?;
                self.impl_.impl_end(&mut *f)?;
            }
        }

        if !self.impl_.derives.contains(ImplDerive::FromBytes) {
            use std::fmt::Write;

            let context = self.context.to_decoder();
            let parent = NamedTypeRef::unresolved(self.impl_.type_name());

            self.impl_.impl_trait_begin(&mut *f, "FromMessage", &[ImplBound::Fields("FromMessage")])?;
            writeln!(f, "\ttype Error = DecodeError;")?;
            write!(f, "\ttype Context = ")?;
            let mut context_destructure = String::new();
            if self.external_params.len() != 1 {
                write!(f, "(")?;
                write!(&mut context_destructure, "(")?;
            }
            for (i, param) in self.external_params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                    write!(&mut context_destructure, ", ")?;
                }
                let parent_field = self.parent.as_ref()
                    .and_then(|parent| parent.fields.iter().find(|f| f.name() == Some(&param.name[..])));
                if let Some(parent_field) = parent_field {
                    let parent = self.parent.as_ref().unwrap();
                    let parent_name = defs::NamedTypeRef::unresolved(parent.name.into());
                    let param_ty = OutputFieldType {
                        namespace: self.impl_.namespace.clone(),
                        def: parent_field,
                        parent: &parent_name,
                    };
                    write!(f, "{}", param_ty)?;
                } else {
                    let param_ty = OutputTypeRef {
                        ref_: &param.type_,
                        namespace: self.impl_.namespace.clone(),
                    };
                    write!(f, "{}", param_ty)?;
                }
                write!(&mut context_destructure, "param_{}", ident::to_rust_variable_name(&param.name))?;
            }
            if self.external_params.len() != 1 {
                context_destructure.push(')');
                write!(f, ")")?;
            }
            writeln!(f, ";")?;
            match self.impl_.kind {
                ImplKind::Enum { repr } => {
                    if repr.is_some() {
                        unimplemented!("Decode for {}", self.impl_.name);
                    }
                },
                _ => (),
            }

            writeln!(f, "\tfn decode<B: bytes::Buf>({}: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {{", context_destructure)?;
            writeln!(f, "\t\tlet _before = b.remaining();")?; // TODO: + start offset
            match self.impl_.kind {
                ImplKind::Enum { .. } if self.variant.is_some() => {
                    let switch = self.variant.unwrap();
                    let variant_field = switch.variant().unwrap();
                    let variant = switch.variant_type().unwrap();
                    let param_name = ident::to_rust_variable_name(variant_field.name().unwrap());
                    let get = match variant.get_resolved() {
                        TypeRef::BuiltIn(..) => "",
                        _ => ".get()",
                    };
                    writeln!(f, "\t\tOk(Some(match param_{}{} {{", param_name, get)?;
                    for case in &switch.def.cases {
                        let case_name = OutputSwitch::case_name(case);
                        let variant = OutputSwitch::case_variants(case).next().unwrap();
                        let variant = OutputExpr {
                            ty: None,
                            expr: variant,
                            context: ExprContext::new(&[], None, self.impl_.namespace.clone())
                        };
                        let context_params: Vec<_> = case.external_params.borrow().iter().map(|p|
                            format!("param_{}", ident::to_rust_variable_name(&p.name))
                        ).collect();
                        let context_params = context_params.join(", ");
                        writeln!(f, "\t\t{} => Self::{}(try_decode!(FromMessage::decode(({}), &mut *b))),",
                            variant,
                            ident::ename_to_rust(case_name),
                            context_params)?;
                    }
                    writeln!(f, "\t\t#[allow(unreachable_patterns)]")?;
                    if get.is_empty() {
                        writeln!(f, "\t\t_ => return Err(DecodeError::UnexpectedEnum(param_{}.into())),", param_name)?;
                    } else {
                        writeln!(f, "\t\t_ => return Err(DecodeError::UnexpectedEnum(crate::conversion::CEnum::repr(param_{}.get()).into())),", param_name)?;
                    }
                    writeln!(f, "\t\t}}))")?;
                },
                ImplKind::Enum { .. } => {
                    // TODO: eventstruct
                    writeln!(f, "\t\tunimplemented!()")?;
                },
                ImplKind::TupleStruct => {
                    writeln!(f, "\t\tlet res = try_decode!(FromMessage::decode({}, b));", context_destructure)?;
                    writeln!(f, "\t\tOk(Some(Self(res)))")?;
                },
                ImplKind::Union => {
                    writeln!(f, "\t\tunimplemented!();")?;
                },
                ImplKind::Struct if self.variant.is_some() => {
                    let switch = self.variant.unwrap();
                    let variant_field = switch.variant().unwrap();
                    let switch_expr = OutputExpr {
                        context: context.clone(),
                        expr: &switch.def.expr,
                        ty: Some(variant_field.value_type().unwrap()),
                    };
                    writeln!(f, "\t\tlet variant = {};", switch_expr)?;
                    writeln!(f, "\t\tOk(Some(Self {{")?;
                    for case in &switch.def.cases {
                        let case_name = OutputSwitch::case_name(&case);
                        let context_params: Vec<_> = case.external_params.borrow().iter().map(|p|
                            format!("param_{}", ident::to_rust_variable_name(&p.name))
                        ).collect();
                        let context_params = context_params.join(", ");
                        writeln!(f, "\t\t\t{}: try_decode!(FromMessage::decode((Self::variant_{}(variant), ({})), &mut *b)),", ident::to_rust_variable_name(&case_name), ident::to_rust_variable_name(&case_name), context_params)?;
                    }
                    writeln!(f, "\t\t}}))")?;
                },
                ImplKind::Struct => {
                    for field in self.fields.iter() {
                        let type_ = OutputFieldType {
                            namespace: self.impl_.namespace.clone(),
                            parent: &parent,
                            def: field,
                        };
                        match field {
                            FieldDef::Switch(def) => {
                                let switch_params = def.external_params.borrow();
                                let context_params: Vec<_> = switch_params.iter().map(|p|
                                    if self.fields.iter().any(|f| f.name() == Some(&p.name)) {
                                        ident::to_rust_variable_name(&p.name)
                                    } else {
                                        format!("param_{}", ident::to_rust_variable_name(&p.name))
                                    }
                                ).collect();
                                let context_params = context_params.join(", ");
                                let switch_name = ident::to_rust_variable_name(&def.name);
                                writeln!(f, "\t\tlet {}: {} = try_decode!(FromMessage::decode(({}), &mut *b));", switch_name, type_, context_params)?;
                            },
                            FieldDef::Expr(def) => {
                                let name = ident::to_rust_variable_name(&def.name);
                                writeln!(f, "\t\tlet _{}: {} = try_decode!(FromMessage::decode((), &mut *b));", name, type_)?;
                                let expr = OutputExpr {
                                    expr: &def.expr,
                                    context: context.clone(),
                                    ty: None,
                                };
                                if expr.is_const() {
                                    writeln!(f, "\t\tif _{} != {} {{ return Err(DecodeError::ConstMismatch) }}", name, self.context.format(&def.name, defs::FieldRefKind::LocalField, false))?;
                                }
                            },
                            FieldDef::Normal(def) => {
                                let context_params: Vec<_> = OutputStruct::params_ref(def.type_.type_.get_resolved(), self.impl_.namespace.clone()).iter().map(|p|
                                    if self.fields.iter().any(|f| f.name() == Some(&p.field_name)) {
                                        ident::to_rust_variable_name(&p.field_name)
                                    } else {
                                        format!("param_{}", ident::to_rust_variable_name(&p.field_name))
                                    }
                                ).collect();
                                let context_params = context_params.join(", ");
                                writeln!(f, "\t\tlet {}: {} = try_decode!(FromMessage::decode(({}), &mut *b));", ident::to_rust_variable_name(&def.name), type_, context_params)?;
                            },
                            FieldDef::Fd(def) => {
                                let context_params = ""; // TODO: Fd read context?
                                writeln!(f, "\t\tlet {} = try_decode!(FromMessage::decode(({}), &mut *b));", ident::to_rust_variable_name(&def.name), context_params)?;
                            },
                            FieldDef::FdList(def) => {
                                let context_params = ""; // TODO: Fd read context?
                                let list_name = ident::to_rust_variable_name(&def.name);
                                if def.length().is_some() {
                                    writeln!(f, "\t\tlet {} = try_decode!(FromMessage::decode(({}), &mut *b));", list_name, context_params)?;
                                } else {
                                    let list_len_expr = OutputExpr {
                                        expr: &def.length_expr,
                                        ty: None,
                                        /*ty: Some(OutputFieldType {
                                            parent: &parent,
                                            def: type_,
                                        }),*/
                                        context: context.clone(),
                                    };
                                    writeln!(f, "\t\tlet {}_lenexpr = {};", list_name, list_len_expr)?;
                                    writeln!(f, "\t\tlet {} = try_decode!(FromMessage::decode((Some({}_lenexpr as usize), ({})), &mut *b));", list_name, list_name, context_params)?;
                                }
                            },
                            FieldDef::Pad(def) => match def.kind {
                                defs::PadKind::Bytes(len) => {
                                    writeln!(f, "\t\tb.advance({});", len)?;
                                },
                                defs::PadKind::Align(alignment) => {
                                    let align1 = alignment - 1;
                                    writeln!(f, "\t\tb.advance({} - (_before - b.remaining() + {}) % {});", align1, align1, alignment)?;
                                },
                            },
                            FieldDef::List(def) => {
                                let context_params: Vec<_> = OutputStruct::params_ref(def.element_type.type_.get_resolved(), self.impl_.namespace.clone()).iter().map(|p|
                                    if self.fields.iter().any(|f| f.name() == Some(&p.field_name)) {
                                        ident::to_rust_variable_name(&p.field_name)
                                    } else {
                                        format!("param_{}", ident::to_rust_variable_name(&p.field_name))
                                    }
                                ).collect();
                                let context_params = context_params.join(", ");

                                let list_name = ident::to_rust_variable_name(&def.name);
                                if def.has_fixed_length() {
                                    writeln!(f, "\t\tlet {}: {} = try_decode!(FromMessage::decode(({}), &mut *b));", list_name, type_, context_params)?;
                                } else if let Some(length_expr) = def.length_expr.as_ref() {
                                    let list_len_expr = OutputExpr {
                                        expr: length_expr,
                                        ty: None,
                                        /*ty: Some(OutputFieldType {
                                            parent: &parent,
                                            def: type_,
                                        }),*/
                                        context: context.clone(),
                                    };
                                    writeln!(f, "\t\tlet {}_lenexpr = {};", list_name, list_len_expr)?;
                                    writeln!(f, "\t\tlet {}: {} = try_decode!(FromMessage::decode((Some({}_lenexpr as usize), ({})), &mut *b));", list_name, type_, list_name, context_params)?;
                                } else {
                                    writeln!(f, "\t\tlet {}: {} = try_decode!(FromMessage::decode((None, ({})), &mut *b));", list_name, type_, context_params)?;
                                }
                            },
                            FieldDef::VirtualLen(..) => (),
                        }
                    }

                    let deducible = gather_deducible_fields(self.fields);
                    writeln!(f, "\t\tOk(Some(Self {{")?;
                    for (index, field) in self.fields.iter().enumerate() {
                        let name = OutputFieldName {
                            def: field,
                            kind: self.impl_.kind,
                            index,
                        };
                        if let Some(name) = field.name() {
                            if deducible.contains_key(name) {
                                // TODO: assert or something?
                                continue;
                            }
                        }
                        if OutputFieldType::is_field(field, self.impl_.derives) {
                            match field {
                                FieldDef::Pad(pad) => match pad.kind {
                                    defs::PadKind::Bytes(len) =>
                                        writeln!(f, "\t\t\t{}: [0u8; {}],", name, len),
                                    _ => unimplemented!(),
                                },
                                _ =>
                                    writeln!(f, "\t\t\t{},", name),
                            }?
                        }
                    }
                    writeln!(f, "\t\t}}))")?;
                },
            }
            //writeln!(f, "\t\tunimplemented!()")?;
            writeln!(f, "\t}}")?;
            self.impl_.impl_end(&mut *f)?;
        }

        Ok(())
    }
}

pub struct OutputUnion<'a> {
    pub def: &'a defs::UnionDef,
}

impl fmt::Display for OutputUnion<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

#[derive(Clone)]
pub struct OutputSwitch<'a> {
    pub parent: OutputStruct<'a>,
    pub def: &'a defs::SwitchField,
}

impl OutputSwitch<'_> {
    fn case_name(case: &defs::SwitchCase) -> &str {
        match case.name.as_ref() {
            Some(name) => name,
            None => {
                match case.exprs.iter().next() {
                    Some(defs::Expression::EnumRef(r)) => &r.variant,
                    _ => unimplemented!(),
                }
            },
        }
    }

    fn switch_field(&self) -> &defs::FieldDef {
        self.parent.fields.iter().find(|f| f.name() == Some(&self.def.name[..])).copied()
            .unwrap()
    }

    fn variant(&self) -> Option<&defs::FieldDef> {
        let variant = self.variant_field()?;
        self.parent.fields.iter().find(|f| f.name() == Some(&variant.field_name[..])).copied()
    }

    fn variant_field(&self) -> Option<&defs::FieldRefExpr> {
        let res = match &self.def.expr {
            Expression::FieldRef(field) =>
                Some(field),
            Expression::BinaryOp(field) => match &*field.lhs {
                Expression::FieldRef(field) =>
                    Some(field),
                _ => None,
            },
            _ => None,
        };
        if res.is_none() {
            eprintln!("TODO: unsupported switch variant expr {:?}", self.def.expr);
        }
        res
    }

    fn variant_type(&self) -> Option<&NamedTypeRef> {
        let ty = self.variant()?.value_type()?;
        match &ty.value_set {
            defs::FieldValueSet::Enum(ref_) | defs::FieldValueSet::AltEnum(ref_) =>
                Some(ref_),
            defs::FieldValueSet::Mask(ref_) | defs::FieldValueSet::AltMask(ref_) =>
                Some(ref_),
            defs::FieldValueSet::None => Some(&ty.type_),
        }
    }

    fn case_variants(case: &defs::SwitchCase) -> impl Iterator<Item=&defs::Expression> {
        case.exprs.iter()/*.map(|var| match var {
            Expression::EnumRef(ref_) =>
                ref_,
            e => panic!("unexpected switch expr {:?}", e),
        })*/
    }

    fn is_one_to_one(&self) -> bool {
        self.def.cases.iter().all(|case| case.exprs.len() == 1)
    }
}

impl fmt::Display for OutputSwitch<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = format!("{}{}", ident::to_rust_type_name(self.parent.name), ident::to_rust_type_name(&self.def.name));

        let mut fields = Vec::new();
        for case in &self.def.cases {
            let case_name = Self::case_name(case);
            let case_type = format!("{}{}", name, ident::to_rust_type_name(case_name));
            fields.push(FieldDef::Normal(defs::NormalField {
                type_: defs::FieldValueType {
                    type_: NamedTypeRef::unresolved(match self.def.kind {
                        defs::SwitchKind::BitCase => format!("Option<{}>", case_type),
                        defs::SwitchKind::Case => case_type.clone(),
                    }),
                    value_set: FieldValueSet::None,
                },
                name: match self.def.kind {
                    defs::SwitchKind::BitCase => ident::to_rust_variable_name(case_name),
                    defs::SwitchKind::Case => ident::ename_to_rust(case_name),
                },
            }));
        }
        let fields: Vec<_> = fields.iter().collect();
        let struct_ = OutputStruct {
            name: &name,
            fields: &fields[..],
            parent: Some(&self.parent),
            kind: match self.def.kind {
                defs::SwitchKind::Case => ImplKind::Enum { repr: None },
                defs::SwitchKind::BitCase => ImplKind::Struct,
            },
            variant: Some(self),
            external_params: &self.def.external_params.borrow()[..],
            alignment: self.def.alignment.get().unwrap().clone(),
            namespace: self.parent.namespace.clone(),
        };
        writeln!(f, "{}", struct_)?;

        match self.def.kind {
            defs::SwitchKind::Case => if let Some(variant) = self.variant() {
                assert!(self.is_one_to_one());
                let parent = NamedTypeRef::unresolved(Default::default());
                let variant_field_ty = OutputFieldType {
                    namespace: self.parent.namespace.clone(),
                    parent: &parent,
                    def: variant,
                };
                writeln!(f, "impl {} {{", name)?;
                writeln!(f, "\tpub fn variant(&self) -> {} {{", variant_field_ty)?;
                writeln!(f, "\t\tmatch self {{")?;
                for case in &self.def.cases {
                    let case_name = Self::case_name(case);
                    assert!(case.exprs.len() == 1);
                    let variant = Self::case_variants(case).next().unwrap();
                    let variant = OutputExpr {
                        ty: None,
                        expr: variant,
                        context: ExprContext::new(&[], None, self.parent.namespace.clone()),
                    };
                    writeln!(f, "\t\t\tSelf::{}(..) => {},",
                        ident::ename_to_rust(case_name),
                        variant)?;
                }
                writeln!(f, "\t\t}}.into()")?;
                writeln!(f, "\t}}")?;
                writeln!(f, "}}")?;
            },
            defs::SwitchKind::BitCase => if let Some(variant) = self.variant() {
                let parent = NamedTypeRef::unresolved(Default::default());
                let variant_field_ty = OutputFieldType {
                    namespace: self.parent.namespace.clone(),
                    parent: &parent,
                    def: variant,
                };

                writeln!(f, "impl {} {{", name)?;

                if self.is_one_to_one() {
                    writeln!(f, "\tpub fn variant(&self) -> {} {{", variant_field_ty)?;
                    writeln!(f, "\t\tlet mut res = Default::default();")?;
                    for case in &self.def.cases {
                        let case_name = Self::case_name(case);
                        let variant = Self::case_variants(case).next().unwrap();
                        let variant = OutputExpr {
                            ty: None,
                            expr: variant,
                            context: ExprContext::new(&[], None, self.parent.namespace.clone()),
                        };
                        writeln!(f, "\t\tif self.{}.is_some() {{ res |= {} }}",
                            ident::to_rust_variable_name(case_name),
                            variant)?;
                    }
                    writeln!(f, "\t\tres")?;
                    writeln!(f, "\t}}")?;
                }

                for case in &self.def.cases {
                    let case_name = Self::case_name(case);
                    writeln!(f, "\tfn variant_{}(variant: {}) -> bool {{", ident::to_rust_variable_name(case_name), variant_field_ty)?;
                    write!(f, "\t\t")?;
                    for (i, variant) in Self::case_variants(case).enumerate() {
                        if i > 0 {
                            write!(f, " || ")?;
                        }
                        let variant = OutputExpr {
                            ty: None,
                            expr: variant,
                            context: ExprContext::new(&[], None, self.parent.namespace.clone()),
                        };
                        write!(f, "variant.flags().contains({})",
                            variant)?;
                    }
                    writeln!(f)?;
                    writeln!(f, "\t}}")?;
                }

                writeln!(f, "}}")?;
            },
        }

        for case in &self.def.cases {
            let case_name = Self::case_name(case);
            let case_type = format!("{}{}", name, ident::to_rust_type_name(case_name));
            let fields = case.fields.borrow();
            let fields: Vec<_> = fields.iter().collect();
            let output = OutputStruct {
                name: &case_type,
                fields: &fields[..],
                parent: Some(&self.parent),
                kind: ImplKind::Struct,
                variant: None,
                external_params: &case.external_params.borrow()[..],
                alignment: self.def.alignment.get().unwrap().clone(),
                namespace: self.parent.namespace.clone(),
            };
            writeln!(f, "{}", output)?;
        }

        Ok(())
    }
}

pub struct OutputType<'a> {
    pub def: &'a TypeDef,
}

impl fmt::Display for OutputType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.def {
            TypeDef::Enum(def) => {
                let output = OutputEnum {
                    def
                };
                writeln!(f, "{}", output)?;
            },
            TypeDef::Union(def) => {
                let output = OutputUnion {
                    def
                };
                writeln!(f, "{}", output)?;
            },
            TypeDef::Alias(def) => {
                let old_name = OutputNamedTypeRef {
                    ref_: &def.old_name,
                    namespace: def.namespace.clone(),
                };
                writeln!(f, "pub type {} = {};", ident::to_rust_type_name(&def.new_name), old_name)?;
            },
            TypeDef::Struct(def) => {
                let fields = def.fields.borrow();
                let external_params: Vec<_> = OutputStruct::params(fields.iter(), def.namespace.clone())
                    .into_iter().map(|p| defs::ExternalParam {
                        name: p.field_name,
                        type_: p.type_.get_resolved().clone(),
                    }).collect();
                let fields: Vec<_> = fields.iter().collect();
                let output = OutputStruct {
                    name: &def.name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::Struct,
                    variant: None,
                    external_params: &external_params,
                    alignment: def.alignment.get().unwrap().clone(),
                    namespace: def.namespace.clone(),
                };
                writeln!(f, "{}", output)?;
            },
            TypeDef::EventStruct(def) => {
                let mut fields = Vec::new();
                for allowed in &def.alloweds {
                    let resolved = allowed.resolved.borrow();
                    for event in resolved.iter() {
                        let name = match event {
                            defs::EventRef::Full(def) => def.upgrade().unwrap().name.clone(),
                            defs::EventRef::Copy(def) => def.upgrade().unwrap().name.clone(),
                        };
                        fields.push(FieldDef::Normal(defs::NormalField {
                            type_: defs::FieldValueType {
                                type_: NamedTypeRef::unresolved(format!("{}Event", ident::to_rust_type_name(&name))),
                                value_set: FieldValueSet::None,
                            },
                            name,
                        }));
                    }
                }
                let fields: Vec<_> = fields.iter().collect();
                let output = OutputStruct {
                    name: &def.name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::Enum {
                        repr: None,
                    },
                    variant: None,
                    external_params: &[],
                    alignment: defs::ComplexAlignment::fixed_size(32, 4).zero_one_or_many().unwrap(), // TODO
                    namespace: def.namespace.clone(),
                };
                let impl_ = output.impl_();
                writeln!(f, "{}", output)?;
                for allowed in &def.alloweds {
                    let header = Header::Events {
                        ext: None,
                        opcode_min: allowed.opcode_min.try_into().unwrap(),
                        opcode_max: allowed.opcode_max.try_into().unwrap(),
                    };
                    let output = OutputHeader {
                        header,
                        impl_: impl_.clone(),
                    };
                    writeln!(f, "{}", output)?;
                }
            },
            TypeDef::Xid(def) => {
                writeln!(f, "xid! {{ {} }}", ident::to_rust_type_name(&def.name))?;
            },
            TypeDef::XidUnion(def) => {
                let mut seen = HashSet::new();
                let typename = ident::to_rust_type_name(&def.name);
                writeln!(f, "xid_union! {{ {} {}Kind =>", typename, typename)?;
                for ref_ in &def.types {
                    let output = OutputNamedTypeRef {
                        ref_,
                        namespace: def.namespace.clone(),
                    };
                    let (namespace, refname) = match ref_.get_resolved() {
                        TypeRef::Xid(xid) => {
                            let xid = xid.upgrade().unwrap();
                            (xid.namespace.upgrade(), xid.name.clone())
                        },
                        TypeRef::XidUnion(xid) => {
                            let xid = xid.upgrade().unwrap();
                            (xid.namespace.upgrade(), xid.name.clone())
                        },
                        _ => unimplemented!(),
                    };
                    let refname = if !seen.insert(refname.clone()) {
                        format!("{}_{}", namespace.unwrap().header, refname)
                    } else {
                        refname
                    };
                    writeln!(f, "\t{}:{},", ident::to_rust_variable_name(&refname), output)?;
                }
                writeln!(f, "}}")?;
            },
        }

        Ok(())
    }
}

impl fmt::Display for HeaderKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            HeaderKind::Request => "RequestHeader",
            HeaderKind::ExtensionRequest => "ExtensionRequestHeader",
            HeaderKind::Reply => "ReplyHeader",
            HeaderKind::Event => "EventHeader",
            HeaderKind::Events => unimplemented!(),
            HeaderKind::PlainEvent => "PlainEventHeader",
            HeaderKind::GenericEvent => "GenericEventHeader",
            HeaderKind::Error => "ErrorHeader",
        };
        f.write_str(name)
    }
}

pub struct OutputHeader<'a> {
    pub header: Header<'a>,
    pub impl_: OutputImpl<'a>,
}

impl fmt::Display for OutputHeader<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.header {
            Header::Request { reply, opcode, combine_adjacent } => {
                self.impl_.impl_trait_begin(&mut *f, "protocol::CoreRequest", &[ImplBound::Self_("Message"), ImplBound::External(reply, "FromMessage")])?;
                writeln!(f, "\tconst CORE_INFO: protocol::CoreRequestInfo = protocol::CoreRequestInfo {{")?;
                writeln!(f, "\t\topcode: {},", opcode)?;
                writeln!(f, "\t}};")?;
                self.impl_.impl_end(&mut *f)?;

                self.impl_.impl_trait_begin(&mut *f, "protocol::Request", &[ImplBound::Self_("Message"), ImplBound::External(reply, "FromMessage")])?;
                writeln!(f, "\ttype Reply = {};", reply)?;
                writeln!(f, "\tconst COMBINE_ADJACENT: bool = {:?};", combine_adjacent)?;
                writeln!(f, "\tconst INFO: protocol::RequestInfo = protocol::RequestInfo::Core(<Self as protocol::CoreRequest>::CORE_INFO);")?;
                self.impl_.impl_end(f)?;
            },
            Header::ExtensionRequest { reply, minor_opcode, combine_adjacent, ext } => {
                self.impl_.impl_trait_begin(&mut *f, "protocol::ExtensionRequest", &[ImplBound::Self_("Message"), ImplBound::External(reply, "FromMessage")])?;
                writeln!(f, "\tconst EXTENSION_INFO: protocol::ExtensionRequestInfo = protocol::ExtensionRequestInfo {{")?;
                writeln!(f, "\t\tminor_opcode: {},", minor_opcode)?;
                writeln!(f, "\t\textension: protocol::ExtensionInfo {{")?;
                writeln!(f, "\t\t\txname: std::borrow::Cow::Borrowed(\"{}\"),", ext.xname)?;
                writeln!(f, "\t\t\tname: std::borrow::Cow::Borrowed(\"{}\"),", ext.name)?;
                writeln!(f, "\t\t\tmultiword: {:?},", ext.multiword)?;
                writeln!(f, "\t\t\tmajor_version: {},", ext.major_version)?;
                writeln!(f, "\t\t\tminor_version: {},", ext.minor_version)?;
                writeln!(f, "\t\t\tkind: protocol::ExtensionKind::{},", ident::ename_to_rust(&ext.name))?;
                writeln!(f, "\t\t}},")?;
                writeln!(f, "\t}};")?;
                self.impl_.impl_end(&mut *f)?;

                self.impl_.impl_trait_begin(&mut *f, "protocol::Request", &[ImplBound::Self_("Message"), ImplBound::External(reply, "FromMessage")])?;
                writeln!(f, "\ttype Reply = {};", reply)?;
                writeln!(f, "\tconst COMBINE_ADJACENT: bool = {:?};", combine_adjacent)?;
                writeln!(f, "\tconst INFO: protocol::RequestInfo = protocol::RequestInfo::Extension(<Self as protocol::ExtensionRequest>::EXTENSION_INFO);")?;
                self.impl_.impl_end(f)?;
            },
            Header::Reply { request } => {
                self.impl_.impl_trait_begin(&mut *f, "protocol::Reply", &[ImplBound::Self_("FromMessage")])?;
                //writeln!(f, "\ttype Request = {};", request)?;
                self.impl_.impl_end(f)?;
            },
            Header::Error { number } => {
                self.impl_.impl_trait_begin(&mut *f, "protocol::Error", &[ImplBound::Self_("FromMessage")])?;
                writeln!(f, "\tconst NUMBER: i8 = {};", number)?;
                self.impl_.impl_end(f)?;
            },
            Header::Event { number, is_plain, is_generic } => {
                self.impl_.impl_trait_begin(&mut *f, "protocol::Event", &[ImplBound::Self_("FromMessage")])?;
                writeln!(f, "\tconst NUMBER: u16 = {};", number)?;
                writeln!(f, "\tconst IS_PLAIN: bool = {:?};", is_plain)?;
                writeln!(f, "\tconst IS_GENERIC: bool = {:?};", is_generic)?;
                self.impl_.impl_end(f)?;
            },
            Header::Events { ext, opcode_min, opcode_max } => {
                eprintln!("TODO: EventStruct impl stuff");
            },
        }

        Ok(())
    }
}

pub struct OutputOp<'a, O> {
    op: &'a O,
}

impl fmt::Display for OutputOp<'_, defs::UnaryOperator> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self.op {
            defs::UnaryOperator::Not => "!",
        })
    }
}

impl fmt::Display for OutputOp<'_, defs::BinaryOperator> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self.op {
            defs::BinaryOperator::Or => "|",
            defs::BinaryOperator::Add => "+",
            defs::BinaryOperator::Sub => "-",
            defs::BinaryOperator::Mul => "*",
            defs::BinaryOperator::Div => "/",
            defs::BinaryOperator::And => "&",
            defs::BinaryOperator::Shl => "<<",
        })
    }
}

#[derive(Clone, Default)]
pub struct ExprContext<'a> {
    pub fields: &'a [&'a FieldDef],
    pub deducible: Option<&'a HashMap<String, DeducibleField>>,
    pub namespace: Weak<defs::Namespace>,
    pub is_decoder: bool,
    pub is_local: bool,
}

impl<'a> ExprContext<'a> {
    pub fn new(fields: &'a [&'a FieldDef], deducible: Option<&'a HashMap<String, DeducibleField>>, namespace: Weak<defs::Namespace>) -> Self {
        Self {
            fields,
            deducible,
            namespace,
            is_local: false,
            is_decoder: false,
        }
    }

    pub fn to_decoder(&self) -> Self {
        Self {
            fields: self.fields,
            deducible: self.deducible,
            namespace: self.namespace.clone(),
            is_local: self.is_local,
            is_decoder: true,
        }
    }

    pub fn to_local(&self) -> Self {
        Self {
            fields: self.fields,
            deducible: self.deducible,
            namespace: self.namespace.clone(),
            is_decoder: self.is_decoder,
            is_local: true,
        }
    }

    pub fn is_const(&self, name: &str) -> bool {
        false
    }

    pub fn format(&self, name: &str, kind: defs::FieldRefKind, is_field_ref: bool) -> String {
        let prefix = match kind {
            defs::FieldRefKind::LocalField if self.is_decoder => "",
            defs::FieldRefKind::LocalField | defs::FieldRefKind::ExtParam if self.is_local => "self.",
            defs::FieldRefKind::LocalField => "self.",
            defs::FieldRefKind::ExtParam => "param_",
            defs::FieldRefKind::SumOfRef => "_element.",
        };
        match self.fields.iter().find(|&f| f.name() == Some(name)) {
            Some(&field) => match field {
                FieldDef::VirtualLen(def) =>
                    format!("{}{}.len()", prefix, ident::to_rust_variable_name(&def.list_name)),
                FieldDef::Expr(def) => {
                    let output = OutputExpr {
                        expr: &def.expr,
                        context: self.clone(),
                        ty: None,
                    };
                    let params = output.params();
                    let params_names: Vec<_> = params.iter().map(|p| format!("param_{}", ident::to_rust_variable_name(&p.field_name))).collect();
                    let params_names = params_names.join(", ");
                    let is_const = output.is_const();
                    if params.is_empty() && is_const {
                        format!("Self::{}", ident::camel_case_to_upper_snake(&def.name))
                    } else if is_const {
                        format!("Self::{}({})", ident::to_rust_variable_name(&def.name), params_names)
                    } else if self.is_decoder {
                        format!("_{}", ident::to_rust_variable_name(&def.name))
                    } else {
                        format!("self.{}({})", ident::to_rust_variable_name(&def.name), params_names)
                    }
                },
                FieldDef::Normal(def) if self.deducible.map(|d| d.contains_key(&def.name)).unwrap_or(false) => if self.is_decoder {
                    ident::to_rust_variable_name(&def.name)
                } else {
                    format!("self.{}()", ident::to_rust_variable_name(&def.name))
                },
                /*FieldDef::Normal(def) if is_field_ref && !matches!(def.type_.value_set, FieldValueSet::None) =>
                    format!("{}{}.bits()", prefix, ident::to_rust_variable_name(&def.name)),*/
                FieldDef::List(list) if is_field_ref => // XXX: special case refer to list length by list name
                    format!("{}{}.len()", prefix, ident::to_rust_variable_name(&list.name)),
                FieldDef::FdList(list) if is_field_ref => // XXX: special case refer to list length by list name
                    format!("{}{}.len()", prefix, ident::to_rust_variable_name(&list.name)),
                FieldDef::Switch(switch) if is_field_ref => // XXX: special case refer to list length by list name
                    format!("{}{}.variant()", prefix, ident::to_rust_variable_name(&switch.name)),
                _ => {
                    let name = if name.is_empty() {
                        "0".into()
                    } else {
                        ident::to_rust_variable_name(name)
                    };
                    format!("{}{}", prefix, name)
                },
            },
            None => {
                match kind {
                    defs::FieldRefKind::LocalField =>
                        eprintln!("WARN: expr field {} not found", name),
                    _ => (),
                }
                format!("{}{}", prefix, ident::to_rust_variable_name(name))
            },
        }
    }
}

#[derive(Clone)]
pub struct OutputExpr<'a> {
    expr: &'a defs::Expression,
    context: ExprContext<'a>,
    ty: Option<&'a defs::FieldValueType>,
}

impl<'a> OutputExpr<'a> {
    pub fn new(expr: &'a defs::Expression, context: ExprContext<'a>) -> Self {
        Self {
            expr,
            context,
            ty: None,
        }
    }

    pub fn subexpr<'b>(&self, expr: &'b defs::Expression) -> OutputExpr<'b> where 'a: 'b {
        OutputExpr {
            expr,
            context: self.context.clone(),
            ty: None,
        }
    }

    pub fn params(&self) -> Vec<defs::ParamRefExpr> {
        match self.expr {
            Expression::FieldRef(..) | Expression::SumOf(..) | Expression::EnumRef(..) | Expression::Value(..) | Expression::Bit(..) | Expression::ListElementRef =>
                Default::default(),
            Expression::UnaryOp(op) => self.subexpr(&op.rhs).params(),
            Expression::BinaryOp(op) => {
                let mut params = self.subexpr(&op.lhs).params();
                params.extend(self.subexpr(&op.rhs).params());
                params
            },
            Expression::PopCount(expr) => self.subexpr(expr).params(),
            Expression::ParamRef(param) => vec![param.clone()],
        }
    }

    pub fn is_const(&self) -> bool {
        match self.expr {
            Expression::FieldRef(expr) => /*match expr.resolved.get().map(|r| r.ref_kind) {
                Some(defs::FieldRefKind::LocalField) => false,
                _ => true,
            }*/
                self.context.is_const(&expr.field_name),
            Expression::SumOf(..) => false,
            Expression::EnumRef(..) => true,
            Expression::Value(..) => true,
            Expression::UnaryOp(op) => self.subexpr(&op.rhs).is_const(),
            Expression::BinaryOp(op) => self.subexpr(&op.lhs).is_const() && self.subexpr(&op.rhs).is_const(),
            Expression::PopCount(expr) => self.subexpr(expr).is_const(),
            Expression::ListElementRef => true,
            Expression::ParamRef(..) => true,
            Expression::Bit(..) => true,
        }
    }
}

impl fmt::Display for OutputExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.expr {
            Expression::FieldRef(defs::FieldRefExpr { field_name, resolved }) => {
                let ref_kind = resolved.get().map(|r| r.ref_kind)
                    .unwrap_or(defs::FieldRefKind::LocalField);
                write!(f, "{}", self.context.format(field_name, ref_kind, true))
            },
            Expression::EnumRef(defs::EnumRefExpr { enum_, variant }) =>
                write!(f, "{}::{}", OutputNamedTypeRef { ref_: enum_, namespace: self.context.namespace.clone() }, ident::ename_to_rust(variant)),
            Expression::Value(v) =>
                write!(f, "{}", v),
            Expression::Bit(bit) =>
                write!(f, "{}", (1u32 << bit)),
            Expression::UnaryOp(op) =>
                write!(f, "({}({}))",
                    OutputOp { op: &op.operator },
                    self.subexpr(&op.rhs)),
            Expression::BinaryOp(op) =>
                write!(f, "(crate::conversion::Promoted({}) {} crate::conversion::Promoted({}))",
                    self.subexpr(&op.lhs),
                    OutputOp { op: &op.operator },
                    self.subexpr(&op.rhs)),
            Expression::SumOf(sum) => {
                let ref_kind = sum.resolved_field.get().map(|r| r.ref_kind)
                    .unwrap_or(defs::FieldRefKind::LocalField);
                write!(f, "{}", self.context.format(&sum.field_name, ref_kind, false))?;
                write!(f, ".iter().map(|_element| {}).fold(0, |lhs, rhs| lhs + rhs)",
                    self.subexpr(&sum.operand))
            },
            Expression::PopCount(expr) =>
                write!(f, "({}).count_ones()",
                    self.subexpr(expr)),
            Expression::ListElementRef =>
                write!(f, "_element"),
            Expression::ParamRef(param) =>
                write!(f, "param_{}", ident::to_rust_variable_name(&param.field_name)), // TODO: expr needs to access parent context to format this access properly
        }?;

        // TODO: better handling of type tracking across expressions and operators, fallible conversions...
        match self.ty {
            Some(ty) => match &ty.value_set {
                FieldValueSet::None => match ty.type_.try_get_resolved() {
                    Some(TypeRef::BuiltIn(BuiltInType::Bool)) =>
                        write!(f, " != 0")?,
                    Some(TypeRef::BuiltIn(BuiltInType::Card32)) =>
                        write!(f, " as u32")?,
                    Some(TypeRef::BuiltIn(BuiltInType::Int32)) =>
                        write!(f, " as i32")?,
                    Some(TypeRef::BuiltIn(BuiltInType::Card16)) =>
                        write!(f, " as u16")?,
                    Some(TypeRef::BuiltIn(BuiltInType::Card8)) | Some(TypeRef::BuiltIn(BuiltInType::Byte)) | Some(TypeRef::BuiltIn(BuiltInType::Char)) =>
                        write!(f, " as u8")?,
                    _ => (),
                },
                _ => (),
            },
            None => (),
        }

        Ok(())
    }
}

pub struct OutputDeduction<'a> {
    ded: &'a DeducibleField,
}

impl OutputDeduction<'_> {
    pub fn to_expr(&self) -> defs::Expression {
        match self.ded {
            DeducibleField::LengthOf(list_name, op) => {
                let list_ref = Expression::FieldRef(defs::FieldRefExpr {
                    field_name: list_name.clone(),
                    resolved: Default::default(),
                });
                match op {
                    DeducibleLengthFieldOp::None =>
                        list_ref,
                    DeducibleLengthFieldOp::Mul(value) =>
                        Expression::BinaryOp(defs::BinaryOpExpr {
                            operator: defs::BinaryOperator::Mul,
                            lhs: list_ref.into(),
                            rhs: Expression::Value(*value).into(),
                        }),
                    DeducibleLengthFieldOp::Div(value) =>
                        Expression::BinaryOp(defs::BinaryOpExpr {
                            operator: defs::BinaryOperator::Div,
                            lhs: list_ref.into(),
                            rhs: Expression::Value(*value).into(),
                        }),
                    DeducibleLengthFieldOp::MulDiv(field, value) =>
                        unimplemented!("removed support for {:?}", field),
                        /*Expression::BinaryOp(defs::BinaryOpExpr {
                            operator: defs::BinaryOperator::Div,
                            lhs: Expression::BinaryOp(defs::BinaryOpExpr {
                                operator: defs::BinaryOperator::Mul,
                                lhs: list_ref.into(),
                                rhs: Expression::FieldRef(defs::FieldRefExpr {
                                    field_name: field.clone(),
                                    resolved: Default::default(),
                                }).into(),
                            }).into(),
                            rhs: Expression::Value(*value).into(),
                        }),*/
                }
            },
            DeducibleField::CaseSwitchExpr(switch_name, op) | DeducibleField::BitCaseSwitchExpr(switch_name, op) => {
                let switch_ref = Expression::FieldRef(defs::FieldRefExpr {
                    field_name: switch_name.clone(),
                    resolved: Default::default(),
                });
                match op {
                    DeducibleFieldOp::None =>
                        switch_ref,
                    DeducibleFieldOp::Or(rhs) =>
                        Expression::BinaryOp(defs::BinaryOpExpr {
                            operator: defs::BinaryOperator::Or,
                            lhs: switch_ref.into(),
                            rhs: rhs.clone(),
                        }),
                }
            },
        }
    }
}

impl fmt::Display for OutputDeduction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
