use std::io::{self, Write};
use std::rc::Rc;
use std::convert::TryInto;
use xcbgen::defs::{self, Namespace, ComplexAlignment, AlignBody};

//mod formatting;
mod header;
mod deduction;
mod ident;
mod output;
mod util;

pub use util::process_dir;

use header::Header;
use output::*;

pub fn generate<W: Write>(mut w: W, ns: &Rc<Namespace>) -> io::Result<()> {
    let w = &mut w;

    writeln!(w, "use crate::codegen_prelude::*;")?;

    for (_name, def) in ns.type_defs.borrow().iter() {
        writeln!(w, "{}", OutputType {
            def,
        })?;
    }

    let event_defs = ns.event_defs.borrow();
    for (_name, event) in event_defs.iter() {
        match event {
            defs::EventDef::Full(def) => {
                let alignment = def.required_start_align.clone()
                    .unwrap_or(xcbgen::defs::Alignment::new(4, 0));
                let alignment = ComplexAlignment {
                    begin: alignment,
                    body: if def.xge {
                        // TODO: figure out base and incr from fields?
                        AlignBody::Size(xcbgen::defs::VariableSize::new(32, 4))
                    } else {
                        AlignBody::Size(xcbgen::defs::VariableSize::new(32, 0))
                    },
                    internal_align: 1,
                };
                if let Some(docs) = &def.doc {
                    // TODO: fill this in
                }
                let header = header::Header::Event {
                    number: def.number,
                    is_generic: def.xge,
                    is_plain: !def.no_sequence_number,
                };
                let name = format!("{}Event", ident::to_rust_type_name(&def.name));
                let fields = def.fields.borrow();
                let fields: Vec<_> = fields.iter().collect();
                let struct_ = OutputStruct {
                    name: &name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::Struct,
                    variant: None,
                    external_params: &[],
                    alignment,
                    namespace: Rc::downgrade(ns),
                };
                writeln!(w, "{}", struct_)?;
                writeln!(w, "{}", OutputHeader {
                    header,
                    impl_: struct_.impl_(),
                })?;
            },
            defs::EventDef::Copy(def) => {
                let ref_ = &def.ref_;
                let old_event = defs::NamedTypeRef::unresolved(ref_.name().into());
                let ty = OutputNamedTypeRef {
                    ref_: &old_event, // TODO: support the actual ref type?
                    namespace: Rc::downgrade(ns),
                };
                let original = match event_defs.get(ref_.name()) {
                    Some(defs::EventDef::Full(e)) => e,
                    _ => unreachable!(),
                };
                let header = header::Header::Event {
                    number: def.number,
                    is_generic: original.xge,
                    is_plain: !original.no_sequence_number,
                };
                let name = format!("{}Event", ident::to_rust_type_name(&def.name));
                let type_ = defs::FieldValueType {
                    type_: defs::NamedTypeRef::unresolved(format!("{}Event", ty)),
                    value_set: defs::FieldValueSet::None,
                };
                let field = defs::FieldDef::Normal(defs::NormalField { name: Default::default(), type_ });
                let fields = [&field];
                let struct_ = OutputStruct {
                    name: &name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::TupleStruct,
                    variant: None,
                    external_params: &[],
                    alignment: ComplexAlignment::fixed_size(32, 4),
                    namespace: Rc::downgrade(ns),
                };
                writeln!(w, "{}", struct_)?;
                writeln!(w, "{}", OutputHeader {
                    header,
                    impl_: struct_.impl_(),
                })?;
            },
        }
    }

    if !event_defs.is_empty() {
        let mut fields = Vec::new();
        let mut cases = Vec::new();
        let mut opcode_min = u8::max_value();
        let mut opcode_max = 0;
        for (_name, event) in event_defs.iter() {
            let (name, number) = match event.as_event_ref() {
                defs::EventRef::Full(def) => {
                    let def = def.upgrade().unwrap();
                    (def.name.clone(), def.number)
                },
                defs::EventRef::Copy(def) => {
                    let def = def.upgrade().unwrap();
                    (def.name.clone(), def.number)
                },
            };
            if name == "GeGeneric" {
                // this is handled separately and not considered a core event
                continue
            }
            opcode_min = core::cmp::min(opcode_min, number.try_into().unwrap());
            opcode_max = core::cmp::max(opcode_max, number.try_into().unwrap());
            let disambiguated = if event.is_xge() { (number as u32 + 1) << 6 } else { number as u32 };
            fields.push(defs::FieldDef::Normal(defs::NormalField {
                type_: defs::FieldValueType {
                    type_: defs::NamedTypeRef::unresolved(format!("{}Event", ident::to_rust_type_name(&name))),
                    value_set: defs::FieldValueSet::None,
                },
                name: name.clone(),
            }));
            cases.push(defs::SwitchCase {
                name: Some(name),
                required_start_align: None,
                alignment: ComplexAlignment::fixed_size(32, 4).into(),
                external_params: Default::default(),
                fields: Default::default(),
                exprs: vec![
                    defs::Expression::Value(disambiguated),
                ],
            });
        }
        let fields: Vec<_> = fields.iter().collect();
        let opcode_ty = defs::TypeRef::BuiltIn(if ns.ext_info.is_some() {
            defs::BuiltInType::Card32
        } else {
            defs::BuiltInType::Card8
        });
        let external_params = [
            defs::ExternalParam {
                name: "opcode".into(),
                type_: opcode_ty.clone(),
            }
        ];

        let test_fields = [
            defs::FieldDef::Normal(defs::NormalField {
                type_: defs::FieldValueType {
                    type_: defs::NamedTypeRef::resolved(Default::default(), opcode_ty.clone()),
                    value_set: defs::FieldValueSet::None,
                },
                name: "opcode".into(),
            }),
            defs::FieldDef::Normal(defs::NormalField {
                type_: defs::FieldValueType {
                    type_: defs::NamedTypeRef::unresolved("Events".into()),
                    value_set: defs::FieldValueSet::None,
                },
                name: "event".into(),
            }),
        ];
        let test_fields: Vec<_> = test_fields.iter().collect();
        let test_struct = OutputStruct {
            name: "Events",
            fields: &test_fields,
            parent: None,
            kind: ImplKind::Enum {
                repr: None,
            },
            variant: None,
            external_params: &external_params,
            alignment: defs::ComplexAlignment::fixed_size(32, 4).zero_one_or_many().unwrap(), // TODO
            namespace: Rc::downgrade(ns),
        };
        let event_switch = defs::SwitchField {
            name: "event".into(),
            expr: defs::Expression::FieldRef(defs::FieldRefExpr {
                field_name: "opcode".into(),
                resolved: defs::ResolvedFieldRef {
                    field_type: opcode_ty,
                    ref_kind: defs::FieldRefKind::LocalField,
                }.into(),
            }),
            required_start_align: None,
            alignment: ComplexAlignment::fixed_size(32, 4).into(),
            kind: defs::SwitchKind::Case,
            external_params: Default::default(),
            cases,
        };
        let event_switch = OutputSwitch {
            def: &event_switch,
            parent: test_struct,
        };
        let output = OutputStruct {
            name: "Events",
            fields: &fields[..],
            parent: None,
            kind: ImplKind::Enum {
                repr: None,
            },
            variant: Some(&event_switch),
            external_params: &external_params,
            alignment: defs::ComplexAlignment::fixed_size(32, 4).zero_one_or_many().unwrap(), // TODO
            namespace: Rc::downgrade(ns),
        };
        let impl_ = output.impl_();
        writeln!(w, "{}", output)?;
        let header = Header::Events {
            ext: ns.ext_info.as_ref(),
            opcode_min,
            opcode_max,
        };
        let output = OutputHeader {
            header,
            impl_: impl_.clone(),
        };
        writeln!(w, "{}", output)?;
    }

    let error_defs = ns.error_defs.borrow();
    for (_name, error) in error_defs.iter() {
        match error {
            defs::ErrorDef::Full(def) => {
                let alignment = def.required_start_align.clone()
                    .unwrap_or(xcbgen::defs::Alignment::new(4, 0));
                let header = Header::Error {
                    number: def.number.try_into().unwrap(),
                };
                let name = format!("{}Error", ident::to_rust_type_name(&def.name));
                let fields = def.fields.borrow();
                let fields: Vec<_> = fields.iter().collect();
                let struct_ = OutputStruct {
                    name: &name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::Struct,
                    variant: None,
                    external_params: &[],
                    alignment: ComplexAlignment::fixed_size(32, alignment.align()),
                    namespace: Rc::downgrade(ns),
                };
                writeln!(w, "{}", struct_)?;
                writeln!(w, "{}", OutputHeader {
                    header,
                    impl_: struct_.impl_(),
                })?;
            },
            defs::ErrorDef::Copy(def) => {
                let ref_ = &def.ref_;
                let old_def = def.get_original_full_def();
                let old_error = defs::NamedTypeRef::unresolved(ref_.name().into());
                let old_namespace = old_def.namespace.upgrade().unwrap();
                /*let original = match error_defs.get(ref_.name()) {
                    Some(defs::ErrorDef::Full(e)) => e,
                    _ => unreachable!(),
                };*/
                let header = Header::Error {
                    number: def.number.try_into().unwrap(),
                };
                let ref_ = OutputNamespaceRef {
                    name: ref_.name(),
                    namespace: OutputNamespaceHeader {
                        header: &old_namespace.header,
                        current: Rc::downgrade(ns),
                    },
                };
                let type_ = defs::FieldValueType {
                    type_: defs::NamedTypeRef::unresolved(format!("{}Error", ref_)),
                    value_set: defs::FieldValueSet::None,
                };
                let name = format!("{}Error", ident::to_rust_type_name(&def.name));
                let field = defs::FieldDef::Normal(defs::NormalField { name: Default::default(), type_ });
                let fields = [&field];
                let struct_ = OutputStruct {
                    name: &name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::TupleStruct,
                    variant: None,
                    external_params: &[],
                    alignment: ComplexAlignment::fixed_size(32, 4),
                    namespace: Rc::downgrade(ns),
                };
                writeln!(w, "{}", struct_)?;
                writeln!(w, "{}", OutputHeader {
                    header,
                    impl_: struct_.impl_(),
                })?;
            },
        }
    }

    for (_name, def) in ns.request_defs.borrow().iter() {
        let name = format!("{}Request", ident::to_rust_type_name(&def.name));
        let opcode = def.opcode;
        let combine_adjacent = def.combine_adjacent;
        let alignment = def.required_start_align.clone()
            .unwrap_or(xcbgen::defs::Alignment::new(4, 0));
        let reply = match &def.reply {
            Some(reply) => {
                let alignment = reply.required_start_align.clone()
                    .unwrap_or(xcbgen::defs::Alignment::new(4, 0));
                let header = Header::Reply {
                    request: &name,
                };
                let name = format!("{}Reply", ident::to_rust_type_name(&def.name));
                if let Some(docs) = &reply.doc {
                    // TODO: fill this in
                }
                let fields = reply.fields.borrow();
                let fields: Vec<_> = fields.iter().collect();
                let struct_ = OutputStruct {
                    name: &name,
                    fields: &fields[..],
                    parent: None,
                    kind: ImplKind::Struct,
                    variant: None,
                    external_params: &[],
                    alignment: ComplexAlignment::fixed_size(alignment.align(), alignment.align()).zero_one_or_many().unwrap(),
                    namespace: Rc::downgrade(ns),
                };
                writeln!(w, "{}", struct_)?;
                writeln!(w, "{}", OutputHeader {
                    header,
                    impl_: struct_.impl_(),
                })?;
                name
            },
            None => {
                "()".into()
            },
        };
        if let Some(docs) = &def.doc {
            // TODO: fill this in
        }
        let ext = &def.namespace.upgrade().unwrap().ext_info;
        let header = match ext {
            Some(ext) => Header::ExtensionRequest {
                reply: &reply,
                combine_adjacent,
                minor_opcode: opcode,
                ext,
            },
            None => Header::Request {
                reply: &reply,
                combine_adjacent,
                opcode,
            },
        };
        let fields = def.fields.borrow();
        let end_pad = defs::FieldDef::Pad(defs::PadField {
            // all requests must be 4-byte aligned
            kind: defs::PadKind::Align(4),
            serialize: true,
        });
        let fields: Vec<_> = fields.iter()
            .chain(core::iter::once(&end_pad))
            .collect();
        let struct_ = OutputStruct {
            name: &name,
            fields: &fields[..],
            parent: None,
            kind: ImplKind::Struct,
            variant: None,
            external_params: &[],
            alignment: ComplexAlignment::fixed_size(alignment.align(), alignment.align()).zero_one_or_many().unwrap(),
            namespace: Rc::downgrade(ns),
        };
        writeln!(w, "{}", struct_)?;
        writeln!(w, "{}", OutputHeader {
            header,
            impl_: struct_.impl_(),
        })?;
    }

    Ok(())
}

pub fn generate_meta<'a, N: AsRef<Namespace>, W: Write, I: IntoIterator<Item=N>>(mut w: W, ns: I) -> io::Result<()> {
    let ns: Vec<_> = ns.into_iter().collect();
    let w = &mut w;
    writeln!(w, "#[non_exhaustive]")?;
    writeln!(w, "#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]")?;
    writeln!(w, "pub enum ExtensionKind {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let name = match &ns.ext_info {
            None => continue,
            Some(ext) => &ext.name,
        };
        if ns.ext_info.is_some() {
            writeln!(w, "\t#[cfg(feature = \"{}\")]", ns.header)?;
        }
        writeln!(w, "\t{},", ident::ename_to_rust(name))?;
    }
    writeln!(w, "}}")?;

    writeln!(w, "#[non_exhaustive]")?;
    writeln!(w, "#[derive(Debug, Clone)]")?;
    writeln!(w, "pub enum ExtensionEvent {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let (header, name) = match &ns.ext_info {
            None => ("xcore", "Core"),
            Some(ext) => (&ns.header[..], &ext.name[..]),
        };
        let event_defs = ns.event_defs.borrow();
        if !event_defs.is_empty() {
            if ns.ext_info.is_some() {
                writeln!(w, "\t#[cfg(feature = \"{}\")]", header)?;
            }
            writeln!(w, "\t{}({}::Events),", ident::ename_to_rust(name), header)?;
        }
    }
    writeln!(w, "}}")?;

    writeln!(w, "
impl Message for ExtensionEvent {{
    const ALIGNMENT: MessageAlignment = MessageAlignment::new(4);
    const SIZE: MessageSize = MessageSize::new(0);

    fn size(&self) -> usize {{
        match self {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let name = match &ns.ext_info {
            None => "Core",
            Some(ext) => &ext.name,
        };
        let event_defs = ns.event_defs.borrow();
        if !event_defs.is_empty() {
            if ns.ext_info.is_some() {
                writeln!(w, "\t\t\t#[cfg(feature = \"{}\")]", ns.header)?;
            }
            writeln!(w, "\t\t\tExtensionEvent::{}(e) => e.size(),", ident::ename_to_rust(name))?;
        }
    }
    writeln!(w, "
        }}
    }}")?;

    writeln!(w, "
    fn encode<B: bytes::BufMut>(&self, b: &mut B) {{
        match self {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let name = match &ns.ext_info {
            None => "Core",
            Some(ext) => &ext.name,
        };
        let event_defs = ns.event_defs.borrow();
        if !event_defs.is_empty() {
            if ns.ext_info.is_some() {
                writeln!(w, "\t\t\t#[cfg(feature = \"{}\")]", ns.header)?;
            }
            writeln!(w, "\t\t\tExtensionEvent::{}(e) => e.encode(b),", ident::ename_to_rust(name))?;
        }
    }
    writeln!(w, "
        }}
    }}
}}")?;

    writeln!(w, "
impl core::fmt::Display for ExtensionKind {{
    #[allow(unreachable_code, unreachable_patterns)]
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {{
        f.write_str(match self {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let (name, xname) = match &ns.ext_info {
            None => continue,
            Some(ext) => (&ext.name, &ext.xname),
        };
        writeln!(w, "\t\t\t#[cfg(feature = \"{}\")]", ns.header)?;
        writeln!(w, "\t\t\tExtensionKind::{} => \"{}\",", ident::ename_to_rust(name), xname)?;
    }
    writeln!(w, "\t\t\t_ => unsafe {{ core::hint::unreachable_unchecked() }},")?;
    writeln!(w, "
        }})
    }}
}}")?;

    writeln!(w, "
impl FromMessage for ExtensionEvent {{
    type Error = DecodeError;
    type Context = EventCode;

    fn decode<B: bytes::Buf>(code: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {{
        Ok(Some(match code {{
            EventCode::Core {{ code }} => ExtensionEvent::Core(try_decode!(xcore::Events::decode(code, b))),
            #[allow(unused_variables)]
            EventCode::Extension {{ extension, code }} => match extension {{")?;
    for ns in ns.iter() {
        let ns = ns.as_ref();
        let name = match &ns.ext_info {
            None => continue,
            Some(ext) => &ext.name,
        };
        let event_defs = ns.event_defs.borrow();
        if !event_defs.is_empty() {
            if ns.ext_info.is_some() {
                writeln!(w, "\t\t\t\t#[cfg(feature = \"{}\")]", ns.header)?;
            }
            writeln!(w, "\t\t\t\tExtensionKind::{} => ExtensionEvent::{}(try_decode!({}::Events::decode(code.disambiguate(), b))),", ident::ename_to_rust(name), ident::ename_to_rust(name), ns.header)?;
        }
    }
    writeln!(w, "
                _ => return Err(DecodeError::InvalidEnum(0)),
            }}
        }}))
    }}
}}")?;

    Ok(())
}
