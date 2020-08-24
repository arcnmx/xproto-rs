use xcbgen::defs::ExtInfo;

pub enum Header<'a> {
    Request {
        reply: &'a str,
        combine_adjacent: bool,
        opcode: u8,
    },
    ExtensionRequest {
        reply: &'a str,
        combine_adjacent: bool,
        minor_opcode: u8,
        ext: &'a ExtInfo,
    },
    Reply {
        request: &'a str,
    },
    Error {
        number: i8,
    },
    Event {
        number: u16,
        is_plain: bool,
        is_generic: bool,
    },
    Events {
        ext: Option<&'a ExtInfo>,
        opcode_min: u8,
        opcode_max: u8,
    },
}

impl Header<'_> {
    pub fn kind(&self) -> HeaderKind {
        match self {
            Header::Request { .. } => HeaderKind::Request,
            Header::ExtensionRequest { .. } => HeaderKind::ExtensionRequest,
            Header::Reply { .. } => HeaderKind::Reply,
            Header::Error { .. } => HeaderKind::Error,
            Header::Events { .. } => HeaderKind::Events,
            Header::Event { is_generic, is_plain, .. } => match (is_generic, is_plain) {
                (true, _) => HeaderKind::GenericEvent,
                (false, true) => HeaderKind::PlainEvent,
                (false, false) => HeaderKind::Event,
            },
        }
    }
}

pub enum HeaderKind {
    Request,
    ExtensionRequest,
    Reply,
    Event,
    Events,
    PlainEvent,
    GenericEvent,
    Error,
}

impl HeaderKind {
    pub fn has_data(&self) -> bool {
        match self {
            HeaderKind::ExtensionRequest | HeaderKind::GenericEvent | HeaderKind::Error => false,
            _ => true,
        }
    }
}
