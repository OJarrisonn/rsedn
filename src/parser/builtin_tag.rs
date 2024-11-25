/// EDN built-in tags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInTag {
    /// Represents a RFC3339 formatted date-time.
    Inst,
    /// Represents a canonical UUID.
    UUID,
}

impl BuiltInTag {
    /// Convert a string to a `BuiltInTag`.
    /// This is case-sensitive.
    /// The string may or not have a leading `#`.
    pub fn from_str(tag: &str) -> Option<Self> {
        let tag = if tag.starts_with('#') { &tag[1..] } else { tag };

        match tag {
            "inst" => Some(Self::Inst),
            "uuid" => Some(Self::UUID),
            _ => None,
        }
    }
}

impl From<BuiltInTag> for &str {
    fn from(val: BuiltInTag) -> Self {
        match val {
            BuiltInTag::Inst => "inst",
            BuiltInTag::UUID => "uuid",
        }
    }
}
