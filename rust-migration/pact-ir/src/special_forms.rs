//! Special form definitions and recognition
//!
//! This module handles the special syntactic forms in Pact that get
//! transformed into BuiltinForm during desugaring.


/// Special form enumeration - matches Haskell SpecialForm exactly
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecialForm {
    SFAnd,
    SFOr,
    SFIf,
    SFEnforce,
    SFWithCapability,
    SFSuspend,
    SFDo,
    SFEnforceOne,
    SFTry,
    SFMap,
    SFCond,
    SFCreateUserGuard,
    SFPure,
    SFError,
}

impl SpecialForm {
    /// Convert text to special form if it matches
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "and" => Some(Self::SFAnd),
            "or" => Some(Self::SFOr),
            "if" => Some(Self::SFIf),
            "enforce" => Some(Self::SFEnforce),
            "with-capability" => Some(Self::SFWithCapability),
            "suspend" => Some(Self::SFSuspend),
            "do" => Some(Self::SFDo),
            "enforce-one" => Some(Self::SFEnforceOne),
            "try" => Some(Self::SFTry),
            "map" => Some(Self::SFMap),
            "cond" => Some(Self::SFCond),
            "create-user-guard" => Some(Self::SFCreateUserGuard),
            "pure" => Some(Self::SFPure),
            "error" => Some(Self::SFError),
            _ => None,
        }
    }

    /// Convert special form back to string
    pub fn to_str(self) -> &'static str {
        match self {
            Self::SFAnd => "and",
            Self::SFOr => "or",
            Self::SFIf => "if",
            Self::SFEnforce => "enforce",
            Self::SFWithCapability => "with-capability",
            Self::SFSuspend => "suspend",
            Self::SFDo => "do",
            Self::SFEnforceOne => "enforce-one",
            Self::SFTry => "try",
            Self::SFMap => "map",
            Self::SFCond => "cond",
            Self::SFCreateUserGuard => "create-user-guard",
            Self::SFPure => "pure",
            Self::SFError => "error",
        }
    }

    /// Check if a name is a special form
    pub fn is_special_form(name: &str) -> bool {
        Self::from_str(name).is_some()
    }
}

impl std::fmt::Display for SpecialForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_special_form_recognition() {
        assert_eq!(SpecialForm::from_str("and"), Some(SpecialForm::SFAnd));
        assert_eq!(SpecialForm::from_str("if"), Some(SpecialForm::SFIf));
        assert_eq!(SpecialForm::from_str("with-capability"), Some(SpecialForm::SFWithCapability));
        assert_eq!(SpecialForm::from_str("not-special"), None);
    }

    #[test]
    fn test_special_form_to_string() {
        assert_eq!(SpecialForm::SFAnd.to_str(), "and");
        assert_eq!(SpecialForm::SFIf.to_str(), "if");
        assert_eq!(SpecialForm::SFWithCapability.to_str(), "with-capability");
    }

    #[test]
    fn test_is_special_form() {
        assert!(SpecialForm::is_special_form("and"));
        assert!(SpecialForm::is_special_form("enforce"));
        assert!(!SpecialForm::is_special_form("defun"));
        assert!(!SpecialForm::is_special_form("regular-function"));
    }
}