pub mod bluetooth_monitor;

use lazy_regex::regex_is_match;
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum UtilError {
    #[error("Invalid MAC address: {0}")]
    InvalidMacAddress(String),
}
type Result<T> = std::result::Result<T, UtilError>;

/// A MAC address
#[derive(Clone)]
pub struct MacAddress(String);

// impl Display for MacAddress {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

// impl std::fmt::Debug for MacAddress {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         Display::fmt(&self, f)
//     }
// }

impl TryFrom<String> for MacAddress {
    type Error = UtilError;

    fn try_from(value: String) -> Result<Self> {
        if regex_is_match!("([[:xdigit:]]{2}:){5}[[:xdigit:]]{2}", &value) {
            Ok(Self(value))
        } else {
            Err(UtilError::InvalidMacAddress(value))
        }
    }
}

impl AsRef<str> for MacAddress {
    fn as_ref(&self) -> &str {
        &self.0
    }
}
