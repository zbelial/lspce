use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::Error as IoError;
use std::num::ParseIntError;
use std::result::Result as RustResult;
use std::str::Utf8Error;

use crate::msg::{Notification, Request};

#[derive(Debug, Clone)]
pub struct ProtocolError(pub(crate) String);

impl std::error::Error for ProtocolError {}

impl fmt::Display for ProtocolError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug)]
pub enum ExtractError<T> {
    /// The extracted message was of a different method than expected.
    MethodMismatch(T),
    /// Failed to deserialize the message.
    JsonError {
        method: String,
        error: serde_json::Error,
    },
}

impl std::error::Error for ExtractError<Request> {}
impl fmt::Display for ExtractError<Request> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(req) => {
                write!(f, "Method mismatch for request '{}'", req.method)
            }
            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid request\nMethod: {method}\n error: {error}",)
            }
        }
    }
}

impl std::error::Error for ExtractError<Notification> {}
impl fmt::Display for ExtractError<Notification> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(req) => {
                write!(f, "Method mismatch for notification '{}'", req.method)
            }
            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid notification\nMethod: {method}\n error: {error}")
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    /// Failed to parse the JSON body.
    Body(serde_json::Error),
    /// Failed to encode the response.
    Encode(IoError),
    /// Failed to parse headers.
    Headers(httparse::Error),
    /// The media type in the `Content-Type` header is invalid.
    InvalidContentType,
    /// The length value in the `Content-Length` header is invalid.
    InvalidContentLength(ParseIntError),
    /// Request lacks the required `Content-Length` header.
    MissingContentLength,
    /// Request contains invalid UTF8.
    Utf8(Utf8Error),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ParseError::Body(ref e) => write!(f, "unable to parse JSON body: {}", e),
            ParseError::Encode(ref e) => write!(f, "failed to encode response: {}", e),
            ParseError::Headers(ref e) => write!(f, "failed to parse headers: {}", e),
            ParseError::InvalidContentType => write!(f, "unable to parse content type"),
            ParseError::InvalidContentLength(ref e) => {
                write!(f, "unable to parse content length: {}", e)
            }
            ParseError::MissingContentLength => {
                write!(f, "missing required `Content-Length` header")
            }
            ParseError::Utf8(ref e) => write!(f, "request contains invalid UTF8: {}", e),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            ParseError::Body(ref e) => Some(e),
            ParseError::Encode(ref e) => Some(e),
            ParseError::InvalidContentLength(ref e) => Some(e),
            ParseError::Utf8(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<serde_json::Error> for ParseError {
    fn from(error: serde_json::Error) -> Self {
        ParseError::Body(error)
    }
}

impl From<IoError> for ParseError {
    fn from(error: IoError) -> Self {
        ParseError::Encode(error)
    }
}

impl From<httparse::Error> for ParseError {
    fn from(error: httparse::Error) -> Self {
        ParseError::Headers(error)
    }
}

impl From<ParseIntError> for ParseError {
    fn from(error: ParseIntError) -> Self {
        ParseError::InvalidContentLength(error)
    }
}

impl From<Utf8Error> for ParseError {
    fn from(error: Utf8Error) -> Self {
        ParseError::Utf8(error)
    }
}
