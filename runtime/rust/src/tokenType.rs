#[derive(Debug, Clone)]
pub struct Token(pub String, pub String);

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.0, self.1)
    }
}