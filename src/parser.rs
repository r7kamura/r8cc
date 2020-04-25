use std::collections::HashMap;
use std::iter::Peekable;

use crate::tokenizer::{Keyword, Symbol, Token, TokenKind};

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> ParseResult {
    Parser::new(tokens).parse()
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeKind {
    Program {
        function_definitions: Vec<Node>,
        global_variable_declarations: Vec<Node>,
        strings: Vec<String>,
    },
    Integer {
        value: i32,
    },
    String {
        index: usize,
    },
    Reference {
        value: Box<Node>,
    },
    Dereference {
        value: Box<Node>,
    },
    Add {
        left: Box<Node>,
        right: Box<Node>,
    },
    AddPointer {
        left: Box<Node>,
        right: Box<Node>,
    },
    Subtract {
        left: Box<Node>,
        right: Box<Node>,
    },
    SubtractPointer {
        left: Box<Node>,
        right: Box<Node>,
    },
    Assign {
        left: Box<Node>,
        right: Box<Node>,
    },
    LocalVariable {
        local_variable: LocalVariable,
    },
    Return {
        value: Box<Node>,
    },
    If {
        condition: Box<Node>,
        statement_true: Box<Node>,
        statement_false: Option<Box<Node>>,
    },
    While {
        condition: Box<Node>,
        statement: Box<Node>,
    },
    For {
        initialization: Option<Box<Node>>,
        condition: Option<Box<Node>>,
        afterthrough: Option<Box<Node>>,
        statement: Box<Node>,
    },
    Block {
        statements: Vec<Node>,
    },
    LocalVariableDeclaration,
    GlobalVariableDeclaration {
        local_variable: LocalVariable,
    },
    FunctionDefinition {
        name: String,
        local_variables: HashMap<String, LocalVariable>,
        block: Box<Node>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub type_: Type,
}

impl Node {
    fn new_program(
        function_definitions: Vec<Node>,
        global_variable_declarations: Vec<Node>,
        strings: Vec<String>,
    ) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::Program {
                function_definitions,
                global_variable_declarations,
                strings,
            },
        }
    }

    fn new_function_definition(
        name: String,
        local_variables: HashMap<String, LocalVariable>,
        block: Node,
    ) -> Self {
        Self {
            type_: Type::Function,
            kind: NodeKind::FunctionDefinition {
                name,
                local_variables,
                block: Box::new(block),
            },
        }
    }

    fn new_global_variable_declaration(local_variable: LocalVariable) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::GlobalVariableDeclaration { local_variable },
        }
    }

    fn new_block(statements: Vec<Node>) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::Block { statements },
        }
    }

    fn new_for(
        initialization: Option<Node>,
        condition: Option<Node>,
        afterthrough: Option<Node>,
        statement: Node,
    ) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::For {
                initialization: initialization.map(Box::new),
                condition: condition.map(Box::new),
                afterthrough: afterthrough.map(Box::new),
                statement: Box::new(statement),
            },
        }
    }

    fn new_return(value: Node) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::Return {
                value: Box::new(value),
            },
        }
    }

    fn new_if(condition: Node, statement_true: Node) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::If {
                condition: Box::new(condition),
                statement_true: Box::new(statement_true),
                statement_false: None,
            },
        }
    }

    fn new_if_else(condition: Node, statement_true: Node, statement_false: Node) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::If {
                condition: Box::new(condition),
                statement_true: Box::new(statement_true),
                statement_false: Some(Box::new(statement_false)),
            },
        }
    }

    fn new_while(condition: Node, statement: Node) -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::While {
                condition: Box::new(condition),
                statement: Box::new(statement),
            },
        }
    }

    fn new_local_variable_declaration() -> Self {
        Self {
            type_: Type::Void,
            kind: NodeKind::LocalVariableDeclaration,
        }
    }

    fn new_assign(left: Node, right: Node) -> Self {
        Self {
            type_: right.type_.clone(),
            kind: NodeKind::Assign {
                left: Box::new(left),
                right: Box::new(right),
            },
        }
    }

    fn new_add(left: Node, right: Node) -> Result<Self, ParseError> {
        if left.type_.pointable() {
            if right.type_.pointable() {
                Err(ParseError::UnexpectedAddOperand { left, right })
            } else {
                Ok(Self {
                    type_: left.type_.clone(),
                    kind: NodeKind::AddPointer {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                })
            }
        } else if right.type_.pointable() {
            Ok(Self {
                type_: right.type_.clone(),
                kind: NodeKind::AddPointer {
                    left: Box::new(right),
                    right: Box::new(left),
                },
            })
        } else {
            Ok(Self {
                type_: left.type_.clone(),
                kind: NodeKind::Add {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            })
        }
    }

    fn new_subtract(left: Node, right: Node) -> Result<Self, ParseError> {
        if left.type_.pointable() {
            if right.type_.pointable() {
                Err(ParseError::UnexpectedSubtractOperand { left, right })
            } else {
                Ok(Self {
                    type_: left.type_.clone(),
                    kind: NodeKind::SubtractPointer {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                })
            }
        } else if right.type_.pointable() {
            Ok(Self {
                type_: right.type_.clone(),
                kind: NodeKind::SubtractPointer {
                    left: Box::new(right),
                    right: Box::new(left),
                },
            })
        } else {
            Ok(Self {
                type_: left.type_.clone(),
                kind: NodeKind::Subtract {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            })
        }
    }

    fn new_reference(value: Node) -> Self {
        Self {
            type_: Type::Pointer {
                type_: Box::new(value.type_.clone()),
            },
            kind: NodeKind::Reference {
                value: Box::new(value),
            },
        }
    }

    fn new_dereference(value: Node) -> Result<Self, ParseError> {
        if value.type_.pointable() {
            Ok(Self {
                type_: value.type_.clone(),
                kind: NodeKind::Dereference {
                    value: Box::new(value),
                },
            })
        } else {
            Err(ParseError::UnexpectedDereferenceOperand { node: value })
        }
    }

    fn new_local_variable(local_variable: LocalVariable) -> Self {
        Self {
            type_: local_variable.type_.clone(),
            kind: NodeKind::LocalVariable { local_variable },
        }
    }

    fn new_integer(value: i32) -> Self {
        Self {
            type_: Type::Integer,
            kind: NodeKind::Integer { value },
        }
    }

    fn new_string(index: usize) -> Self {
        Self {
            type_: Type::String,
            kind: NodeKind::String { index },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Char,
    Integer,
    Pointer { type_: Box<Type> },
    Array { type_: Box<Type>, length: u32 },
    String,
    Function,
    Void,
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Integer => 8,
            Type::Pointer { .. } | Type::String { .. } | Type::Function => 16,
            Type::Array { type_, length } => type_.size() * length,
        }
    }

    fn pointable(&self) -> bool {
        match self {
            Type::Pointer { .. } | Type::Array { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub offset: u32,
    pub type_: Type,
    pub global: bool,
}

impl LocalVariable {
    fn new_local_variable(name: String, type_: Type, offset: u32) -> Self {
        Self {
            name,
            type_,
            offset,
            global: false,
        }
    }

    fn new_global_variable(name: String, type_: Type, offset: u32) -> Self {
        Self {
            name,
            type_,
            offset,
            global: true,
        }
    }
}

#[derive(Clone)]
struct Environment {
    local_variables: HashMap<String, LocalVariable>,
    offset: u32,
    parent: Option<Box<Environment>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            local_variables: HashMap::new(),
            offset: 0,
            parent: None,
        }
    }

    fn new_with_parent(parent: Environment) -> Self {
        Self {
            local_variables: HashMap::new(),
            offset: 0,
            parent: Some(Box::new(parent)),
        }
    }

    fn add_local_variable(&mut self, name: String, type_: Type) -> LocalVariable {
        self.offset += type_.size();
        let local_variable =
            LocalVariable::new_local_variable(name.clone(), type_.clone(), self.offset);
        self.local_variables
            .insert(name.clone(), local_variable.clone());
        local_variable
    }

    fn add_global_variable(&mut self, name: String, type_: Type) -> LocalVariable {
        let local_variable =
            LocalVariable::new_global_variable(name.clone(), type_.clone(), self.offset);
        self.local_variables
            .insert(name.clone(), local_variable.clone());
        local_variable
    }

    fn find_local_variable_by(&self, name: &str) -> Option<&LocalVariable> {
        let variable = self.local_variables.get(name);
        if variable.is_some() {
            variable
        } else if let Some(environment) = &self.parent {
            environment.find_local_variable_by(name)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    MissingToken(TokenKind),
    UnexpectedToken {
        expected: Option<TokenKind>, // Use this hint field whenever there is one expected TokenKind.
        actual: Token,
    },
    NotIdentifierToken {
        actual: Token,
    },
    NotIntegerToken {
        actual: Token,
    },
    UnexpectedEos,
    UndefinedLocalVariable {
        name: String,
    },
    UnexpectedAddOperand {
        left: Node,
        right: Node,
    },
    UnexpectedSubtractOperand {
        left: Node,
        right: Node,
    },
    UnexpectedDereferenceOperand {
        node: Node,
    },
}

type ParseResult = Result<Node, ParseError>;

struct Parser<T: Iterator<Item = Token>> {
    environment: Environment,
    tokens: Peekable<T>,
    strings: Vec<String>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
            environment: Environment::new(),
            strings: Vec::new(),
        }
    }

    // program
    //   = function_definition_or_global_variable_declaration*
    //
    // function_definition_or_global_variable_declaration
    //   = type identifier "(" ")" block
    //   | type identifier ";"
    //
    // block
    //   = "{" statement* "}"
    //
    // statement
    //   = "return" expression ";"
    //   | "if" "(" expression ")" statement ("else" statement)?
    //   | "while" "(" expression ")" statement
    //   | "for" "(" expression? ";" expression? ";" expression? ")" statement
    //   | type identifier type_postfix ";"
    //   | block
    //   | expression ";"
    //
    // expression
    //   = assign
    //
    // assign
    //   = equality ("=" assign)?
    //
    // equality
    //   = relational ("==" relational | "!=" relational)*
    //
    // relational
    //   = add ("<" add | "<=" add | ">" add | ">=" add)*
    //
    // add
    //   = multiply ("+" multiply | "-" multiply)*
    //
    // multiply
    //   = unary ("*" unary | "/" unary)*
    //
    // unary
    //   = "+" primary
    //   | "-" primary
    //   | "&" unary
    //   | "*" unary
    //   | primary ("[" expression "]")*
    //
    // primary
    //   = number
    //   | string
    //   | identifier
    //   | "(" expression ")"
    //
    // type
    //   = "int" ("*")*
    //
    // type_postfix
    //   = ("[" number "]")*
    fn parse(&mut self) -> ParseResult {
        let mut function_definitions = Vec::new();
        let mut global_variable_declarations = Vec::new();
        while self.tokens.peek().is_some() {
            let node = self.parse_function_definition_or_global_variable_declaration()?;
            match node.kind {
                NodeKind::FunctionDefinition { .. } => {
                    function_definitions.push(node);
                }
                NodeKind::GlobalVariableDeclaration { .. } => {
                    global_variable_declarations.push(node);
                }
                _ => {}
            }
        }
        Ok(Node::new_program(
            function_definitions,
            global_variable_declarations,
            self.strings.clone(),
        ))
    }

    fn parse_function_definition_or_global_variable_declaration(&mut self) -> ParseResult {
        let type_ = self.parse_type()?;
        let identifier = self.consume_token()?;
        if let Token {
            kind: TokenKind::Identifier(name),
            ..
        } = identifier
        {
            if self.has_next_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft)) {
                self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
                self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
                let parent = self.environment.clone();
                self.environment = Environment::new_with_parent(parent.clone());
                let block = self.parse_block()?;
                let result = Ok(Node::new_function_definition(
                    name,
                    self.environment.local_variables.clone(),
                    block,
                ));
                self.environment = parent;
                result
            } else {
                self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
                Ok(Node::new_global_variable_declaration(
                    self.environment.add_global_variable(name, type_),
                ))
            }
        } else {
            Err(ParseError::NotIdentifierToken {
                actual: self.tokens.next().unwrap(),
            })
        }
    }

    fn parse_block(&mut self) -> ParseResult {
        let mut statements = Vec::new();
        self.consume_token_of(TokenKind::Symbol(Symbol::BraceLeft))?;
        while !self.has_next_token_of(TokenKind::Symbol(Symbol::BraceRight)) {
            statements.push(self.parse_statement()?);
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::BraceRight))?;
        Ok(Node::new_block(statements))
    }

    fn parse_statement(&mut self) -> ParseResult {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Keyword(Keyword::For) => self.parse_statement_for(),
                TokenKind::Keyword(Keyword::If) => self.parse_statement_if(),
                TokenKind::Keyword(Keyword::Return) => self.parse_statement_return(),
                TokenKind::Keyword(Keyword::While) => self.parse_statement_while(),
                TokenKind::Keyword(Keyword::Integer) | TokenKind::Keyword(Keyword::Char) => {
                    self.parse_statement_local_variable_declaration()
                }
                TokenKind::Symbol(Symbol::BraceLeft) => self.parse_block(),
                _ => self.parse_statement_expression(),
            }
        } else {
            Err(ParseError::UnexpectedEos)
        }
    }

    fn parse_statement_for(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::For))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let mut initialization = None;
        let mut condition = None;
        let mut afterthrough = None;
        if !self.has_next_token_of(TokenKind::Symbol(Symbol::Semicolon)) {
            initialization = Some(self.parse_expression()?);
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        if !self.has_next_token_of(TokenKind::Symbol(Symbol::Semicolon)) {
            condition = Some(self.parse_expression()?);
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::ParenthesisRight),
                ..
            }) => {}
            _ => {
                afterthrough = Some(self.parse_expression()?);
            }
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        Ok(Node::new_for(
            initialization,
            condition,
            afterthrough,
            self.parse_statement()?,
        ))
    }

    fn parse_statement_return(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::Return))?;
        let expression = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(Node::new_return(expression))
    }

    fn parse_statement_if(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::If))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement_true = self.parse_statement()?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Else),
                ..
            }) => {
                self.consume_token_of(TokenKind::Keyword(Keyword::Else))?;
                Ok(Node::new_if_else(
                    condition,
                    statement_true,
                    self.parse_statement()?,
                ))
            }
            _ => Ok(Node::new_if(condition, statement_true)),
        }
    }

    fn parse_statement_while(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::While))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement = self.parse_statement()?;
        Ok(Node::new_while(condition, statement))
    }

    fn parse_statement_local_variable_declaration(&mut self) -> ParseResult {
        let mut type_ = self.parse_type()?;
        let token = self.consume_token()?;
        match token.kind {
            TokenKind::Identifier(name) => {
                while self.has_next_token_of(TokenKind::Symbol(Symbol::BracketLeft)) {
                    self.consume_token_of(TokenKind::Symbol(Symbol::BracketLeft))?;
                    let token = self.consume_token()?;
                    if let Token {
                        kind: TokenKind::Integer(length),
                        ..
                    } = token
                    {
                        type_ = Type::Array {
                            type_: Box::new(type_),
                            length,
                        };
                    } else {
                        return Err(ParseError::NotIntegerToken { actual: token });
                    }
                    self.consume_token_of(TokenKind::Symbol(Symbol::BracketRight))?;
                }
                self.environment.add_local_variable(name, type_);
                self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
                Ok(Node::new_local_variable_declaration())
            }
            _ => Err(ParseError::NotIdentifierToken { actual: token }),
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.consume_token()?;
        let mut type_ = match token.kind {
            TokenKind::Keyword(Keyword::Integer) => Type::Integer,
            TokenKind::Keyword(Keyword::Char) => Type::Char,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: None,
                    actual: token,
                });
            }
        };
        while self.has_next_token_of(TokenKind::Symbol(Symbol::Asterisk)) {
            type_ = Type::Pointer {
                type_: Box::new(type_),
            };
            self.consume_token()?;
        }
        Ok(type_)
    }

    fn parse_statement_expression(&mut self) -> ParseResult {
        let expression = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(expression)
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> ParseResult {
        let mut node = self.parse_add()?;
        if let Some(token) = self.tokens.peek() {
            match token {
                Token {
                    kind: TokenKind::Symbol(Symbol::Equal),
                    ..
                } => {
                    self.tokens.next();
                    node = Node::new_assign(node, self.parse_assign()?);
                }
                _ => {}
            }
        }
        Ok(node)
    }

    fn parse_add(&mut self) -> ParseResult {
        let mut node = self.parse_multiply()?;
        while let Some(token) = self.tokens.peek() {
            match token {
                Token {
                    kind: TokenKind::Symbol(Symbol::Plus),
                    ..
                } => {
                    self.tokens.next();
                    node = Node::new_add(node, self.parse_multiply()?)?;
                }
                Token {
                    kind: TokenKind::Symbol(Symbol::Minus),
                    ..
                } => {
                    self.tokens.next();
                    node = Node::new_subtract(node, self.parse_multiply()?)?;
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_multiply(&mut self) -> ParseResult {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> ParseResult {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Symbol(Symbol::Minus) => {
                    self.consume_token_of(TokenKind::Symbol(Symbol::Minus))?;
                    Ok(Node::new_subtract(
                        Node::new_integer(0),
                        self.parse_primary()?,
                    )?)
                }
                TokenKind::Symbol(Symbol::Ampersand) => {
                    self.consume_token_of(TokenKind::Symbol(Symbol::Ampersand))?;
                    Ok(Node::new_reference(self.parse_unary()?))
                }
                TokenKind::Symbol(Symbol::Asterisk) => {
                    self.consume_token_of(TokenKind::Symbol(Symbol::Asterisk))?;
                    Ok(Node::new_dereference(self.parse_unary()?)?)
                }
                _ => {
                    let mut node = self.parse_primary()?;
                    while self.has_next_token_of(TokenKind::Symbol(Symbol::BracketLeft)) {
                        self.consume_token_of(TokenKind::Symbol(Symbol::BracketLeft))?;
                        let index_node = self.parse_expression()?;
                        self.consume_token_of(TokenKind::Symbol(Symbol::BracketRight))?;
                        node = Node::new_dereference(Node::new_add(node, index_node)?)?;
                    }
                    Ok(node)
                }
            }
        } else {
            Err(ParseError::UnexpectedEos)
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        let token = self.tokens.next().expect("Expect token.");
        match token {
            Token {
                kind: TokenKind::Integer(value),
                ..
            } => Ok(Node::new_integer(value as i32)),
            Token {
                kind: TokenKind::String(value),
                ..
            } => {
                let index = self.strings.len();
                self.strings.push(value);
                Ok(Node::new_string(index))
            }
            Token {
                kind: TokenKind::Identifier(name),
                ..
            } => {
                if let Some(local_variable) = self.environment.find_local_variable_by(&name) {
                    Ok(Node::new_local_variable(local_variable.clone()))
                } else {
                    Err(ParseError::UndefinedLocalVariable { name })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                actual: token,
            }),
        }
    }

    fn consume_token(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.next() {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedEos)
        }
    }

    fn consume_token_of(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: Some(kind),
                    actual: token,
                })
            }
        } else {
            Err(ParseError::MissingToken(kind))
        }
    }

    fn has_next_token_of(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.tokens.peek() {
            token.kind == kind
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_parse_undefined_variable() {
        let tokens = tokenize("int main() { return a; }");
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn test_parse_if() {
        let tokens = tokenize("int main() { if (1) { return 2; } else { return 3; } }");
        assert!(parse(tokens).is_ok());
    }

    #[test]
    fn test_parse_while() {
        let tokens = tokenize("int main() { while(1) return 2; }");
        assert!(parse(tokens).is_ok());
    }

    #[test]
    fn test_parse_array() {
        let tokens = tokenize("int main() { int a[3]; a[0] = 1; return a[0]; }");
        assert!(parse(tokens).is_ok());
    }

    #[test]
    fn test_parse_char() {
        let tokens = tokenize("int main() { char a; }");
        let result = parse(tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_global_variable() {
        let tokens = tokenize("int a; int main() { a = 1; return a; }");
        let result = parse(tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_string() {
        let tokens = tokenize("int main() { char *a; a = \"abc\"; return 1; }");
        let result = parse(tokens);
        assert!(result.is_ok());
    }
}
