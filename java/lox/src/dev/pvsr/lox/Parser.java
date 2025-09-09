package dev.pvsr.lox;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

class Parser {
  private static class ParseError extends RuntimeException {
  }

  private final List<Token> tokens;
  private int current = 0;

  Parser(List<Token> tokens) {
    this.tokens = tokens;
  }

  List<Stmt> parse() {
    List<Stmt> statements = new ArrayList<>();
    while (!isAtEnd()) {
      statements.add(declaration());
    }

    return statements;
  }

  private Stmt declaration() {
    try {
      if (match(TokenType.FUN))
        return function(FnKind.FUNCTION);
      if (match(TokenType.VAR))
        return varDeclaration();
      return statement();
    } catch (ParseError error) {
      synchronize();
      return null;
    }
  }

  private Stmt statement() {
    if (match(TokenType.FOR))
      return forStatement();
    if (match(TokenType.IF))
      return ifStatement();
    if (match(TokenType.PRINT))
      return printStatement();
    if (match(TokenType.RETURN))
      return returnStatement();
    if (match(TokenType.WHILE))
      return whileStatement();
    if (match(TokenType.LEFT_BRACE))
      return new Stmt.Block(block());
    return expressionStatement();
  }

  private Stmt forStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
    Stmt initializer = match(TokenType.SEMICOLON)
        ? null
        : match(TokenType.VAR)
            ? varDeclaration()
            : expressionStatement();

    Expr condition = check(TokenType.SEMICOLON) ? new Expr.Literal(true) : expression();
    consume(TokenType.SEMICOLON, "Expect ';' after loop condition.");

    Expr increment = check(TokenType.RIGHT_PAREN) ? null : expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

    Stmt body = statement();
    if (increment != null) {
      body = new Stmt.Block(List.of(body, new Stmt.Expression(increment)));
    }
    body = new Stmt.While(condition, body);
    if (initializer != null) {
      body = new Stmt.Block(List.of(initializer, body));
    }
    return body;
  }

  private Stmt ifStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    Expr condition = expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.");
    Stmt thenBranch = statement();
    Stmt elseBranch = match(TokenType.ELSE) ? statement() : null;
    return new Stmt.If(condition, thenBranch, elseBranch);
  }

  private Stmt printStatement() {
    Expr value = expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    return new Stmt.Print(value);
  }

  private Stmt returnStatement() {
    Token keyword = previous();
    Expr value = check(TokenType.SEMICOLON) ? null : expression();
    consume(TokenType.SEMICOLON, "Expect ';' after return value.");
    return new Stmt.Return(keyword, value);
  }

  private Stmt whileStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    Expr condition = expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");
    Stmt body = statement();
    return new Stmt.While(condition, body);
  }

  private Stmt expressionStatement() {
    Expr expr = expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    return new Stmt.Expression(expr);
  }

  enum FnKind {
    FUNCTION, METHOD
  }

  private Stmt.Function function(FnKind kind) {
    String kindName = kind.name().toLowerCase();
    Token name = consume(TokenType.IDENTIFIER, "Expect %s name.".formatted(kindName));
    consume(TokenType.LEFT_PAREN, "Expect '(' after %s name".formatted(kindName));
    List<Token> parameters = new ArrayList<>();
    if (!check(TokenType.RIGHT_PAREN)) {
      do {
        if (parameters.size() >= 255) {
          error(peek(), "Can't have more than 255 parameters.");
        }
        parameters.add(consume(TokenType.IDENTIFIER, "Expect parameter name."));
      } while (match(TokenType.COMMA));
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TokenType.LEFT_BRACE, "Expect '{' before %s body".formatted(kindName));
    return new Stmt.Function(name, parameters, block());
  }

  private List<Stmt> block() {
    List<Stmt> statements = new ArrayList<>();

    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
      statements.add(declaration());
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
    return statements;
  }

  private Stmt varDeclaration() {
    Token name = consume(TokenType.IDENTIFIER, "Expect variable name.");
    Expr initializer = null;
    if (match(TokenType.EQUAL)) {
      initializer = expression();
    }
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
    return new Stmt.Var(name, initializer);
  }

  private Expr expression() {
    return assignment();
  }

  private Expr assignment() {
    Expr expr = or();

    if (match(TokenType.EQUAL)) {
      Token equals = previous();
      Expr value = assignment();

      if (expr instanceof Expr.Variable(Token name)) {
        return new Expr.Assign(name, value);
      }

      error(equals, "Invalid assignment target.");
    }

    return expr;
  }

  private Expr or() {
    Expr expr = and();
    while (match(TokenType.OR)) {
      Token operator = previous();
      Expr right = and();
      expr = new Expr.Logical(expr, operator, right);
    }
    return expr;
  }

  private Expr and() {
    Expr expr = equality();
    while (match(TokenType.AND)) {
      Token operator = previous();
      Expr right = equality();
      expr = new Expr.Logical(expr, operator, right);
    }
    return expr;
  }

  private Function<Supplier<Expr>, Expr> binary(TokenType... types) {
    return next -> {
      Expr expr = next.get();
      while (match(types)) {
        Token operator = previous();
        Expr right = next.get();
        expr = new Expr.Binary(expr, operator, right);
      }
      final var e = expr;
      return e;
    };
  }

  private Expr equality() {
    // this is a bit too clever
    return Stream.of(
        binary(TokenType.SLASH, TokenType.STAR),
        binary(TokenType.MINUS, TokenType.PLUS),
        binary(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL),
        binary(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))
        .reduce((e1, e2) -> e -> e2.apply(() -> e1.apply(e))).get().apply(this::unary);
  }

  // return expr;
  // }

  // private Expr comparison() {
  // Expr expr = term();

  // while (match(
  // TokenType.GREATER,
  // TokenType.GREATER_EQUAL,
  // TokenType.LESS,
  // TokenType.LESS_EQUAL)) {
  // Token operator = previous();
  // Expr right = term();
  // expr = new Expr.Binary(expr, operator, right);
  // }

  // return expr;
  // }

  // private Expr term() {
  // Expr expr = factor();

  // while (match(TokenType.MINUS, TokenType.PLUS)) {
  // Token operator = previous();
  // Expr right = factor();
  // expr = new Expr.Binary(expr, operator, right);
  // }

  // return expr;
  // }

  // private Expr factor() {
  // Expr expr = unary();

  // while (match(TokenType.SLASH, TokenType.STAR)) {
  // Token operator = previous();
  // Expr right = unary();
  // expr = new Expr.Binary(expr, operator, right);
  // }

  // return expr;
  // }

  private Expr unary() {
    if (match(TokenType.BANG, TokenType.MINUS)) {
      Token operator = previous();
      Expr right = unary();
      return new Expr.Unary(operator, right);
    }

    return call();
  }

  private Expr call() {
    Expr expr = primary();
    while (match(TokenType.LEFT_PAREN)) {
      expr = doCall(expr);
    }
    return expr;
  }

  private Expr doCall(Expr callee) {
    List<Expr> arguments = new ArrayList<>();
    if (!check(TokenType.RIGHT_PAREN)) {
      do {
        if (arguments.size() >= 255) {
          error(peek(), "Can't have more than 255 arguments.");
        }
        arguments.add(expression());
      } while (match(TokenType.COMMA));
    }
    Token paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return new Expr.Call(callee, paren, arguments);
  }

  private Expr primary() {
    if (match(TokenType.FALSE))
      return new Expr.Literal(false);
    if (match(TokenType.TRUE))
      return new Expr.Literal(true);
    if (match(TokenType.NIL))
      return new Expr.Literal(null);

    if (match(TokenType.NUMBER, TokenType.STRING)) {
      return new Expr.Literal(previous().literal());
    }

    if (match(TokenType.IDENTIFIER)) {
      return new Expr.Variable(previous());
    }

    if (match(TokenType.LEFT_PAREN)) {
      Expr expr = expression();
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
      return new Expr.Grouping(expr);
    }

    throw error(peek(), "Expect expression");
  }

  private boolean match(TokenType... types) {
    for (TokenType type : types) {
      if (check(type)) {
        advance();
        return true;
      }
    }

    return false;
  }

  private Token consume(TokenType type, String message) {
    if (check(type))
      return advance();
    throw error(peek(), message);
  }

  private boolean check(TokenType type) {
    if (isAtEnd())
      return false;
    return peek().type() == type;
  }

  Token advance() {
    if (!isAtEnd())
      current++;
    return previous();
  }

  boolean isAtEnd() {
    return peek().type() == TokenType.EOF;
  }

  private Token peek() {
    return tokens.get(current);
  }

  private Token previous() {
    return tokens.get(current - 1);
  }

  private ParseError error(Token token, String message) {
    Lox.error(token, message);
    return new ParseError();
  }

  private void synchronize() {
    advance();

    while (!isAtEnd()) {
      if (previous().type() == TokenType.SEMICOLON)
        return;

      switch (peek().type()) {
        case TokenType.CLASS:
        case TokenType.FUN:
        case TokenType.VAR:
        case TokenType.FOR:
        case TokenType.IF:
        case TokenType.WHILE:
        case TokenType.PRINT:
        case TokenType.RETURN:
          return;
        default:
      }

      advance();
    }
  }
}
