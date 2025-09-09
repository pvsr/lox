package dev.pvsr.lox;

import java.util.List;
import java.util.stream.Collectors;

class AstPrinter {
  String print(Stmt stmt) {
    return switch (stmt) {
      case Stmt.Expression(Expr expression) -> parenthesize(";", expression);
      case Stmt.Print(Expr expression) -> parenthesize("print", expression);
      case Stmt.Return(Token keyword, Expr value) -> parenthesize("print", value);
      case Stmt.Var(Token name, Expr initializer) -> initializer == null
          ? parenthesize2("var", name)
          : parenthesize2("var", name, "=", initializer);
      case Stmt.If(Expr condition, Stmt thenBranch, Stmt elseBranch) -> {
        yield elseBranch == null
            ? parenthesize2("if", condition, thenBranch)
            : parenthesize2("if-else", condition, thenBranch,
                elseBranch);
      }
      case Stmt.Function(Token name, List<Token> params, List<Stmt> body) -> {
        StringBuilder builder = new StringBuilder();
        builder.append("(fun " + name.lexeme() + "(");
        builder.append(params.stream().map(Token::lexeme).collect(Collectors.joining(" ")));
        builder.append(") ");
        body.forEach(this::print);
        body.stream().map(this::print).forEach(builder::append);
        builder.append(")");
        yield builder.toString();
      }
      case Stmt.While(Expr condition, Stmt body) -> parenthesize2("while", condition, body);
      case Stmt.Block(List<Stmt> statements) -> {
        StringBuilder builder = new StringBuilder();
        builder.append("(block ");
        statements.stream().map(this::print).forEach(builder::append);
        builder.append(")");
        yield builder.toString();
      }
    };
  }

  String print(Expr expr) {
    return switch (expr) {
      case Expr.Grouping(Expr expression) ->
        parenthesize("group", expression);
      case Expr.Literal(Object value) ->
        value == null ? "nil" : value.toString();
      case Expr.Unary(Token operator, Expr right) ->
        parenthesize(operator.lexeme(), right);
      case Expr.Binary(Expr left, Token operator, Expr right) ->
        parenthesize(operator.lexeme(), left, right);
      case Expr.Logical(Expr left, Token operator, Expr right) -> parenthesize(operator.lexeme(), left, right);
      case Expr.Call(Expr callee, Token paren, List<Expr> arguments) -> parenthesize2("call", callee, arguments);
      case Expr.Variable(Token name) -> name.lexeme();
      case Expr.Assign(Token name, Expr value) -> parenthesize2("=", name.lexeme(), value);
    };
  }

  private String parenthesize2(String name, Object... parts) {
    StringBuilder builder = new StringBuilder();
    builder.append("(").append(name);
    transform(builder, parts);
    builder.append(")");
    return builder.toString();
  }

  private void transform(StringBuilder builder, Object... parts) {
    for (Object part : parts) {
      if (part instanceof List<?> list) {
        transform(builder, list.toArray());
        continue;
      }
      builder.append(" ");
      builder.append(switch (part) {
        case Expr expr -> print(expr);
        case Stmt stmt -> print(stmt);
        case Token token -> token.lexeme();
        default -> part;
      });
    }
  }

  private String parenthesize(String name, Expr... exprs) {
    StringBuilder builder = new StringBuilder();

    builder.append("(").append(name);
    for (Expr expr : exprs) {
      builder.append(" ");
      builder.append(print(expr));
    }
    builder.append(")");

    return builder.toString();
  }
}
