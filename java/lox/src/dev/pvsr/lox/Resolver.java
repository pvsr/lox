package dev.pvsr.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Stack;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

class Resolver {
  private final Interpreter interpreter;
  private final Stack<Map<String, Boolean>> scopes = new Stack<>();
  private FunctionType currentFunction = FunctionType.NONE;

  private enum FunctionType {
    NONE, FUNCTION
  }

  Resolver(Interpreter interpreter) {
    this.interpreter = interpreter;
  }

  public void resolve(Stmt stmt) {
    switch (stmt) {
      case Stmt.Expression(Expr expression) -> resolve(expression);
      case Stmt.Print(Expr expression) -> resolve(expression);
      case Stmt.Return(Token keyword, Expr value) -> {
        if (currentFunction == FunctionType.NONE) {
          Lox.error(keyword, "Can't return from top-level code.");
        }
        if (value != null)
          resolve(value);
      }
      case Stmt.Var(Token name, Expr initializer) -> {
        declare(name);
        if (initializer != null) {
          resolve(initializer);
        }
        define(name);
      }
      case Stmt.If(Expr condition, Stmt thenBranch, Stmt elseBranch) -> {
        resolve(condition);
        resolve(thenBranch);
        if (elseBranch != null)
          resolve(elseBranch);
      }
      case Stmt.Function function -> {
        declare(function.name());
        define(function.name());
        resolveFunction(function, FunctionType.FUNCTION);
      }
      case Stmt.While(Expr condition, Stmt body) -> {
        resolve(condition);
        resolve(body);
      }
      case Stmt.Block(List<Stmt> statements) -> {
        beginScope();
        statements.forEach(this::resolve);
        endScope();
      }
    }
  }

  private void resolve(Expr expr) {
    switch (expr) {
      case Expr.Grouping(Expr expression) -> resolve(expression);
      case Expr.Literal _lit -> {
      }
      case Expr.Unary(Token operator, Expr right) -> resolve(right);
      case Expr.Binary(Expr left, Token operator, Expr right) -> {
        resolve(left);
        resolve(right);
      }
      case Expr.Logical(Expr left, Token operator, Expr right) -> {
        resolve(left);
        resolve(right);
      }
      case Expr.Call(Expr callee, Token paren, List<Expr> arguments) -> {
        resolve(callee);
        arguments.forEach(this::resolve);
      }
      case Expr.Variable(Token name) -> {
        if (!scopes.isEmpty() && scopes.peek().get(name.lexeme()) == Boolean.FALSE) {
          Lox.error(name, "Can't read local variable in its own initializer.");
        }
        resolveLocal(expr, name);
      }
      case Expr.Assign(Token name, Expr value) -> {
        resolve(value);
        resolveLocal(expr, name);
      }
    }
  }

  private void resolveFunction(Stmt.Function function, FunctionType type) {
    beginScope();
    FunctionType enclosingFunction = currentFunction;
    currentFunction = type;

    function.params().forEach(param -> {
      declare(param);
      define(param);
    });
    function.body().forEach(this::resolve);
    endScope();
    currentFunction = enclosingFunction;
  }

  private void beginScope() {
    scopes.push(new HashMap<>());
  }

  private void endScope() {
    scopes.pop();
  }

  private void declare(Token name) {
    if (scopes.isEmpty())
      return;
    var scope = scopes.peek();
    if (scope.containsKey(name.lexeme()))
      Lox.error(name, "Already a variable with this name in this scope.");
    scope.put(name.lexeme(), false);
  }

  private void define(Token name) {
    if (!scopes.isEmpty())
      scopes.peek().put(name.lexeme(), true);
  }

  private void resolveLocal(Expr expr, Token name) {
    for (int i = scopes.size() - 1; i >= 0; i--) {
      if (scopes.get(i).containsKey(name.lexeme())) {
        interpreter.resolve(expr, scopes.size() - 1 - i);
        return;
      }
    }
  }
}
