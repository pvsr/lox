package dev.pvsr.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

class Interpreter {
  final Environment globals = new Environment();
  private Environment environment = globals;
  private final Map<Expr, Integer> locals = new HashMap<>();

  Interpreter() {
    globals.define("clock", new LoxCallable.NativeFunction(0, (_i, _args) -> System.currentTimeMillis() / 1000.0));
    globals.define("toString", new LoxCallable.NativeFunction(1, (_i, args) -> args.getFirst().toString()));
  }

  void interpret(List<Stmt> statements) {
    try {
      statements.forEach(this::execute);
    } catch (RuntimeError error) {
      Lox.runtimeError(error);
    }
  }

  private String stringify(Object object) {
    if (object == null)
      return "nil";

    if (object instanceof Double) {
      String text = object.toString();
      if (text.endsWith(".0")) {
        text = text.substring(0, text.length() - 2);
      }
      return text;
    }

    return object.toString();
  }

  private void execute(Stmt stmt) {
    switch (stmt) {
      case Stmt.Print(Expr expr) -> System.out.println(stringify(evaluate(expr)));
      case Stmt.Return(Token keyword, Expr value) -> {
        throw new Return(value == null ? null : evaluate(value));
      }
      case Stmt.Expression(Expr expr) -> evaluate(expr);
      case Stmt.Function function ->
        environment.define(function.name().lexeme(), new LoxCallable.LoxFunction(function, environment));
      case Stmt.If(Expr condition, Stmt thenBranch, Stmt elseBranch) -> {
        if (isTruthy(evaluate(condition))) {
          execute(thenBranch);
        } else if (elseBranch != null) {
          execute(elseBranch);
        }
      }
      case Stmt.Var(Token name, Expr initializer) -> {
        Object value = initializer != null ? evaluate(initializer) : null;
        environment.define(name.lexeme(), value);
      }
      case Stmt.While(Expr condition, Stmt body) -> {
        while (isTruthy(evaluate(condition))) {
          execute(body);
        }
      }
      case Stmt.Block(List<Stmt> statements) -> executeBlock(statements, new Environment(environment));
    }
  }

  void resolve(Expr expr, int depth) {
    locals.put(expr, depth);
  }

  void executeBlock(List<Stmt> statements, Environment environment) {
    Environment previous = this.environment;
    try {
      this.environment = environment;
      statements.forEach(this::execute);
    } finally {
      this.environment = previous;
    }
  }

  private Object evaluate(Expr expr) {
    return switch (expr) {
      case Expr.Literal(Object value) ->
        value;
      case Expr.Grouping(Expr expression) ->
        evaluate(expression);
      case Expr.Unary(Token operator, Expr right) ->
        operate(operator, evaluate(right));
      case Expr.Binary(Expr left, Token operator, Expr right) ->
        operate(evaluate(left), operator, evaluate(right));
      case Expr.Logical(Expr left, Token operator, Expr right) -> {
        Object l = evaluate(left);
        if (operator.type() == TokenType.OR) {
          if (isTruthy(l))
            yield l;
        } else {
          if (!isTruthy(l))
            yield l;
        }
        yield evaluate(right);
      }
      case Expr.Call(Expr callee, Token paren, List<Expr> arguments) -> {
        if (!(evaluate(callee) instanceof LoxCallable function)) {
          throw new RuntimeError(paren, "Can only call functions and classes.");
        }
        if (arguments.size() != function.arity()) {
          throw new RuntimeError(paren, "Expected " +
              function.arity() + " arguments but got " +
              arguments.size() + ".");
        }
        yield function.call(this, arguments.stream().map(this::evaluate).toList());
      }
      case Expr.Variable(Token name) -> lookUpVariable(name, expr);
      case Expr.Assign(Token name, Expr valueExpr) -> {
        Object value = evaluate(valueExpr);
        Integer distance = locals.get(expr);
        if (distance != null)
          environment.assignAt(distance, name, value);
        else
          globals.assign(name, value);
        yield value;
      }
    };
  }

  private Object lookUpVariable(Token name, Expr expr) {
    Integer distance = locals.get(expr);
    return distance != null ? environment.getAt(distance, name.lexeme()) : globals.get(name);
  }

  private Object operate(Token operator, Object value) {
    return (switch (operator.type()) {
      case TokenType.MINUS -> number(value).map(n -> -n);
      case TokenType.BANG -> new Value<>(!isTruthy(value), null);
      default -> new Value<>(null, "unexpected operator");
    }).or(operator);
  }

  private Object operate(Object left, Token operator, Object right) {
    return (switch (operator.type()) {
      case TokenType.PLUS -> (switch (left) {
        case Double d -> numbers(d, right, (l, r) -> l + r);
        case String str -> strings(str, right, (l, r) -> l + r);
        default -> new Value<>(null, null);
      }).replaceError("Operands must be two numbers or two strings");
      case TokenType.MINUS -> numbers(left, right, (l, r) -> l - r);
      case TokenType.STAR -> numbers(left, right, (l, r) -> l * r);
      case TokenType.SLASH -> numbers(left, right, (l, r) -> l / r);
      case TokenType.BANG_EQUAL -> equals(left, right).map(eq -> !eq);
      case TokenType.EQUAL_EQUAL -> equals(left, right);
      case TokenType.GREATER -> numbers(left, right, (l, r) -> l > r);
      case TokenType.GREATER_EQUAL -> numbers(left, right, (l, r) -> l >= r);
      case TokenType.LESS -> numbers(left, right, (l, r) -> l < r);
      case TokenType.LESS_EQUAL -> numbers(left, right, (l, r) -> l <= r);
      case TokenType.AND -> bools(left, right, (l, r) -> l && r);
      case TokenType.OR -> bools(left, right, (l, r) -> l || r);
      default -> new Value<>(null, "unexpected token");
    }).or(operator);
  }

  private boolean isTruthy(Object object) {
    if (object == null)
      return false;
    if (object instanceof Boolean bool)
      return bool;
    return true;
  }

  private Value<Boolean> equals(Object left, Object right) {
    return new Value<>(!(left instanceof Double d && d.isNaN()) && Objects.equals(left, right), null);
  }

  private Value<Double> number(Object value) {
    return checkValue(Double.class, value);
  }

  private <T> Value<T> numbers(Object left, Object right, BiFunction<Double, Double, T> operator) {
    return checkValues(Double.class, left, right, operator);
  }

  private <T> Value<T> bools(Object left, Object right, BiFunction<Boolean, Boolean, T> operator) {
    return checkValues(Boolean.class, left, right, operator);
  }

  private <T> Value<T> strings(Object left, Object right, BiFunction<String, String, T> operator) {
    return checkValues(String.class, left, right, operator);
  }

  record Value<T>(T value, String error) {
    T or(Token operator) {
      if (value == null)
        throw new RuntimeError(operator, error);
      return value;
    }

    <U> Value<U> map(Function<T, U> fun) {
      if (value == null)
        return new Value<>(null, error);
      return new Value<>(fun.apply(value), error);
    }

    Value<T> replaceError(String error) {
      return value != null ? this : new Value<>(null, error);
    }
  }

  private <T> Value<T> checkValue(Class<T> klass, Object value) {
    return klass.isInstance(value) ? new Value<>(klass.cast(value), null)
        : new Value<>(null, "Operand must be a " + klass.getSimpleName().toLowerCase());
  }

  private <T, U> Value<U> checkValues(Class<T> klass, Object left, Object right, BiFunction<T, T, U> operator) {
    return (klass.isInstance(left) && klass.isInstance(right))
        ? new Value<>(operator.apply(klass.cast(left), klass.cast(right)), null)
        : new Value<>(null, "Operands must be " + klass.getSimpleName().toLowerCase() + "s");
  }
}
