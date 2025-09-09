package dev.pvsr.lox;

import java.util.List;
import java.util.function.BiFunction;

interface LoxCallable {
  int arity();

  Object call(Interpreter interpreter, List<Object> arguments);

  record LoxFunction(Stmt.Function declaration, Environment closure) implements LoxCallable {
    @Override
    public int arity() {
      return declaration.params().size();
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
      Environment environment = new Environment(closure);
      for (int i = 0; i < declaration.params().size(); i++) {
        environment.define(declaration.params().get(i).lexeme(), arguments.get(i));
      }
      try {
        interpreter.executeBlock(declaration.body(), environment);
      } catch (Return returnValue) {
        return returnValue.value;
      }
      return null;
    }

    @Override
    public String toString() {
      return "<fn " + declaration.name().lexeme() + ">";
    }
  }

  record NativeFunction(int arity, BiFunction<Interpreter, List<Object>, Object> callee) implements LoxCallable {
    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
      return callee.apply(interpreter, arguments);
    }

    @Override
    public String toString() {
      return "<native fn>";
    }
  }
}
