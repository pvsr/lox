package dev.pvsr.lox;

import java.util.List;

sealed interface Stmt {
  record Expression(Expr expression) implements Stmt {};
  record Print(Expr expression) implements Stmt {};
  record Return(Token keyword, Expr value) implements Stmt {};
  record Var(Token name, Expr initializer) implements Stmt {};
  record If(Expr condition, Stmt thenBranch, Stmt elseBranch) implements Stmt {};
  record Function(Token name, List<Token> params, List<Stmt> body) implements Stmt {};
  record While(Expr condition, Stmt body) implements Stmt {};
  record Block(List<Stmt> statements) implements Stmt {};
}
