package dev.pvsr.lox;

public record Token(TokenType type, String lexeme, Object literal, int line) {
  public String toString() {
    return type + " " + lexeme + " " + literal;
  }

  public boolean equals(Object object) {
    return this == object;
  }
}
