/**
 * Author: Armando Valdez
 * Course: CPSC326, Spring 2020
 * Date: 1/30/20
 * Assign: HW2
 *
 * The lexer implementation tokenizes a given input stream. The lexer
 * implements a pull-based model via the nextToken function such that
 * each call to nextToken advances the lexer to the next token (which
 * is returned by nextToken). The file has been completed read when
 * nextToken returns the EOS token. Lexical errors in the source file
 * result in the nextToken function throwing a MyPL Exception.
 */

import java.util.*;
import java.io.*;


public class Lexer {

  private BufferedReader buffer; // handle to input stream
  private int line;
  private int column;

  /**
   */
  public Lexer(InputStream instream) {
    buffer = new BufferedReader(new InputStreamReader(instream));
    this.line = 1;
    this.column = 0;
  }


  /**
   * Returns next character in the stream. Returns -1 if end of file.
   */
  private int read() throws MyPLException {
    try {
      int ch = buffer.read();
      return ch;
    } catch(IOException e) {
      error("read error", line, column + 1);
    }
    return -1;
  }


  /**
   * Returns next character without removing it from the stream.
   */
  private int peek() throws MyPLException {
    int ch = -1;
    try {
      buffer.mark(1);
      ch = read();
      buffer.reset();
    } catch(IOException e) {
      error("read error", line, column + 1);
    }
    return ch;
  }


  /**
   * Print an error message and exit the program.
   */
  private void error(String msg, int line, int column) throws MyPLException {
    throw new MyPLException("Lexer", msg, line, column);
  }


  /**
  Traverses through a .mypl file and assigns every symbol, and data type to a
  specific TokenType. This function also throws exceptions when the lexer runs
  into code errors in the .mypl file.
   */
  public Token nextToken() throws MyPLException {

    String lexeme = "";
    char symbol;


    if (peek() == -1){
        return new Token(TokenType.EOS, "", line, column);
    }
    symbol = (char)read();
    column++;

    if(Character.isLetter(symbol)){
        int currColumn = column;
        lexeme += symbol;
        while(Character.isLetter((char)peek()) || (char)peek() == '_' || Character.isDigit((char)peek())){
            column++;
            lexeme += (char)read();
        }
        //Cheking each lexeme read in my the read() fuction.
        switch(lexeme){
            case "int":
                return new Token(TokenType.INT_TYPE, lexeme, line, currColumn);
            case "bool":
                return new Token(TokenType.BOOL_TYPE, lexeme, line, currColumn);
            case "double":
                return new Token(TokenType.DOUBLE_TYPE, lexeme, line, currColumn);
            case "char":
                return new Token(TokenType.CHAR_TYPE, lexeme, line, currColumn);
            case "string":
                return new Token(TokenType.STRING_TYPE, lexeme, line, currColumn);
            case "type":
                return new Token(TokenType.TYPE, lexeme, line, currColumn);
            case "and":
                return new Token(TokenType.AND, lexeme, line, currColumn);
            case "or":
                return new Token(TokenType.OR, lexeme, line, currColumn);
            case "not":
                return new Token(TokenType.NOT, lexeme, line, currColumn);
            case "neg":
                return new Token(TokenType.NEG, lexeme, line, currColumn);
            case "while":
                return new Token(TokenType.WHILE, lexeme, line, currColumn);
            case "for":
                return new Token(TokenType.FOR, lexeme, line, currColumn);
            case "to":
                return new Token(TokenType.TO, lexeme, line, currColumn);
            case "do":
                return new Token(TokenType.DO, lexeme, line, currColumn);
            case "if":
                return new Token(TokenType.IF, lexeme, line, currColumn);
            case "then":
                return new Token(TokenType.THEN, lexeme, line, currColumn);
            case "else":
                return new Token(TokenType.ELSE, lexeme, line, currColumn);
            case "elif":
                return new Token(TokenType.ELIF, lexeme, line, currColumn);
            case "end":
                return new Token(TokenType.END, lexeme, line, currColumn);
            case "fun":
                return new Token(TokenType.FUN, lexeme, line, currColumn);
            case "var":
                return new Token(TokenType.VAR, lexeme, line, currColumn);
            case "set":
                return new Token(TokenType.SET, lexeme, line, currColumn);
            case "return":
                return new Token(TokenType.RETURN, lexeme, line, currColumn);
            case "new":
                return new Token(TokenType.NEW, lexeme, line, currColumn);
            case "nil":
                return new Token(TokenType.NIL, lexeme, line, currColumn);
            case "true":
                return new Token(TokenType.BOOL_VAL, lexeme, line, currColumn);
            case "false":
                return new Token(TokenType.BOOL_VAL, lexeme, line, currColumn);
            default:
                break;
        }
            return new Token(TokenType.ID, lexeme, line, currColumn);
    }
        // Checking each symbol read by the read() function
        switch(symbol){
            case ',':
                return new Token(TokenType.COMMA, ",", line, column);
            case '.':
                return new Token(TokenType.DOT, ".", line, column);
            case '+':
                return new Token(TokenType.PLUS, "+", line, column);
            case '-':
                return new Token(TokenType.MINUS, "-", line, column);
            case '*':
                return new Token(TokenType.MULTIPLY, "*", line, column);
            case '/':
                return new Token(TokenType.DIVIDE, "/", line, column);
            case '%':
                return new Token(TokenType.MODULO, "%", line, column);
            case '=':
                return new Token(TokenType.EQUAL, "=", line, column);
            case '(':
                return new Token(TokenType.LPAREN, "(", line, column);
            case ')':
                return new Token(TokenType.RPAREN, ")", line, column);
            case '>':
                // Checking to see if there is a '>=' sign
                if((char)peek() == '='){
                    read();
                    column++;
                    return new Token(TokenType.GREATER_THAN_EQUAL, ">=", line, column - 1);
                }
                return new Token(TokenType.GREATER_THAN, ">", line, column);
            case '<':
                // Checking to see if there is a '<=' sign
                if((char)peek() == '='){
                    read();
                    column++;
                    return new Token(TokenType.LESS_THAN_EQUAL, "<=", line, column - 1);
                }
                return new Token(TokenType.LESS_THAN, "<", line, column);
            case '\'':
                lexeme += (char)read();
                int currColumn = column;
                if((char)read() != '\''){
                    String msg = "this is an invalid character";
                    error(msg, line, currColumn);
                }
                return new Token(TokenType.CHAR_VAL, lexeme, line, currColumn);
            case ':':
                if((char)peek() == '='){
                    read();
                    column++;
                    return new Token(TokenType.ASSIGN, ":=", line, column - 1);
                }
            case '!':
                if((char)peek() == '='){
                    read();
                    column++;
                    return new Token(TokenType.NOT_EQUAL, "!=", line, column - 1);
                }
            case '"':
                int thisCol = column;
                while ((symbol = (char)read()) != '"'){
                    if(symbol == '\n'){
                        String msg = "found newline within string";
                        error(msg, line, column);
                    }
                    column++;
                    lexeme += symbol;
                }
                column++;
                return new Token(TokenType.STRING_VAL, lexeme, line, thisCol);
            case '#':
                while((char)peek() != '\n'){
                    read();
                }
                return nextToken();
            default:
                break;
        }
            if(Character.isWhitespace(symbol)){
                if(symbol == '\n'){
                    column = 0;
                    line ++;
                }
                return nextToken();
            }

            if(Character.isDigit(symbol)){
                lexeme += symbol;
                int col = column;
                boolean isInt = true;
                while(!Character.isWhitespace((char)peek())){
                    if(Character.isDigit((char)peek())){
                        column++;
                        lexeme += (char)read();
                    }
                    else if((char)peek() == '.'){
                        column++;
                        isInt = false;
                        lexeme += (char)read();
                        if(!Character.isDigit((char)peek())){
                            String msg = "missing digit in float '" + lexeme + "'";
                            error(msg, line, col);
                        }
                    }else if(Character.isLetter((char)peek())){
                        String msg = "unexpected symbol '" + (char)peek() + "'";
                        error(msg, line, column + 1);
                    }else
                        break;
                }if(isInt){
                    if(lexeme.charAt(0) == '0' && lexeme.length() > 1){
                        String msg = "leading zero in '" + lexeme + "'";
                        error(msg, line, col);
                    }
                    return new Token(TokenType.INT_VAL, lexeme, line, col);
                }
                return new Token(TokenType.DOUBLE_VAL, lexeme, line, col);
}
    String msg = "unexpected symbol '" + symbol + "'";
    throw new MyPLException("Lexer", msg, line, column);
  }
}
