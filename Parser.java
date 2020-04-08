/**
 * Author: Armando Valdez
 * Course: CPSC326, Spring 2020
 * Date: 2/25/20
 * Homework: #4
 * File: Parser.java
 *
 * Recursive descent parser implementation for MyPL. The parser
 * requires a lexer. Once a parser is created, the parse() method
 * ensures the given program is syntactically correct.
 */

import java.util.*;



public class Parser {

  private Lexer lexer;
  private Token currToken = null;
  private boolean debug_flag = false;  // set to false to remove debug comments

  /**
   * Create a new parser over the given lexer.
   */
  public Parser(Lexer lexer) {
    this.lexer = lexer;
  }

  /**
   * Ensures program is syntactically correct. On error, throws a
   * MyPLException.
   */

   //
  public StmtList parse() throws MyPLException
  {
    StmtList stmtListNode = new StmtList();
    advance();
    stmts(stmtListNode);
    eat(TokenType.EOS, "expecting end of file");
    return stmtListNode;
  }


  /* Helper Functions */

  // sets current token to next token in stream
  private void advance() throws MyPLException {
    currToken = lexer.nextToken();
  }

  // checks that current token matches given type and advances,
  // otherwise creates an error with the given error message
  private void eat(TokenType t, String errmsg) throws MyPLException {
    if (currToken.type() == t)
      advance();
    else
      error(errmsg);
  }

  // generates an error message from the given message and throws a
  // corresponding MyPLException
  private void error(String errmsg) throws MyPLException {
    String s = errmsg + " found '" + currToken.lexeme() + "'";
    int row = currToken.row();
    int col = currToken.column();
    throw new MyPLException("Parser", errmsg, row, col);
  }

  // function to print a debug string if the debug_flag is set for
  // helping to diagnose/test the parser
  private void debug(String msg) {
    if (debug_flag)
      System.out.println(msg);
  }

  /* Recursive Descent Functions */
  //GOOD
  // <stmts> ::= <stmt> <stmts> | epsilon
  private void stmts(StmtList stmtListNode) throws MyPLException {
    if(currToken.type() != TokenType.EOS){
            stmt(stmtListNode);
            stmts(stmtListNode);
        }
  }

  //GOOD
  // <bstmts> ::= <bstmt> <bstmts> | epsilon
  private void bstmts(StmtList stmtListNode) throws MyPLException {
    if(currToken.type() == TokenType.VAR || currToken.type() == TokenType.SET || currToken.type() == TokenType.IF
        || currToken.type() == TokenType.WHILE || currToken.type() == TokenType.FOR || currToken.type() == TokenType.RETURN
        || currToken.type() == TokenType.NOT || currToken.type() == TokenType.LPAREN || isValExp(currToken.type())){
            stmtListNode.stmts.add(bstmt());
            bstmts(stmtListNode);     
        }
  }

  //Good
  // <stmt> ::= <tdecl> | <fdecl> | <bstmt>
  private void stmt(StmtList stmtListNode) throws MyPLException {
    if(currToken.type() == TokenType.TYPE){
        tdecl(stmtListNode);
    }else if(currToken.type() == TokenType.FUN){
        fdecl(stmtListNode);
    }else{
        stmtListNode.stmts.add(bstmt());
    }
  }

  //GOOD
  // <bstmt> ::= <vdecl> | <assign> | <cond> | <while> | <for> | <expr> | <exit>
  private Stmt bstmt() throws MyPLException {
    
    if(currToken.type() == TokenType.VAR){
        return vdecl();
    }else if(currToken.type() == TokenType.SET){
        return assign();
    }else if(currToken.type() == TokenType.IF){
        return cond();
    }else if(currToken.type() == TokenType.WHILE){
        return tokenWhile();
    }else if(currToken.type() == TokenType.FOR){
        return tokenFor();
    }else if(currToken.type() == TokenType.RETURN){
        return exit();
    }else{
         return expr();
    }
  }

  //GOOD
  // <tdecl> ::= TYPE ID <vdecls> END
  private void tdecl(StmtList stmtListNode) throws MyPLException {
      TypeDeclStmt tDeclNode = new TypeDeclStmt();
      eat(TokenType.TYPE, "Expeting 'TYPE'");
      tDeclNode.typeId = currToken;
      eat(TokenType.ID, "Expecting 'ID'");
      vdecls(tDeclNode);
      eat(TokenType.END, "Expecting 'END'");
      stmtListNode.stmts.add(tDeclNode);
  }

  //GOOD
  // <vdecls> ::= <vdecl> <vdecls> | E
  private void vdecls(TypeDeclStmt tDeclNode) throws MyPLException {
    if(currToken.type() == TokenType.VAR){
        tDeclNode.fields.add(vdecl());
        vdecls(tDeclNode);
    }
  }

  //GOOD
  // <fdecl> ::= FUN ( <dtype> | NIL ) ID LPAREN <params> RPAREN <bstmts> END
  private void fdecl(StmtList stmtListNode) throws MyPLException {
    FunDeclStmt funDeclNode = new FunDeclStmt();
    eat(TokenType.FUN, "Expecting 'FUN' declaration");
    if(isType(currToken.type())){
        funDeclNode.returnType = currToken;
        dtype();
    }else{
        funDeclNode.returnType = currToken;
        eat(TokenType.NIL, "Expecting 'NIL'");
    }
    funDeclNode.funName = currToken;
    eat(TokenType.ID, "Expecting 'ID'");
    eat(TokenType.LPAREN, "Expecting '('");
    params(funDeclNode);
    eat(TokenType.RPAREN, "Expecting ')'");
    bstmts(funDeclNode.stmtList);
    eat(TokenType.END, "Expecting 'END'");
    stmtListNode.stmts.add(funDeclNode);
  }

  //GOOD
  // <params> ::= <dtype> ID ( COMMA <dtype> ID )* | E
  private void params(FunDeclStmt funNode) throws MyPLException {
    FunParam paramNode = new FunParam();
    if(isType(currToken.type())){
        paramNode.paramType = currToken;
        dtype();
        paramNode.paramName = currToken;
        eat(TokenType.ID, "Expecting 'ID'");
        funNode.params.add(paramNode);
        while(currToken.type() == TokenType.COMMA){
            FunParam paramNode2 = new FunParam();
            eat(TokenType.COMMA, "Expecting ','");
            paramNode2.paramType = currToken;
            dtype();
            paramNode2.paramName = currToken;
            eat(TokenType.ID, "Expecting 'ID'");
            funNode.params.add(paramNode2);
        }
    }
  }

  //GOOD
  // <dtype> ::= INT_TYPE | DOUBLE_TYPE | BOOL_TYPE | CHAR_TYPE | STRING_TYPE | ID
  private void dtype() throws MyPLException {
      if(isType(currToken.type())){
          advance();
      }else{
          error("Expecting type value");
      }
  }

  //GOOD
  // <exit> ::= RETURN ( <expr> | E )
  private ReturnStmt exit() throws MyPLException {
    ReturnStmt returnNode = new ReturnStmt();  
    returnNode.returnToken = currToken;
    eat(TokenType.RETURN, "Expecting 'RETURN'");
    if(currToken.type() != TokenType.END){
        if(currToken.type() != TokenType.RETURN){
            returnNode.returnExpr = expr();
        }
    }
    return returnNode;
  }

  //GOOD
  // <vdecl> ::= VAR ( <dtype> | E ) ID ASSIGN <expr>
  private VarDeclStmt vdecl() throws MyPLException {
    VarDeclStmt varDeclNode = new VarDeclStmt();
    eat(TokenType.VAR, "Expecting 'VAR'");
    Token save = currToken;
    advance();
    if (currToken.type() != TokenType.ID) {
        varDeclNode.varId = save;
    }else{
        varDeclNode.varId = currToken;
        varDeclNode.varType = save;
        advance();
    }
    eat(TokenType.ASSIGN, "Expecting ':='");
    varDeclNode.varExpr = expr();
    return varDeclNode;
  }

  //GOOD
  // <assign> ::= SET <lvalue> ASSIGN <expr>
  private AssignStmt assign() throws MyPLException {
    AssignStmt assignNode = new AssignStmt();
    eat(TokenType.SET, "Expecting 'SET'");
    assignNode.lhs = lvalue();
    eat(TokenType.ASSIGN, "EXpecting ':='");
    assignNode.rhs = expr();
    return assignNode;
  }

  //GOOD
  // <lvalue> ::= ID ( DOT ID )*
  private LValue lvalue() throws MyPLException {
      LValue lValNode = new LValue();
      lValNode.path.add(currToken);
      eat(TokenType.ID, "Expecting 'ID'");
      while(currToken.type() == TokenType.DOT){
          advance();
          lValNode.path.add(currToken);
          eat(TokenType.ID, "Expecting 'ID'");
      }
      return lValNode;
  }

  //GOOD
  // <cond> ::= IF <expr> THEN <bstmts> <condt> END
  private IfStmt cond() throws MyPLException {
      IfStmt ifStmtNode = new IfStmt();
      BasicIf basicIfNode = new BasicIf();
      eat(TokenType.IF, "Expecting 'IF'");
      basicIfNode.boolExpr = expr();
      eat(TokenType.THEN, "Expecting 'THEN'");
      bstmts(basicIfNode.stmtList);
      ifStmtNode.ifPart = basicIfNode;
      condt(ifStmtNode);
      eat(TokenType.END, "Expecting 'END'" );
      return ifStmtNode;
  }

  //GOOD
  // <condt> ::= ELIF <expr> THEN <bstmts> <condt> | ELSE <bstmts> | E
  private void condt(IfStmt ifStmtNode) throws MyPLException {
      BasicIf basicIfNode = new BasicIf();
      if(currToken.type() == TokenType.ELIF){
          advance();
          basicIfNode.boolExpr = expr();
          eat(TokenType.THEN, "Expecting 'THEN'");
          bstmts(basicIfNode.stmtList);
          ifStmtNode.elsifs.add(basicIfNode);
          condt(ifStmtNode);
      }else if(currToken.type() == TokenType.ELSE){
          ifStmtNode.hasElse = true;
          advance();
          bstmts(ifStmtNode.elseStmtList);
      }
  }

  //GOOD
  // <while> ::= WHILE <expr> DO <bstmts> END
  private WhileStmt tokenWhile() throws MyPLException {
      WhileStmt whileNode = new WhileStmt();
      eat(TokenType.WHILE, "Expecting 'WHILE'");
      whileNode.boolExpr = expr();
      eat(TokenType.DO, "Expecting 'DO'");
      bstmts(whileNode.stmtList);
      eat(TokenType.END, "Expecting 'END'");
      return whileNode;
    }

  //GOOD
  // <for> ::= FOR ID ASSIGN <expr> TO <expr> DO <bstmts> END
  private ForStmt tokenFor() throws MyPLException {
      ForStmt forStmtNode = new ForStmt();
      eat(TokenType.FOR, "Expecting 'FOR'");
      forStmtNode.var = currToken;
      eat(TokenType.ID, "Expecting 'ID'");
      eat(TokenType.ASSIGN, "Expecting ':='");
      forStmtNode.startExpr = expr();
      eat(TokenType.TO, "Expecting 'TO'");
      forStmtNode.endExpr = expr();
      eat(TokenType.DO, "Expecting 'DO'");
      bstmts(forStmtNode.stmtList);
      eat(TokenType.END, "Expecting 'END'");
      return forStmtNode;
  }

  // <expr> ::= ( <rvalue> | NOT <expr> | LPAREN <expr> RPAREN ) ( <operator> <expr> | E )
  private Expr expr() throws MyPLException {
      Expr exprNode = new Expr();
      if(currToken.type() == TokenType.NOT){
          eat(TokenType.NOT, "Expecting 'NOT'");
          exprNode = expr();
          exprNode.negated = true;
          return exprNode;
      }else if(currToken.type() == TokenType.LPAREN){
          ComplexTerm exprComplexNode = new ComplexTerm();
          eat(TokenType.LPAREN, "Expecting '('");
          exprComplexNode.expr = expr();
          exprNode.first = exprComplexNode;
          eat(TokenType.RPAREN, "Expecting ')'");
      }else{
          SimpleTerm exprSimpleNode = new SimpleTerm();
          exprSimpleNode.rvalue = rvalue();
          exprNode.first = exprSimpleNode;
      }
      if(isOperator(currToken.type())){
          exprNode.operator = currToken;
          operator();
          exprNode.rest = expr();
      }
      return exprNode;
  }

  // <operator> ::= PLUS | MINUS | DIVIDE | MULTIPLY | MODULO | AND | OR | EQUAL | LESS_THAN
 //                 | GREATER_THAN | LESS_THAN_EQUAL | GREATER_THAN_EQUAL | NOT_EQUAL
  private void operator() throws MyPLException {
      if(isOperator(currToken.type())){
          advance();
      }else{
          error("Expecting Operator");
      }
  }

  //GOOD
  // <rvalue> ::= <pval> | NIL | NEW ID | <idrval> | NEG <expr>
  private RValue rvalue() throws MyPLException {
    if(currToken.type() == TokenType.NIL){
        SimpleRValue simpleRValueNode = new SimpleRValue();
        simpleRValueNode.val = currToken;
        advance();
        return simpleRValueNode;
    }else if(currToken.type() == TokenType.NEW){
        NewRValue newRValueNode = new NewRValue();
        advance();
        newRValueNode.typeId = currToken;
        eat(TokenType.ID, "Expeting 'ID'");
        return newRValueNode;
    }else if(currToken.type() == TokenType.NEG){
        NegatedRValue negRValueNode = new NegatedRValue();
        advance();
        negRValueNode.expr = expr();
        negRValueNode.expr.negated = true;
        return negRValueNode;
    }else if(isVal(currToken.type())){
        SimpleRValue simpleRValueNode = new SimpleRValue();
        simpleRValueNode.val = currToken;
        pval();
        return simpleRValueNode;
    }else{
        return idrval();
    }
}

  // <val> ::= INT_VAL | DOUBLE_VAL | BOOL_VAL | CHAR_VAL | STRING_VAL
  private void pval() throws MyPLException {
      if(isVal(currToken.type())){
        advance();
      }else{
          error("Expecting variable declaration or variable value");
      }
  }

  //GOOD
  // <idrval> ::= ID ( DOT ID )* | ID LPAREN <exprlist> RPAREN
  private RValue idrval() throws MyPLException {
      IDRValue idRValNode = new IDRValue();
      CallRValue callRValueNode = new CallRValue();
      idRValNode.path.add(currToken);
      callRValueNode.funName = currToken;
      eat(TokenType.ID, "Expecting 'ID'");
      if(currToken.type() == TokenType.LPAREN){
          advance();
          exprlist(callRValueNode);
          eat(TokenType.RPAREN, "Expecting ')'");
          return callRValueNode;
      }else{
          while(currToken.type() == TokenType.DOT){
              advance();
              idRValNode.path.add(currToken);
              eat(TokenType.ID, "Expecting 'ID'");
          }
          return idRValNode;
      }
  }

  //GOOD
  // <exprlist> ::= <expr> ( COMMA <expr> )* | E
  private void exprlist(CallRValue callRValueNode) throws MyPLException {
      if (currToken.type() == TokenType.NOT || currToken.type() == TokenType.LPAREN ||  isValExp(currToken.type())){
        callRValueNode.argList.add(expr());
        while (currToken.type() == TokenType.COMMA) {
            advance();
            callRValueNode.argList.add(expr());
      }
    }
  }

  private boolean isOperator(TokenType t){
      Set <TokenType> s = new HashSet<TokenType>();

      s.add(TokenType.PLUS);
      s.add(TokenType.MINUS);
      s.add(TokenType.DIVIDE);
      s.add(TokenType.MULTIPLY);
      s.add(TokenType.MODULO);
      s.add(TokenType.AND);
      s.add(TokenType.OR);
      s.add(TokenType.EQUAL);
      s.add(TokenType.LESS_THAN);
      s.add(TokenType.GREATER_THAN);
      s.add(TokenType.LESS_THAN_EQUAL);
      s.add(TokenType.GREATER_THAN_EQUAL);
      s.add(TokenType.NOT_EQUAL);

      return s.contains(t);
  }

  private boolean isType(TokenType t){
      Set <TokenType> s = new HashSet<TokenType>();

      s.add(TokenType.INT_TYPE);
      s.add(TokenType.DOUBLE_TYPE);
      s.add(TokenType.BOOL_TYPE);
      s.add(TokenType.CHAR_TYPE);
      s.add(TokenType.STRING_TYPE);
      s.add(TokenType.ID);

      return s.contains(t);
  }

  private boolean isVal(TokenType t){
      Set <TokenType> s = new HashSet<TokenType>();

      s.add(TokenType.INT_VAL);
      s.add(TokenType.DOUBLE_VAL);
      s.add(TokenType.BOOL_VAL);
      s.add(TokenType.CHAR_VAL);
      s.add(TokenType.STRING_VAL);

      return s.contains(t);
  }

  private boolean isValExp(TokenType t){
      Set <TokenType> s = new HashSet<TokenType>();

      s.add(TokenType.INT_VAL);
      s.add(TokenType.DOUBLE_VAL);
      s.add(TokenType.BOOL_VAL);
      s.add(TokenType.CHAR_VAL);
      s.add(TokenType.STRING_VAL);
      s.add(TokenType.NIL);
      s.add(TokenType.ID);
      s.add(TokenType.NEG);
      s.add(TokenType.NEW);

      return s.contains(t);
  }
}
