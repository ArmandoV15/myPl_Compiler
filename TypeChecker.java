
/**
 * Author: Armando Valdez
 * Assign: 5
 * File: TypeChecker.java
 *
 * Visitor implementation of Semantic Analysis Checking for the MyPL
 * AST. Note the following conventions for representing type
 * information:
 * 
 * A variable name's type is a string (varname to string)
 *
 * A structured type name is a map of var mappings (typename to Map) where
 * each variable name is mapped to its type 
 *
 * A function type name is a list of parameter types (name to
 * List) where the list consists of each formal param type ending with
 * the return type of the function.
 *
 * For more information on the general design see the lecture notes.
 */

import java.util.List;
import java.net.http.HttpResponse.PushPromiseHandler;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;


public class TypeChecker implements Visitor {
  // the symbol table
  private final SymbolTable symbolTable = new SymbolTable();
  // holds last inferred type
  private String currType = null;

  // sets up the initial environment for type checking
  public TypeChecker() {
    symbolTable.pushEnvironment();

    // add return type for global scope
    symbolTable.addName("return");
    symbolTable.setInfo("return", "int");
    // print function
    symbolTable.addName("print");
    symbolTable.setInfo("print", List.of("string", "nil"));

    // read function
    symbolTable.addName("read");
    symbolTable.setInfo("read", List.of("string"));

    //length function
    symbolTable.addName("length");
    symbolTable.setInfo("length", List.of("string", "int"));

    //get function
    symbolTable.addName("get");
    symbolTable.setInfo("get", List.of("int", "string", "char"));

    //concat function
    symbolTable.addName("concat");
    symbolTable.setInfo("concat", List.of("string", "string", "string"));

    //append function
    symbolTable.addName("append");
    symbolTable.setInfo("append", List.of("string", "char", "string"));

    //itos function
    symbolTable.addName("itos");
    symbolTable.setInfo("itos", List.of("int", "string"));

    //stoi function
    symbolTable.addName("stoi");
    symbolTable.setInfo("stoi", List.of("string", "int"));

    //dtos function
    symbolTable.addName("dtos");
    symbolTable.setInfo("dtos", List.of("double", "string"));

    //dtos function
    symbolTable.addName("stod");
    symbolTable.setInfo("stod", List.of("string", "double"));


  }

  // visitor functions
  public void visit(final StmtList node) throws MyPLException {
    symbolTable.pushEnvironment();
    for (final Stmt s : node.stmts)
      s.accept(this);
    symbolTable.popEnvironment();    
  }
  
  public void visit(final AssignStmt node) throws MyPLException {
    // check and infer rhs type
    node.rhs.accept(this);
    final String rhsType = currType;
    // check and obtain lhs type
    node.lhs.accept(this);
    final String lhsType = currType;
    // error if rhs and lhs types don't match
    if (!rhsType.equals("nil") && !rhsType.equals(lhsType)) {
      final String msg = "mismatched type in assignment";
      error(msg, node.lhs.path.get(0));
    }
  }

  public void visit(final SimpleTerm node) throws MyPLException {
    node.rvalue.accept(this);
  }

  
  public void visit(final ComplexTerm node) throws MyPLException {
    node.expr.accept(this);
  }

  public void visit(final SimpleRValue node) throws MyPLException {
    if (node.val.type() == TokenType.INT_VAL)
      currType = "int";
    else if (node.val.type() == TokenType.DOUBLE_VAL)
      currType = "double";
    else if (node.val.type() == TokenType.BOOL_VAL)
      currType = "bool";
    else if (node.val.type() == TokenType.CHAR_VAL)
      currType = "char";
    else if (node.val.type() == TokenType.STRING_VAL)
      currType = "string";
    else if (node.val.type() == TokenType.NIL)
      currType = "nil";
  }

  
  public void visit(final IDRValue node) throws MyPLException {
    // check the first id in the path
    String varName = node.path.get(0).lexeme();
    if (!symbolTable.nameExists(varName))
      error("undefined variable '" + varName + "'", node.path.get(0));
    // make sure it isn't function or type name
    if (symbolTable.getInfo(varName) instanceof List)
      error("unexpected function name in rvalue", node.path.get(0));
    if (symbolTable.getInfo(varName) instanceof Map)
      error("unexpected type name in rvalue", node.path.get(0));
    // grab the type
    currType = (String) symbolTable.getInfo(varName);
    if (node.path.size() > 1 && !(symbolTable.getInfo(currType) instanceof Map))
      error("invalid member access for non-structured type", node.path.get(0));
    // check path
    Map<String, String> typeInfo = (Map<String, String>)symbolTable.getInfo(currType);
    for (int i = 1; i < node.path.size(); i++) {
      typeInfo = (Map<String, String>)symbolTable.getInfo(currType);
      varName = node.path.get(i).lexeme();
      if(!typeInfo.containsKey(varName))
        error("undifined variable '" + varName + "'", node.path.get(i));
      currType = typeInfo.get(varName);
      if(node.path.size() > (i + 1) && !(symbolTable.getInfo(currType) instanceof Map))
        error("Invlaid member access for non-structured type", node.path.get(0));
    }
  }

  //Type checker for MyPL variable declaration.
  public void visit(final VarDeclStmt node) throws MyPLException {
    node.varExpr.accept(this);
    if(node.varType != null && !currType.equals(node.varType.lexeme()) && !currType.equals("nil")){
      String msg = "type mismatch in variable declaration";
      error(msg, node.varType);
    }
    if(node.varType == null && currType.equals("nil")){
      String msg = "type missing in nil assignment";
      error(msg, node.varId);
    }
    if(node.varType != null){
      currType = node.varType.lexeme();
    }

    if(symbolTable.nameExistsInCurrEnv(node.varId.lexeme())){
      String msg = node.varId.lexeme() + "' has already been declared";
      error(msg, node.varId);
    }
    symbolTable.addName(node.varId.lexeme());
    symbolTable.setInfo(node.varId.lexeme(), currType);
  }

  //Type checker for MyPL return statements
  public void visit(final ReturnStmt node) throws MyPLException {
    if(node.returnExpr != null){
      node.returnExpr.accept(this);
    }else{
      currType = "nil";
    }
    if(!currType.equals("nil") && !currType.equals(symbolTable.getInfo("return"))){
      String msg = "return does not match";
      error(msg, node.returnToken);
    } 
  }

  //Type cheker for MyPL 'if' statements. Makes sure to push and pop environments to avoid shadowing
  public void visit(final IfStmt node) throws MyPLException {
    symbolTable.pushEnvironment();
    node.ifPart.boolExpr.accept(this);
    node.ifPart.stmtList.accept(this);
    symbolTable.popEnvironment();
    for(BasicIf item : node.elsifs){
      symbolTable.pushEnvironment();
      item.boolExpr.accept(this);
      item.stmtList.accept(this);
      symbolTable.popEnvironment();
    }
    if(node.hasElse){
      symbolTable.pushEnvironment();
      node.elseStmtList.accept(this);
      symbolTable.popEnvironment();
    }
  }

  //Type cheker for MyPL 'while' statements. Makes sure to push and pop environments to avoid shadowing
  public void visit(final WhileStmt node) throws MyPLException {
    node.boolExpr.accept(this);
    symbolTable.pushEnvironment();
    if(!currType.equals("bool")){
      String msg = "Requires boolean type";
      error(msg, getFirstToken(node.boolExpr));
    }
    node.stmtList.accept(this);
    symbolTable.popEnvironment();
  }

  //Type cheker for MyPL 'for' statements. Makes sure to push and pop environments to avoid shadowing
  public void visit(final ForStmt node) throws MyPLException {
    symbolTable.pushEnvironment();
    symbolTable.addName(node.var.lexeme());
    symbolTable.setInfo(node.var.lexeme(), "int");
    node.startExpr.accept(this);
    node.endExpr.accept(this);
    for(Stmt stmt : node.stmtList.stmts){
      stmt.accept(this);
    }
    symbolTable.popEnvironment();
  }

  //Type checker for type declarations in MyPL
  public void visit(final TypeDeclStmt node) throws MyPLException {
    symbolTable.pushEnvironment();
    Map<String, String> typeInfo = new HashMap<>();
    for(VarDeclStmt v : node.fields){
      v.accept(this);
      typeInfo.put(v.varId.lexeme(), currType);
    }
    symbolTable.popEnvironment();
    symbolTable.addName(node.typeId.lexeme());
    symbolTable.setInfo(node.typeId.lexeme(), typeInfo);
  }

  //Type checker for function declarations in MyPL. 
  //Makes sure functions are declared and structured corretly with the correct types
  public void visit(final FunDeclStmt node) throws MyPLException {
    String func = node.funName.lexeme();
    List<String> args = new ArrayList<>();

    if(symbolTable.nameExists(func)){
      String msg = "Function already declared";
      error(msg, node.funName); 
    }
    symbolTable.addName(func);
    symbolTable.pushEnvironment();
    symbolTable.setInfo("return", node.returnType.lexeme());
    for(FunParam f : node.params){
      if(symbolTable.nameExistsInCurrEnv(f.paramName.lexeme())){
        String msg = "function already exist";
      error(msg, node.funName); 
      }
      args.add(f.paramType.lexeme());
      symbolTable.addName(f.paramName.lexeme());
      symbolTable.setInfo(f.paramName.lexeme(), f.paramType.lexeme());
    }
    args.add(node.returnType.lexeme());
    symbolTable.setInfo(func, args);
    node.stmtList.accept(this);
    symbolTable.popEnvironment();
    symbolTable.setInfo(func, args);
  }

  //Type checker for the lhs and rhs of all expressions in MyPL
  public void visit(final Expr node) throws MyPLException {
    if(node.first != null){
      node.first.accept(this);
    }
    String lhsType = currType;

    if(node.rest != null){
      node.rest.accept(this);
    }
    String rhsType = currType;

    if(node.operator != null){
      TokenType operToken = node.operator.type();

      if(!lhsType.equals(rhsType) && (!lhsType.equals("nil") && !rhsType.equals("nil"))){
        String msg = "Mismatch type in assignment";
        error(msg, node.operator);
      }
      if(operToken == TokenType.MODULO){
        if(!lhsType.equals("int")){
          String msg = "Invlaid use if modulo";
          error(msg, node.operator);
        }
      }
      if(lhsType.equals("int") || lhsType.equals("double")){
        if(!isOperator(operToken) ){
          String msg = "mismatch types in assignment";
          error(msg, node.operator);
        }else if(rhsType.equals("nil")){
          if(!isBop(operToken)){
            String msg = "mismatch types in assignment";
            error(msg, node.operator);
          }
        } 
      }
      if(lhsType.equals("bool")){
        if(!isBoolOperator(operToken)){
          String msg = "invalid use of operator";
          error(msg, node.operator);
        }
      }
      if(lhsType.equals("nil")){
        if(operToken != TokenType.EQUAL || operToken != TokenType.NOT_EQUAL){
          String msg = "invalid use of operator";
          error(msg, node.operator);
        }
      }
      if(lhsType.equals("string") || lhsType.equals("char")){
        if(isOperator(operToken) || isBoolOperator(operToken)){
          String msg = "invalid use of operator";
          error(msg, node.operator);
        }
      }
      if(isCopare(operToken)){
        currType = "bool";
      }
    }else{
      currType = lhsType;
    }
  }

  //Very similair to IDRVal
  public void visit(final LValue node) throws MyPLException {
    // check the first id in the path
    String varName = node.path.get(0).lexeme();
    if (!symbolTable.nameExists(varName))
      error("undefined variable '" + varName + "'", node.path.get(0));
    // make sure it isn't function or type name
    if (symbolTable.getInfo(varName) instanceof List)
      error("unexpected function name in lvalue", node.path.get(0));
    if (symbolTable.getInfo(varName) instanceof Map)
      error("unexpected type name in lvalue", node.path.get(0));
    // grab the type
    currType = (String) symbolTable.getInfo(varName);
    if (node.path.size() > 1 && !(symbolTable.getInfo(currType) instanceof Map))
      error("invalid member access for non-structured type", node.path.get(0));
    // check path
    Map<String, String> typeInfo = (Map<String, String>)symbolTable.getInfo(currType);
    for (int i = 1; i < node.path.size(); i++) {
      typeInfo = (Map<String, String>)symbolTable.getInfo(currType);
      varName = node.path.get(i).lexeme();
      if(!typeInfo.containsKey(varName))
        error("undifined variable '" + varName + "'", node.path.get(i));
      currType = typeInfo.get(varName);
      if(node.path.size() > (i + 1) && !(symbolTable.getInfo(currType) instanceof Map))
        error("Invlaid member access for non-structured type", node.path.get(0));
    }
  }

  //Keeps track of all new RValues and if it already exist in the symbolTable then currType is set to the typeId of that value
  public void visit(final NewRValue node) throws MyPLException {
    String newRVal = node.typeId.lexeme();
    if(!symbolTable.nameExists(newRVal)){
      String msg = newRVal+ "does not exist";
      error(msg, node.typeId);
    }
    currType = node.typeId.lexeme();
  }

  public void visit(final CallRValue node) throws MyPLException {
    String func = node.funName.lexeme();
    if(!symbolTable.nameExists(func)){
      String msg = "Function does not exist";
      error(msg, node.funName);
    }
    List<String> args = (List<String>) symbolTable.getInfo(func);
    if(node.argList.size() != args.size() - 1){
      String msg = "wrong number of arguments";
      error(msg, node.funName);
    }else if(node.argList.size() > (args.size() - 1)){
      String msg = "wrong number of arguments";
      error(msg, node.funName);
    }
    for(int i = 0; i < node.argList.size(); i++){
      node.argList.get(i).accept(this);
      if(!currType.equals(args.get(i)) && !currType.equals("nil")){
        String msg = "wrong argument type";
        error(msg, node.funName);
      }
    }
    currType = args.get(args.size() - 1);
  }

  public void visit(final NegatedRValue node) throws MyPLException {
    node.expr.accept(this);
  }
  
  // helper functions

  private void error(final String msg, final Token token) throws MyPLException {
    final int row = token.row();
    final int col = token.column();
    throw new MyPLException("Type", msg, row, col);
  }

  // gets first token of an expression
  private Token getFirstToken(final Expr node) {
    return getFirstToken(node.first);
  }

  // gets first token of an expression term
  private Token getFirstToken(final ExprTerm node) {
    if (node instanceof SimpleTerm)
      return getFirstToken(((SimpleTerm)node).rvalue);
    else
      return getFirstToken(((ComplexTerm)node).expr);      
  }

  // gets first token of an rvalue
  private Token getFirstToken(final RValue node) {
    if (node instanceof SimpleRValue)
      return ((SimpleRValue)node).val;
    else if (node instanceof CallRValue)
      return ((CallRValue)node).funName;
    else if (node instanceof IDRValue)
      return ((IDRValue)node).path.get(0);
    else if (node instanceof NegatedRValue) 
      return getFirstToken(((NegatedRValue)node).expr);
    else 
      return ((NewRValue)node).typeId;
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

private boolean isBoolOperator(TokenType t){
  Set <TokenType> s = new HashSet<TokenType>();

  s.add(TokenType.AND);
  s.add(TokenType.OR);
  s.add(TokenType.EQUAL);
  s.add(TokenType.NOT_EQUAL);

  return s.contains(t);
}

private boolean isCopare(TokenType t){
  Set <TokenType> s = new HashSet<TokenType>();

  s.add(TokenType.EQUAL);
  s.add(TokenType.LESS_THAN);
  s.add(TokenType.GREATER_THAN);
  s.add(TokenType.LESS_THAN_EQUAL);
  s.add(TokenType.GREATER_THAN_EQUAL);
  s.add(TokenType.NOT_EQUAL);

  return s.contains(t);
}

private boolean isBop(TokenType t){
  Set <TokenType> s = new HashSet<TokenType>();

  s.add(TokenType.EQUAL);
  s.add(TokenType.NOT_EQUAL);

  return s.contains(t);
}
}    

