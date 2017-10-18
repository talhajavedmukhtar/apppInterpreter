package com.talhajavedmukhtar.APPP

sealed trait apppTokenType

case class apppToken(tokenType: apppTokenType, value: String)

case object IDENTIFIER extends apppTokenType

case object LITERAL_ALPHA extends apppTokenType
case object LITERAL_INT extends apppTokenType
case object LITERAL_BOOL extends apppTokenType

case object IDENTIFIER_TYPE extends apppTokenType   //can be const or var

case object COLON extends apppTokenType

//statements separator (can be ; or \n)
case object STATEMENT_SEP extends apppTokenType

//data types (appear when declaring an identifier to attach a type to it)
case object DATA_TYPE extends apppTokenType    //can be int, alpha and bool

case object PRINT extends apppTokenType

case object NIL extends apppTokenType

case object WHILE_CONDITION extends apppTokenType
case object DO_THIS extends apppTokenType

case object IF_CONDITION extends apppTokenType
case object THEN_DO extends apppTokenType
case object ELSE_DO extends apppTokenType

//the assignment operator
case object ASSIGNMENT_OP extends apppTokenType

//binary operators
/*case object ADD extends apppTokenType
case object MUL extends apppTokenType
case object AND extends apppTokenType
case object OR extends apppTokenType
case object DIF extends apppTokenType
case object XOR extends apppTokenType
case object COMP extends apppTokenType      //Comparison operator (==)
case object GRT extends apppTokenType       //Greater than (>)
case object LST extends apppTokenType       //Less than (<)
case object NOT_EQUAL extends apppTokenType  // (><)*/
case object BINARY_OP extends apppTokenType

//unary operators
//case object NOT extends apppTokenType
case object UNARY_OP extends apppTokenType

case object LPAREN extends apppTokenType
case object RPAREN extends apppTokenType

case class tokenRegEx(regEx: String, tokenType: apppTokenType)


object apppLexer {

  //(regExp,tokenType)
  def identifier = tokenRegEx("[a-zA-Z][a-zA-Z0-9$_*#]*",IDENTIFIER)

  def literalAlpha = tokenRegEx("""(["'])[^"']+\1""",LITERAL_ALPHA)
  def literalInt = tokenRegEx("\\d+",LITERAL_INT)
  def literalBool = tokenRegEx("tt|ff",LITERAL_BOOL)

  def identifierType = tokenRegEx("const|var",IDENTIFIER_TYPE)

  def colon = tokenRegEx(":",COLON)

  def statementSep = tokenRegEx(";|\\n",STATEMENT_SEP)

  def dataType = tokenRegEx("int|bool|alpha",DATA_TYPE)

  def print = tokenRegEx("print",PRINT)

  def whileCondition = tokenRegEx("while", WHILE_CONDITION)
  def doThis = tokenRegEx("do", DO_THIS)

  def ifCondition = tokenRegEx("if",IF_CONDITION)
  def thenDo = tokenRegEx("then",THEN_DO)
  def elseDo = tokenRegEx("else",ELSE_DO)

  def assignmentOp = tokenRegEx("=",ASSIGNMENT_OP)

  def binaryOp = tokenRegEx("\\+|\\*|and|or|\\^|\\/|\\==|\\>|\\<|\\><",BINARY_OP)

  def unaryOp = tokenRegEx("-|not",UNARY_OP)

  def lparen = tokenRegEx("\\(",LPAREN)
  def rparen = tokenRegEx("\\)",RPAREN)

  def candidates = List(statementSep,lparen,rparen,identifierType,colon,print,whileCondition,doThis,ifCondition,thenDo,elseDo,binaryOp,unaryOp,assignmentOp,dataType,literalAlpha,literalBool,literalInt,identifier)

  def find(line: String, candidates: List[tokenRegEx]): (String,apppToken,String) = {
    if (candidates.isEmpty) {
      throw new Exception("no match")
    }

    val regex = candidates.head.regEx
    val tokenType = candidates.head.tokenType

    val matcher=regex.r.pattern.matcher(line)
    if (matcher.find()){
      (
        line.substring(0,matcher.start()),
        apppToken(tokenType,line.substring(matcher.start(),matcher.end())),
        line.substring(matcher.end())
      )
    }else
      find(line,candidates.tail)
  }

  def tokenize(line: String): List[apppToken] = {
    if (line.trim.isEmpty)
      List()
    else{
      val (before,token,after) = find(line,candidates)
      println("Token found: ",token)
      tokenize(before) ::: List(token) ::: tokenize(after)
    }
  }
  
}