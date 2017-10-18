package com.talhajavedmukhtar.APPP

sealed trait ASTNode

case class INT_NODE(value: Int) extends ASTNode
case class BOOL_NODE(value: Boolean) extends ASTNode
case class ALPHA_NODE(value: String) extends ASTNode

case class UNARY_OP_NODE(operation: apppToken, operand: ASTNode) extends ASTNode
case class BINARY_OP_NODE(leftOperand: ASTNode, operation: apppToken, rightOperand: ASTNode) extends ASTNode

case class EXPRESSION_NODE(expressionType: String, expression: ASTNode) extends ASTNode

case class IDENTIFIER_NODE(idType: String, dataType: String, name: String) extends ASTNode

case class ASSIGNMENT_NODE(identifier: ASTNode, value: ASTNode) extends ASTNode

case class IF_ELSE_NODE(ifCondition: ASTNode, thenDo: ASTNode, elseDo: ASTNode) extends ASTNode

case class WHILE_NODE(whileCondition: ASTNode, doThis: ASTNode) extends ASTNode

case class PRINT_NODE(toBePrinted: ASTNode) extends ASTNode

case class PROGRAM_NODE(children: List[ASTNode]) extends ASTNode

case class ERROR(errorMessage: String) extends ASTNode

case class NIL_NODE() extends ASTNode


class apppParser(varMap: Map[String,id]) {
  type tokens = List[apppToken]

  //def error(lineNo: Int) = println("Error at line # ", lineNo)
  //def peek(tokens: tokens)

  def convertToBoolean(value: String): Boolean = {
    value match {
      case "tt" => return true
      case "ff" => return false
    }
  }

  def intFactor(tokens: tokens): (ASTNode,tokens) = {
    val currentToken = tokens.head
    if (currentToken.tokenType == LITERAL_INT){
      (INT_NODE(currentToken.value.toInt), tokens.tail)
    }else if(currentToken.tokenType == LPAREN){
      val (expNode,remainingTokens) = intExp(tokens.tail)
      //skip a right parenthesis
      (expNode,remainingTokens.tail)
    }else if(currentToken.tokenType == UNARY_OP){
      val (operandNode,remainingTokens) = intExp(tokens.tail)

      if(operandNode.isInstanceOf[ERROR]){
        (ERROR("Expected to find int expression; not found"),tokens) //if failed to find integer operand
      }else{
        (UNARY_OP_NODE(currentToken,operandNode),remainingTokens)
      }
    }else if(currentToken.tokenType == IDENTIFIER){
      val (idT,dT,nm) = apppExecutor.getIdentifer(varMap(currentToken.value))
      if(dT == "int"){
        return (IDENTIFIER_NODE(idT,dT,nm),tokens.tail)
      }else{
        (ERROR("Expected to find int identifier; not found"),tokens)
      }
    }else{
      (ERROR("Expected to find int literal; not found"),tokens)
    }
  }

  def intTerm(tokens: tokens): (ASTNode, tokens) = {
    val (factorNode: ASTNode, remainingTokens: tokens) = intFactor(tokens)

    if(remainingTokens.isEmpty){
      (factorNode, remainingTokens)
    }else{
      val currentToken = remainingTokens.head

      if (currentToken.value == "*" || currentToken.value == "^"){
        val (rightNode, rem) = intTerm(remainingTokens.tail)
        val node = BINARY_OP_NODE(factorNode,currentToken,rightNode)
        (node,rem)
      }else{
        (factorNode, remainingTokens)
      }
    }

  }

  def intExp(tokens: tokens): (ASTNode, tokens) = {
    val (termNode, remainingTokens) = intTerm(tokens)

    if(remainingTokens.isEmpty || termNode.isInstanceOf[ERROR]){
      (termNode,remainingTokens)
    }else{
      val currentToken = remainingTokens.head

      if(currentToken.value == "+" || currentToken.value == "/"){
        val(rightNode, rem) = intExp(remainingTokens.tail)
        val node = BINARY_OP_NODE(termNode,currentToken,rightNode)
        (node,rem)
      }else{
        (termNode,remainingTokens)
      }
    }
  }

  def boolFactor(tokens: tokens) = {
    val currentToken = tokens.head
    if (currentToken.tokenType == LITERAL_BOOL){
      (BOOL_NODE(convertToBoolean(currentToken.value)), tokens.tail)
    }else if(currentToken.tokenType == LPAREN){
      val (expNode,remainingTokens) = boolExp(tokens.tail)
      //skip a right parenthesis
      (expNode,remainingTokens.tail)
    }else if(currentToken.tokenType == UNARY_OP){
      val (operandNode,remainingTokens) = boolExp(tokens.tail)

      if(operandNode.isInstanceOf[ERROR]){
        (ERROR("Expected to bool expression; not found"),tokens) //if failed to find integer operand
      }else{
        (UNARY_OP_NODE(currentToken,operandNode),remainingTokens)
      }
    }else if(currentToken.tokenType == IDENTIFIER){
      val (idT,dT,nm) = apppExecutor.getIdentifer(varMap(currentToken.value))
      if(dT == "bool"){
        (IDENTIFIER_NODE(idT,dT,nm),tokens.tail)
      }else{
        (ERROR("Expected to find int identifier; not found"),tokens)
      }
    }else{
      (ERROR("Expected to find bool literal; not found"),tokens)
    }
  }

  def boolExp(tokens: tokens): (ASTNode, tokens) = {

    val (factorNode: ASTNode, remainingTokens: tokens) = boolFactor(tokens)

    if (factorNode.isInstanceOf[ERROR]){  //a boolean expression can contain Int boolOp Int
      val (factorNode: ASTNode, remainingTokens: tokens) = intFactor(tokens)

      if(remainingTokens.isEmpty){
        return (factorNode,remainingTokens)
      }

      val currentToken = remainingTokens.head
      if(currentToken.value == "==" || currentToken.value == "<" || currentToken.value == ">" || currentToken.value == "><"){
        val(rightNode, rem) = intExp(remainingTokens.tail)
        val node = BINARY_OP_NODE(factorNode,currentToken,rightNode)
        return (node,rem)
      }else{
        (ERROR("Expected to find boolean expression; expression not found"),remainingTokens)
      }

    }

    if(remainingTokens.isEmpty){
      (factorNode,remainingTokens)
    }else{
      val currentToken = remainingTokens.head
      if(currentToken.value == "and" || currentToken.value == "or" || currentToken.value == "^" || currentToken.value == "==" || currentToken.value == "<" || currentToken.value == ">" || currentToken.value == "><"){
        val(rightNode, rem) = boolExp(remainingTokens.tail)
        val node = BINARY_OP_NODE(factorNode,currentToken,rightNode)
        (node,rem)
      }else{
        (factorNode,remainingTokens)
      }
    }

    //if its not a boolean nor a sign was found
  }


  def alphaExp(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head
    if(currentToken.tokenType == LITERAL_ALPHA){
      val node = ALPHA_NODE(tokens.head.value)
      (node,tokens.tail)
    }else{
      (ERROR("Expected to find alpha literal; not found"),tokens)
    }

  }


  def expr(tokens: tokens): (ASTNode, tokens) = {

    val (node, rem) = alphaExp(tokens)
    if(node.isInstanceOf[ERROR]){  //failed to find alpha expression
      val (node, rem) = boolExp(tokens)
      if(node.isInstanceOf[ERROR]){
        val (node, rem) = intExp(tokens)
        if(node.isInstanceOf[ERROR]){
          (ERROR("Expected to find expression; not found"), tokens)
        }else{
          return (EXPRESSION_NODE("int",node),rem)
        }
      }else{
        return (EXPRESSION_NODE("bool",node),rem)
      }
    }else{
      return (EXPRESSION_NODE("alpha",node),rem)
    }
  }

  def identifier(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head

    if(currentToken.tokenType == IDENTIFIER){  //if just the identifier, it must already have a value
      val (idT,dT,nm) = apppExecutor.getIdentifer(varMap(currentToken.value))
      (IDENTIFIER_NODE(idT,dT,nm), tokens.tail)
    }else if(currentToken.tokenType == IDENTIFIER_TYPE){
      val idT = currentToken.value
      val nm = tokens.tail.head.value
      val rem = tokens.tail.tail.tail
      val dT = rem.head.value
      (IDENTIFIER_NODE(idT,dT,nm), rem.tail)
    }else{
      (ERROR("Expected to find identifer (declaration); not found"),tokens)
    }
  }

  def assignment(tokens: tokens): (ASTNode, tokens) = {
    val (idNode,remainingTokens) = identifier(tokens)
    if(!idNode.isInstanceOf[ERROR]){
      val currentToken = remainingTokens.head
      if (currentToken.tokenType == ASSIGNMENT_OP){
        val (expNode,rem) = expr(remainingTokens.tail)
        if(!expNode.isInstanceOf[ERROR]){
          (ASSIGNMENT_NODE(idNode,expNode),rem)
        }else{
          val (id2Node,rem2) = identifier(remainingTokens.tail)
          if(!id2Node.isInstanceOf[ERROR]){
            (ASSIGNMENT_NODE(idNode,id2Node),rem2)
          }else{
            return (ERROR("Invalid assignment"),tokens)
          }
        }
      }else{
        return (ERROR("Invalid assignment"),tokens)
      }
    }else{
      return (ERROR("Invalid assignment"),tokens)
    }
  }

  def print(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head
    if(currentToken.tokenType == PRINT){
      val (expNode,rem) = expr(tokens.tail)
      if(!expNode.isInstanceOf[ERROR]){
        (PRINT_NODE(expNode),rem)
      }else{
        (ERROR("Invalid argument to print"),tokens)
      }
    }else{
      (ERROR("Expected to find print statement"),tokens)
    }
  }

  def ifElse(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head
    if(currentToken.tokenType == IF_CONDITION){
      val (expNode, rem) = expr(tokens.tail)
      if(!expNode.isInstanceOf[ERROR]){
        if(rem.head.tokenType == THEN_DO){
          val (command1,rem2) = statement(rem.tail)
          if(!command1.isInstanceOf[ERROR]){
            if(rem2.head.tokenType == ELSE_DO){
              val(command2,rem3) = statement(rem2.tail)
              if(!command2.isInstanceOf[ERROR]){
                (IF_ELSE_NODE(expNode,command1,command2),rem2)
              }else{
                (ERROR("Invalid else command"),tokens)
              }
            }else{
              (IF_ELSE_NODE(expNode,command1,NIL_NODE()),rem2)
            }
          }else{
            (ERROR("Invalid then command"),tokens)
          }
        }else{
          (ERROR("Expected to find a then command"),tokens)
        }
      }else{
        (ERROR("Invalid expression to if statement"),tokens)
      }
    }else{
      (ERROR("Expected to find if statement"),tokens)
    }
  }

  def whileDo(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head

    if(currentToken.tokenType == WHILE_CONDITION){
      val (expNode, rem) = expr(tokens.tail)
      if(!expNode.isInstanceOf[ERROR]){
        if(rem.head.tokenType == DO_THIS){
          val (command, rem2) = statement(rem.tail)
          if(!command.isInstanceOf[ERROR]){
            (WHILE_NODE(expNode,command),rem2)
          }else{
            (ERROR("Invalid command to do"),tokens)
          }
        }else{
          (ERROR("Expected to find do statement"),tokens)
        }
      }else{
        (ERROR("Invalid expression to while statement"),tokens)
      }
    }else{
      (ERROR("Expected to find while statement"),tokens)
    }

  }

  def statement(tokens: tokens): (ASTNode, tokens) = {
    val (idAssign,rem) = assignment(tokens)
    if(idAssign.isInstanceOf[ERROR]){
      val (idDecl,rem2) = identifier(tokens)
      if(idDecl.isInstanceOf[ERROR]){
        val (ifElseNode, rem3) = ifElse(tokens)
        if(ifElseNode.isInstanceOf[ERROR]){
          val (whileNode, rem4) = whileDo(tokens)
          if(whileNode.isInstanceOf[ERROR]){
            val (printNode, rem5) = print(tokens)
            if(printNode.isInstanceOf[ERROR]){
              val (expNode, rem6) = expr(tokens)
              if(expNode.isInstanceOf[ERROR]){
                (ERROR("Invalid statement"),tokens)
              }else{
                (expNode, rem6)
              }
            }else{
              (printNode,rem5)
            }
          }else{
            (whileNode, rem4)
          }
        }else{
          (ifElseNode, rem3)
        }
      }else{
        (idDecl,rem2)
      }
    }else{
      (idAssign,rem)
    }
  }

  def statements(tokens: tokens, lineNo: Int=0): List[ASTNode] = {
    if(tokens.isEmpty){
      List()
    }else{
      if (tokens.head.tokenType == STATEMENT_SEP){
        statements(tokens.tail)
      }else{
        val (stmnt,rem) = statement(tokens)
        if(stmnt.isInstanceOf[ERROR]){
          println("ERROR detected at line ", lineNo, stmnt.asInstanceOf[ERROR].errorMessage)
          List()
        }else{
          List(stmnt) ::: statements(rem,lineNo+1)
        }
      }
    }
  }

  def program(tokens: tokens): ASTNode = {
    val stmnts = statements(tokens)
    PROGRAM_NODE(stmnts)
  }

  /*def variable(tokens: tokens): (ASTNode, tokens) = {
    val currentToken = tokens.head

    if(currentToken.tokenType == IDENTIFIER){
      return (VAR_NODE(),tokens.tail)
    }
  }*/



  //add unary operations
  //add variables
  //add assignment

}
