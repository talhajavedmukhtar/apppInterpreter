package com.talhajavedmukhtar.APPP

case class id(idType: String, dataType: String, name: String, value: Any)

object apppExecutor {

  def getIdentifer(idntfr: id) = (idntfr.idType,idntfr.dataType,idntfr.name)

  def execute(astTree: ASTNode) = {
    astTree match {
      case i: INT_NODE => i.value

      case b: BOOL_NODE => b.value

      case a: ALPHA_NODE => a.value

      case bOp: BINARY_OP_NODE => {}
    }
  }

  /*def executeIntBinaryOp(bOp: BINARY_OP_NODE) = {
    bOp.operation match {
      case + => executeOperand(bOp.leftOperand) + executeOperand(bOp.rightOperand)
    }
  }

  def executeIntOperand(operand: ASTNode): Int = {
    operand match {
      case i: INT_NODE => i.value

      case _ => executeBinaryOp(_)
    }
  }*/
}
