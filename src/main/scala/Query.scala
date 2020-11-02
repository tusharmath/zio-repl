object GQLQuery {
  sealed trait Node
  case class Complex(value: List[ComplexValue]) extends Node
  case class Scalar(value: ScalarValue)         extends Node

  case class ComplexValue(key: String, node: Node)

  sealed trait ScalarValue
  case class ScalarInt(int: Int)          extends ScalarValue
  case class ScalarString(string: String) extends ScalarValue

  
}
