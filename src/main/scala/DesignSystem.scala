object DesignSystem {
  sealed trait Token

  object Token {
    sealed trait FontType
    object FontType {
      case object Normal100    extends FontType
      case object Normal300    extends FontType
      case object Normal400    extends FontType
      case object Normal500    extends FontType
      case object Normal700    extends FontType
      case object Normal900    extends FontType
      case object Condensed300 extends FontType
      case object Condensed400 extends FontType
      case object Condensed700 extends FontType
    }

    sealed trait Color
  }
}
