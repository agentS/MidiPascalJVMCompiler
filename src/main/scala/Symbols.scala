package at.churchwood.midipascal;

class Symbol()
{
}

case class PlusSymbol() extends Symbol
{
	override def toString(): String = return "+";
}

case class MinusSymbol() extends Symbol
{
	override def toString(): String = return "-";
}

case class MultiplicationSymbol() extends Symbol
{
	override def toString(): String = return "*";
}

case class DivisionSymbol() extends Symbol
{
	override def toString(): String = return "/";
}

case class OpenParenthesisSymbol() extends Symbol
{
	override def toString(): String = return "(";
}

case class ClosingParenthesisSymbol() extends Symbol
{
	override def toString(): String = return ")";
}

case class EndOfFileSymbol() extends Symbol
{
}

case class IdentifierSymbol(var name: String) extends Symbol
{
}

case class NumberSymbol(val number: Int) extends Symbol
{
	override def toString(): String = return number.toString();
}

case class NumberStringSymbol(var numberString: String) extends Symbol
{
}

case class DigitSymbol(val digitString: Char) extends Symbol
{
}

case class BlankSymbol() extends Symbol
{
}

case class ErrorSymbol() extends Symbol
{
}

case class SemicolonSymbol() extends Symbol
{
}

case class PeriodSymbol() extends Symbol
{
}

case class CommaSymbol() extends Symbol
{
}

case class ColonSymbol() extends Symbol
{
}

case class AssignmentSymbol() extends Symbol
{
}

case class CharacterSymbol(val value: Char) extends Symbol
{
}

class ReservedWordSymbol() extends Symbol
{
}

case class ReadSymbol() extends ReservedWordSymbol
{
}

case class WriteSymbol() extends ReservedWordSymbol
{
}

case class ProgramSymbol() extends ReservedWordSymbol
{
}

case class VariableSymbol() extends ReservedWordSymbol
{
}

case class BeginSymbol() extends ReservedWordSymbol
{
}

case class EndSymbol() extends ReservedWordSymbol
{
}

case class IfSymbol() extends ReservedWordSymbol
{
}

case class ThenSymbol() extends ReservedWordSymbol
{
}

case class ElseSymbol() extends ReservedWordSymbol
{
}

case class WhileSymbol() extends ReservedWordSymbol
{
}

case class DoSymbol() extends ReservedWordSymbol
{
}

case class IntegerDataTypeSymbol() extends ReservedWordSymbol
{
}
