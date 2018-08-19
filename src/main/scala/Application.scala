package at.churchwood.midipascal;

object Application
{
	def main(args: Array[String]) =
	{
		val lex: Lexer = new Lexer(args(0));
		val symbols: List[Symbol] = lex.getSymbols();

		/*for { printedSymbol <- symbols }
		{
			println(printedSymbol);
		}*/

		val interpreter: Parser = new Parser(symbols);
		interpreter.processProgram();
	}
}
