package at.churchwood.midipascal;

import java.lang.IllegalArgumentException;
import scala.collection.mutable.ListBuffer;

import java.io.FileReader;
import java.io.BufferedReader;

object Lexer
{
	val REGEX_DIGIT = "[0-9]".r();
	val REGEX_CHARACTER = "[a-zA-Z]".r();
	val REGEX_LOWER_CASE_CHARACTER = "[a-z]".r();
}

class Lexer(val sourceFilePath: String)
{
	val sourceFileReader: BufferedReader = new BufferedReader(new FileReader(sourceFilePath));

	def getSymbols(): List[Symbol] =
	{
		var symbols: ListBuffer[Symbol] = ListBuffer[Symbol]();
		var previousSymbol: Symbol = new BlankSymbol();
		var processedNumberSymbol: NumberStringSymbol = new NumberStringSymbol("");
		var processedIdentifierSymbol: IdentifierSymbol = null;
		var character: Char = 0;
		var characterIndex: Int = (-1);
		var characterLineNumber: Int = (-1);

		var line: String = sourceFileReader.readLine();
		while (line != null)
		{
			for (character <- line)
			{
				characterIndex += 1;
				val newSymbol: Symbol = getNextSymbol(character);

				newSymbol match
				{
					case BlankSymbol() =>
					{
						previousSymbol = newSymbol;
					}
					case digit: DigitSymbol =>
					{
						processedNumberSymbol.numberString += digit.digitString.toString;
						previousSymbol = newSymbol;
					}
					case character: CharacterSymbol =>
					{
						previousSymbol match
						{
							case BeginSymbol() =>
							{
								character.value match
								{
									case 'N' => previousSymbol = BlankSymbol();
									case _ => {}
								}
							}
							case s: ReservedWordSymbol =>
							{
								character.value match
								{
									case Lexer.REGEX_LOWER_CASE_CHARACTER() =>
									{
										processedIdentifierSymbol = new IdentifierSymbol(character.value.toString());
										symbols += processedIdentifierSymbol;
										previousSymbol = processedIdentifierSymbol;
									}
									case 'H' =>
									{
										previousSymbol match
										{
											case WriteSymbol() =>
											{
												val newSymbol: ReservedWordSymbol = new WhileSymbol();
												symbols.remove(symbols.length - 1);
												symbols += newSymbol;
												previousSymbol = newSymbol;
											}
											case Lexer.REGEX_LOWER_CASE_CHARACTER() =>
											{
												processedIdentifierSymbol = new IdentifierSymbol(character.value.toString());
												symbols += processedIdentifierSymbol;
												previousSymbol = processedIdentifierSymbol;
											}
											case _ => {}
										}
									}
									case 'F' =>
									{
										previousSymbol match
										{
											case IntegerDataTypeSymbol() =>
											{
												val newSymbol: ReservedWordSymbol = new IfSymbol();
												symbols.remove(symbols.length - 1);
												symbols += newSymbol;
												previousSymbol = newSymbol;
											}
											case Lexer.REGEX_LOWER_CASE_CHARACTER() =>
											{
												processedIdentifierSymbol = new IdentifierSymbol(character.value.toString());
												symbols += processedIdentifierSymbol;
												previousSymbol = processedIdentifierSymbol;
											}
											case _ => {}
										}
									}
									case 'L' =>
									{
										previousSymbol match
										{
											case EndSymbol() =>
											{
												val newSymbol: ReservedWordSymbol = new ElseSymbol();
												symbols.remove(symbols.length - 1);
												symbols += newSymbol;
												previousSymbol = newSymbol;
											}
											case Lexer.REGEX_LOWER_CASE_CHARACTER() =>
											{
												processedIdentifierSymbol = new IdentifierSymbol(character.value.toString());
												symbols += processedIdentifierSymbol;
												previousSymbol = processedIdentifierSymbol;
											}
											case _ => {}
										}
									}
									case _ => {}
								}
							}
							case identifier: IdentifierSymbol =>
							{
								processedIdentifierSymbol.name += character.value;
							}
							case _ =>
							{
								var detectedSymbol: Symbol = null;
								character.value match
								{
									case 'P' =>
									{
										detectedSymbol = new ProgramSymbol();
									}
									case 'R' =>
									{
										detectedSymbol = new ReadSymbol();
									}
									case 'W' =>
									{
										detectedSymbol = new WriteSymbol();
									}
									case 'V' =>
									{
										detectedSymbol = new VariableSymbol();
									}
									case 'B' =>
									{
										detectedSymbol = new BeginSymbol();
									}
									case 'E' =>
									{
										detectedSymbol = new EndSymbol();
									}
									case 'I' =>
									{
										detectedSymbol = new IntegerDataTypeSymbol();
									}
									case 'D' =>
									{
										detectedSymbol = new DoSymbol();
									}
									case 'T' =>
									{
										detectedSymbol = new ThenSymbol();
									}
									case Lexer.REGEX_LOWER_CASE_CHARACTER() =>
									{
										processedIdentifierSymbol = new IdentifierSymbol(character.value.toString());
										detectedSymbol = processedIdentifierSymbol;
									}
								}

								symbols += detectedSymbol;
								previousSymbol = detectedSymbol;
							}
						}
					}
					case _ =>
					{
						previousSymbol match
						{
							case previousDigit: DigitSymbol =>
							{
								newSymbol match
								{
									case actualDigit: DigitSymbol => {}
									case _ =>
									{
										symbols += new NumberSymbol(processedNumberSymbol.numberString.toInt);
										processedNumberSymbol = new NumberStringSymbol("");
									}
								}
							}
							case ColonSymbol() =>
							{
								newSymbol match
								{
									case AssignmentSymbol() =>
									{
										// remove the previous colon symbol
										symbols.remove(symbols.length - 1);
									}
									case _ => {}
								}
							}
							case _ => {}
						}

						symbols += newSymbol;
						previousSymbol = newSymbol;
					}
				}
			}

			characterLineNumber += 1;
			line = sourceFileReader.readLine();
		}

		previousSymbol match
		{
			case actualDigit: DigitSymbol =>
			{
				symbols += new NumberSymbol(processedNumberSymbol.numberString.toInt);
				processedNumberSymbol = new NumberStringSymbol("");
			}
			case _ => {}
		}

		sourceFileReader.close();
		symbols += new EndOfFileSymbol();
		return symbols.toList;
	}

	private def getNextSymbol(character: Char): Symbol =
	{
		character match
		{
			case '+' => return new PlusSymbol();
			case '-' => return new MinusSymbol();
			case '*' => return new MultiplicationSymbol();
			case '/' => return new DivisionSymbol();
			case '(' => return new OpenParenthesisSymbol();
			case ')' => return new ClosingParenthesisSymbol();
			case Lexer.REGEX_DIGIT() =>
			{
				return new DigitSymbol(character);
			}
			case Lexer.REGEX_CHARACTER() =>
			{
				return new CharacterSymbol(character);
			}
			case ';' => return new SemicolonSymbol();
			case ':' => return new ColonSymbol();
			case ',' => return new CommaSymbol();
			case '.' => return new PeriodSymbol();
			case '=' => return new AssignmentSymbol();
			case '\n' | '\r' | '\t' | ' ' => return new BlankSymbol();
			case _ => throw new IllegalArgumentException("Invalid character " + character);
		}
	}
}
