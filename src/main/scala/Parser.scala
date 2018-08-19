package at.churchwood.midipascal;

import java.lang.IllegalArgumentException;

import scala.collection.mutable;

import org.apache.bcel.generic.InstructionHandle;

class Parser(val symbols: List[Symbol])
{
	private var actualSymbol: Symbol = null;
	private var symbolIndex: Int = 0;

	private var symbolTable = new mutable.HashMap[String, Int]();
	private var codeGenerator: BytecodeGenerator = new BytecodeGenerator();
	private var variableCounter: Int = 1;

	def processProgram(): Unit =
	{
		variableCounter = 1;

		startExpression();
		codeGenerator.saveClassFile();
	}

	private def startExpression(): Unit =
	{
		symbolIndex = 0;
		actualSymbol = symbols(symbolIndex);
		return miniPascal();
	}

	private def miniPascal(): Unit =
	{
		actualSymbol match
		{
			case ProgramSymbol() =>
			{
				moveToNextSymbol();

				identifier();
				actualSymbol match
				{
					case SemicolonSymbol() =>
					{
						moveToNextSymbol();

						actualSymbol match
						{
							case VariableSymbol() =>
							{
								variableDeclaration();
								codeGenerator.defineVariables(symbolTable);
							}
							case _ => {}
						}
					}
					case _ => throw new IllegalArgumentException("';' expected.");
				}

				actualSymbol match
				{
					case BeginSymbol() =>
					{
						moveToNextSymbol();

						statementSequence();

						actualSymbol match
						{
							case EndSymbol() =>
							{
								moveToNextSymbol();

								actualSymbol match
								{
									case PeriodSymbol() =>
									{
										moveToNextSymbol();
										println("Finished");
									}
									case _ => throw new IllegalArgumentException("'.' expected.");
								}
							}
							case _ => throw new IllegalArgumentException("END expected.");
						}
					}
					case _ => throw new IllegalArgumentException("BEGIN expected.");
				}
			};
			case _ => throw new IllegalArgumentException("PROGRAM expected.");
		}
	}

	private def identifier(): String =
	{
		actualSymbol match
		{
			case id: IdentifierSymbol =>
			{
				moveToNextSymbol();
				return id.name;
			}
			case _ => throw new IllegalArgumentException("identifier expected");
		}
	}

	private def variableDeclaration(): Unit =
	{
		actualSymbol match
		{
			case VariableSymbol() =>
			{
				moveToNextSymbol();
				
				identifierList();

				actualSymbol match
				{
					case ColonSymbol() =>
					{
						moveToNextSymbol();

						actualSymbol match
						{
							case IntegerDataTypeSymbol() =>
							{
								moveToNextSymbol();

								actualSymbol match
								{
									case SemicolonSymbol() =>
									{
										moveToNextSymbol();
									}
									case _ => throw new IllegalArgumentException("';' expected after end of variable declaration.");
								}
							}
							case _ => throw new IllegalArgumentException("INTEGER expected.");
						}
					}
					case _ => throw new IllegalArgumentException("':' expected.");
				}
			}
			case _ => throw new IllegalArgumentException("VAR expected.");
		}
	}

	private def identifierList(): Unit =
	{
		actualSymbol match
		{
			case id: IdentifierSymbol =>
			{
				moveToNextSymbol();

				symbolTable(id.name) = variableCounter;
				variableCounter += 1;
				actualSymbol match
				{
					case CommaSymbol() =>
					{
						moveToNextSymbol();
						identifierList();
					}
					case _ => {}
				}
			}
			case _ => throw new IllegalArgumentException("identifier expected");
		}
	}

	private def statementSequence(): Unit =
	{
		statement();

		actualSymbol match
		{
			case SemicolonSymbol() =>
			{
				moveToNextSymbol();

				statementSequence();
			}
			case EndSymbol() => {}
			case _ => throw new IllegalArgumentException(s"';' expected at the end of a statement, but ${actualSymbol} found");
		}
	}

	private def statement(): Unit =
	{
		actualSymbol match
		{
			case id: IdentifierSymbol =>
			{
				moveToNextSymbol();

				actualSymbol match
				{
					case AssignmentSymbol() =>
					{
						moveToNextSymbol();

						expression();
						codeGenerator.assign(symbolTable(id.name));
					}
					case _ => throw new IllegalArgumentException("assignment operator expected");
				}
			}
			case ReadSymbol() =>
			{
				moveToNextSymbol();

				actualSymbol match
				{
					case OpenParenthesisSymbol() =>
					{
						moveToNextSymbol();

						actualSymbol match
						{
							case id: IdentifierSymbol =>
							{
								codeGenerator.scanln(symbolTable(id.name));
								moveToNextSymbol();

								actualSymbol match
								{
									case ClosingParenthesisSymbol() =>
									{
										moveToNextSymbol();
									}
									case _ => throw new IllegalArgumentException("')' expected");
								}
							}
							case _ => throw new IllegalArgumentException("identifier expected");
						}
					}
					case _ => throw new IllegalArgumentException("'(' expected");
				}
			}
			case WriteSymbol() =>
			{
				moveToNextSymbol();

				actualSymbol match
				{
					case OpenParenthesisSymbol() =>
					{
						moveToNextSymbol();

						actualSymbol match
						{
							case id: IdentifierSymbol =>
							{
								codeGenerator.writeln(symbolTable(id.name));
								moveToNextSymbol();
							}
							case _ =>
							{
								expression();
								//codeGenerator.writeln();
							}
						}

						actualSymbol match
						{
							case ClosingParenthesisSymbol() =>
							{
								moveToNextSymbol();
							}
							case _ => throw new IllegalArgumentException("')' expected");
						}
					}
					case _ => throw new IllegalArgumentException("'(' expected");
				}
			}
			case BeginSymbol() =>
			{
				moveToNextSymbol();

				statementSequence();

				actualSymbol match
				{
					case EndSymbol() =>
					{
						moveToNextSymbol();
					}
					case _ => throw new IllegalArgumentException("END expected at the end of a block.");
				}
			}
			case WhileSymbol() =>
			{
				moveToNextSymbol();

				val loopVariableIndex: Int = symbolTable(identifier());
				codeGenerator.pushVariable(loopVariableIndex);
				val startInstruction: InstructionHandle = codeGenerator.getLatestInstructionHandle();
				
				actualSymbol match
				{
					case DoSymbol() =>
					{
						moveToNextSymbol();

						statement();
						
						codeGenerator.pushVariable(loopVariableIndex);
						codeGenerator.jumpIfNotZero(startInstruction);
						val endInstruction: InstructionHandle = codeGenerator.getLatestInstructionHandle();
						
						codeGenerator.skipBlockIfZero(
							startInstruction.getNext(),
							endInstruction
						);
					}
					case _ => throw new IllegalArgumentException("DO expected at the start of a loop.");
				}
			}
			case IfSymbol() =>
			{
				moveToNextSymbol();

				val ifVariableIndex: Int = symbolTable(identifier());
				codeGenerator.pushVariable(ifVariableIndex);
				var startInstruction: InstructionHandle = codeGenerator.getLatestInstructionHandle();

				actualSymbol match
				{
					case ThenSymbol() =>
					{
						moveToNextSymbol();

						statement();
						var endInstruction: InstructionHandle = codeGenerator.getLatestInstructionHandle();
						codeGenerator.jumpIfExpression(
							startInstruction.getNext(),
							endInstruction
						);
						
						actualSymbol match
						{
							case ElseSymbol() =>
							{
								moveToNextSymbol();

								val startInstructionElseBlock: InstructionHandle =
									codeGenerator.getLatestInstructionHandle();
								statement();
								val endInstructionElseBlock = codeGenerator.getLatestInstructionHandle();
								codeGenerator.jumpElseExpression(
									startInstruction.getNext(),
									ifVariableIndex,
									endInstruction,
									startInstructionElseBlock,
									endInstructionElseBlock
								);
							}
							case _ =>
							{
								codeGenerator.jumpIfWithoutElseExpression(
									startInstruction.getNext(),
									ifVariableIndex,
									endInstruction
								);
							}
						}
					}
					case _ => throw new IllegalArgumentException("THEN expected at the start of a condition.");
				}
			}
			case _ => {}
		}
	}

	private def expression(): Unit =
	{
		term();
		actualSymbol match
		{
			case PlusSymbol() =>
			{
				moveToNextSymbol();

				expression();
				codeGenerator.add();
			}
			case MinusSymbol() => 
			{
				moveToNextSymbol();

				expression();
				codeGenerator.sub();
			}
			case _ =>
			{

			}
		}
	}

	private def term(): Unit =
	{
		factor();
		actualSymbol match
		{
			case MultiplicationSymbol() =>
			{
				moveToNextSymbol();

				term();
				codeGenerator.mul();
			}
			case DivisionSymbol() =>
			{
				moveToNextSymbol();

				term();
				codeGenerator.div();
			}
			case _ => {}
		}
	}

	private def factor(): Unit =
	{
		actualSymbol match
		{
			case number: NumberSymbol =>
			{
				numberLiteral();
			}
			case id: IdentifierSymbol =>
			{
				moveToNextSymbol();

				codeGenerator.pushVariable(symbolTable(id.name));
			}
			case OpenParenthesisSymbol() =>
			{
				moveToNextSymbol();
				expression();
				actualSymbol match
				{
					case ClosingParenthesisSymbol() =>
					{
						moveToNextSymbol();
					}
					case _ => throw new IllegalArgumentException(") expected.");
				}
			};
		}
	}

	private def numberLiteral(): Unit =
	{
		actualSymbol match
		{
			case number: NumberSymbol =>
			{
				moveToNextSymbol();

				codeGenerator.push(number.number);
			}
			case _ => throw new IllegalArgumentException("A number was expected.");
		}
	}

	private def moveToNextSymbol()
	{
		symbolIndex += 1;
		actualSymbol = symbols(symbolIndex);
	}
}
