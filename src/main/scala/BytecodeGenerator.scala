package at.churchwood.midipascal;

import org.apache.bcel.{ Const };
import org.apache.bcel.classfile.{ JavaClass, Method };
import org.apache.bcel.generic.{
	ClassGen, MethodGen,
	InstructionList, InstructionHandle, Instruction, ConstantPoolGen, InstructionFactory,
	Type, BasicType, ObjectType, ArrayType,
	InstructionConstants, NOP, GOTO, PUSH, POP, RETURN, GETSTATIC, LDC, INVOKEVIRTUAL,
	SIPUSH, ISTORE, ILOAD, ASTORE, ALOAD,
	IADD, ISUB, IMUL, IDIV,
	IFNE, IFEQ
};

import scala.collection.mutable;

object BytecodeGenerator
{
	val outputStream: ObjectType = new ObjectType("java.io.PrintStream");
	val inputStream: ObjectType = new ObjectType("java.io.InputStream");

	val INDEX_CONOSLE_READER: Int = 0;
}

class BytecodeGenerator
{
	private var classFactory: ClassGen = null;
	private var mainConstantPool: ConstantPoolGen = null;

	// JVM tutorial: https://commons.apache.org/proper/commons-bcel/manual/jvm.html
	classFactory = new ClassGen(
		"MidiPascal",
		"java.lang.Object",
		null,
		Const.ACC_PUBLIC | Const.ACC_SUPER,
		Array()
	);
	
	mainConstantPool = classFactory.getConstantPool();
	private var instructionCreator: InstructionFactory = new InstructionFactory(classFactory);
	private var mainMethod = generateMainMethod();

	private def generateMainMethod(): MethodGen =
	{
		val instructions: InstructionList = new InstructionList();

		mainConstantPool.addNameAndType("main", "([java/lang/String;)V");
		mainConstantPool.addClass("java/io/PrintStream");
		mainConstantPool.addClass("java/util/Scanner");

		addConsoleReader(instructions);

		val methodArgumentNames = Array("args");
		val methodArgumentTypes: Array[Type] = Array(new ArrayType(Type.STRING, 1));
		val mainMethod: MethodGen = new MethodGen(
			Const.ACC_PUBLIC | Const.ACC_STATIC,
			Type.VOID,
			methodArgumentTypes,
			methodArgumentNames,
			"main", "MidiPascal",
			instructions,
			mainConstantPool
		);

		return mainMethod;
	}

	private def addConsoleReader(instructions: InstructionList): Unit =
	{
		instructions.append(instructionCreator.createNew("java.util.Scanner"));
		instructions.append(InstructionConstants.DUP);
		instructions.append(instructionCreator.createFieldAccess(
			"java.lang.System", "in", BytecodeGenerator.inputStream, Const.GETSTATIC
		));
		instructions.append(instructionCreator.createInvoke(
			"java.util.Scanner", "<init>",
			Type.VOID, Array(BytecodeGenerator.inputStream),
			Const.INVOKESPECIAL
		));
		instructions.append(new ASTORE(BytecodeGenerator.INDEX_CONOSLE_READER));
	}

	def defineVariables(variables: mutable.Map[String, Int]): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();
		for { (name, index) <- variables }
		{
			instructions.append(new SIPUSH(0));
			instructions.append(new ISTORE(index));
		}

		mainMethod.setMaxLocals();
	}

	def writeln(text: String): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(
			instructionCreator.createFieldAccess("java.lang.System", "out", BytecodeGenerator.outputStream, Const.GETSTATIC)
		);
		instructions.append(new PUSH(mainConstantPool, text + "\n"));
		instructions.append(instructionCreator.createInvoke(
			"java.io.PrintStream", "println", Type.VOID,
			Array[Type](Type.STRING),
			Const.INVOKEVIRTUAL
		));
	}

	def writeln(variableIndex: Int): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(
			instructionCreator.createFieldAccess("java.lang.System", "out", BytecodeGenerator.outputStream, Const.GETSTATIC)
		);
		instructions.append(new ILOAD(variableIndex));
		instructions.append(instructionCreator.createInvoke(
			"java.io.PrintStream", "println", Type.VOID,
			Array[Type](Type.INT),
			Const.INVOKEVIRTUAL
		));
	}

	def writeln(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(
			instructionCreator.createFieldAccess("java.lang.System", "out", BytecodeGenerator.outputStream, Const.GETSTATIC)
		);
		instructions.append(instructionCreator.createInvoke(
			"java.io.PrintStream", "println", Type.VOID,
			Array[Type](Type.INT),
			Const.INVOKEVIRTUAL
		));
	}

	def scanln(variableIndex: Int): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new ALOAD(BytecodeGenerator.INDEX_CONOSLE_READER));
		instructions.append(instructionCreator.createInvoke(
			"java.util.Scanner", "nextInt",
			Type.INT, Type.NO_ARGS,
			Const.INVOKEVIRTUAL
		));
		instructions.append(new ISTORE(variableIndex));
	}

	def add(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new IADD());
	}

	def sub(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new ISUB());
	}

	def mul(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new IMUL());
	}

	def div(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new IDIV());
	}

	def push(number: Int): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		mainConstantPool.addInteger(number);
		val constatPoolIndex: Int = mainConstantPool.lookupInteger(number);
		instructions.append(new LDC(constatPoolIndex));
	}

	def pushVariable(index: Int): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new ILOAD(index));
	}

	def pop(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new POP());
	}

	def assign(index: Int): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new ISTORE(index));
	}

	def jump(target: InstructionHandle): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new GOTO(target));
	}

	def jumpIfZero(startInstruction: InstructionHandle): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new IFEQ(startInstruction));
	}

	def jumpIfNotZero(startInstruction: InstructionHandle): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(new IFNE(startInstruction));
	}

	def skipBlockIfZero(
		optionalBlock: InstructionHandle,
		nextInstruction: InstructionHandle
	): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(nextInstruction, new NOP());		
		instructions.insert(optionalBlock, new IFEQ(nextInstruction.getNext()));
	}

	def jumpIfExpression(
		firstInstruction: InstructionHandle,
		lastInstruction: InstructionHandle
	): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(lastInstruction, new NOP());
		instructions.insert(firstInstruction, new IFNE(firstInstruction));
	}

	def jumpElseExpression(
		ifCheckInstruction: InstructionHandle,
		conditionVariableIndex: Int,
		lastInstructionIfBlock: InstructionHandle,
		elseBlock: InstructionHandle,
		lastInstructionElseBlock: InstructionHandle
	): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();

		instructions.append(ifCheckInstruction, new ILOAD(conditionVariableIndex));
		instructions.append(ifCheckInstruction.getNext(), new IFEQ(elseBlock));
		instructions.append(lastInstructionElseBlock, new NOP());
		instructions.append(lastInstructionIfBlock, new GOTO(lastInstructionElseBlock.getNext()));
	}

	def jumpIfWithoutElseExpression(
		ifCheckInstruction: InstructionHandle,
		conditionVariableIndex: Int,
		lastInstruction: InstructionHandle
	): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();
		instructions.append(ifCheckInstruction, new ILOAD(conditionVariableIndex));
		instructions.append(ifCheckInstruction.getNext(), new IFEQ(lastInstruction.getNext()));
	}

	def getLatestInstructionHandle(): InstructionHandle =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();
		return instructions.getEnd();
	}

	def addNoOperation(): Unit =
	{
		val instructions: InstructionList = mainMethod.getInstructionList();
		instructions.append(new NOP());
	}

	def saveClassFile(): Unit =
	{
		mainMethod.getInstructionList().append(new RETURN());
		mainMethod.setMaxStack();
		classFactory.addMethod(mainMethod.getMethod());

		printBytecode();

		val classFile: JavaClass = classFactory.getJavaClass();
		classFile.dump("MidiPascal.class");

		//println(classFile.toString());
	}

	def printBytecode()
	{
		val instructions: InstructionList = mainMethod.getInstructionList();
		println(instructions.toString(true));
	}
}