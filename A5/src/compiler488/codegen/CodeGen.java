package compiler488.codegen;

import java.io.*;
import java.util.*;

import compiler488.ast.AST;
import compiler488.ast.ASTList;
import compiler488.ast.ASTVisitor;
import compiler488.ast.Printable;
import compiler488.ast.decl.ArrayDeclPart;
import compiler488.ast.decl.Declaration;
import compiler488.ast.decl.MultiDeclarations;
import compiler488.ast.decl.RoutineDecl;
import compiler488.ast.decl.ScalarDecl;
import compiler488.ast.expn.AnonFuncExpn;
import compiler488.ast.expn.ArithExpn;
import compiler488.ast.expn.BinaryExpn;
import compiler488.ast.expn.BoolConstExpn;
import compiler488.ast.expn.BoolExpn;
import compiler488.ast.expn.CompareExpn;
import compiler488.ast.expn.ConstExpn;
import compiler488.ast.expn.EqualsExpn;
import compiler488.ast.expn.Expn;
import compiler488.ast.expn.FunctionCallExpn;
import compiler488.ast.expn.IdentExpn;
import compiler488.ast.expn.IntConstExpn;
import compiler488.ast.expn.NotExpn;
import compiler488.ast.expn.SkipConstExpn;
import compiler488.ast.expn.SubsExpn;
import compiler488.ast.expn.TextConstExpn;
import compiler488.ast.expn.UnaryExpn;
import compiler488.ast.expn.UnaryMinusExpn;
import compiler488.ast.stmt.AssignStmt;
import compiler488.ast.stmt.ExitStmt;
import compiler488.ast.stmt.GetStmt;
import compiler488.ast.stmt.IfStmt;
import compiler488.ast.stmt.LoopStmt;
import compiler488.ast.stmt.LoopingStmt;
import compiler488.ast.stmt.ProcedureCallStmt;
import compiler488.ast.stmt.Program;
import compiler488.ast.stmt.PutStmt;
import compiler488.ast.stmt.ReturnStmt;
import compiler488.ast.stmt.Scope;
import compiler488.ast.stmt.Stmt;
import compiler488.ast.stmt.WhileDoStmt;
import compiler488.ast.type.BooleanType;
import compiler488.ast.type.IntegerType;
import compiler488.compiler.Main;
import compiler488.runtime.Machine;
import compiler488.runtime.MemoryAddressException;
import compiler488.runtime.ExecutionException;
import compiler488.symbol.MajorScope;
import compiler488.symbol.SymbolTableEntry;
import compiler488.symbol.SymbolTableEntry.SymbolKind;

/**
 * CodeGenerator.java
 *
 * <pre>
 *  Code Generation Conventions
 * 
 *  To simplify the course project, this code generator is
 *  designed to compile directly to pseudo machine memory
 *  which is available as the private array memory[]
 * 
 *  It is assumed that the code generator places instructions
 *  in memory in locations
 * 
 *      memory[ 0 .. startMSP - 1 ]
 * 
 *  The code generator may also place instructions and/or
 *  constants in high memory at locations (though this may
 *  not be necessary)
 *      memory[ startMLP .. Machine.memorySize - 1 ]
 * 
 *  During program exection the memory area
 *      memory[ startMSP .. startMLP - 1 ]
 *  is used as a dynamic stack for storing activation records
 *  and temporaries used during expression evaluation.
 *  A hardware exception (stack overflow) occurs if the pointer
 *  for this stack reaches the memory limit register (mlp).
 * 
 *  The code generator is responsible for setting the global
 *  variables:
 *      startPC         initial value for program counter
 *      startMSP        initial value for msp
 *      startMLP        initial value for mlp
 * </pre>
 * 
 * @author <B> Benson Quach (g0quachb) Eric Chen (g3chencm) Eric Snyder
 *         (g4snyder) Francesco Gramano (g2graman) Nicholas Dujay (c4dujayn)
 *         Winston Yeung (g2yeungw) </B>
 */

public class CodeGen implements ASTVisitor<Void> {

	/** initial value for memory stack pointer */
	private short startMSP;
	/** initial value for program counter */
	private short startPC;
	/** initial value for memory limit pointer */
	private short startMLP;

	/** flag for tracing code generation */
	private boolean traceCodeGen = Main.traceCodeGen;

	private short generationAddress;

	private MajorScope currentScope;

	// addresses for the PRINT_STR function
	private short BEGIN_PRINT_STR;
	private short END_PRINT_STR;

	/**
	 * A List of local functions pending code generation for each function
	 * being generated.
	 *
	 * While a function may be done generating its code, it may not have
	 * generated the code for all local functions, meaning the function still
	 * has a list of local functions here.
	 */ 
	private List<List<RoutineDecl>> pendingRoutines =
		new ArrayList<List<RoutineDecl>>();

	/**
	 * List of addresses containing values to be patched to the loop end; one
	 * list for each open loop.
	 */
	private List<List<Integer>> loopPatchAddresses =
		new ArrayList<List<Integer>>();
	

	private List<Short> returnPatchAddresses =
		new ArrayList<Short>();
	
	/**
	 * List of addresses containing values to be patched to routine starts; one
	 * list for each open routine.
	 */
	private List<List<RoutinePatchEntry>> routinePatchAddresses =
		new ArrayList<List<RoutinePatchEntry>>();
	private class RoutinePatchEntry {
		public RoutinePatchEntry(
				SymbolTableEntry routineSymbol
				, short memoryAddress) {
			this.routineSymbol = routineSymbol;
			this.memoryAddress = memoryAddress;
		}
 
		/** Routine to patch reference to. */
		public SymbolTableEntry routineSymbol;
		/** Memory of reference to patch. */
		public short memoryAddress;
	}
	
	/**
	 * Constructor to initialize code generation
	 */
	public CodeGen() {
		// YOUR CONSTRUCTOR GOES HERE.
		generationAddress = 0;
	}

	// Utility procedures used for code generation GO HERE.

	/**
	 * Additional intialization for gode generation. Called once at the start of
	 * code generation. May be unnecesary if constructor does everything.
	 */

	/** Additional initialization for Code Generation (if required) */
	void Initialize() {
		/********************************************************/
		/* Initialization code for the code generator GOES HERE */
		/* This procedure is called once before codeGeneration */
		/*                                                      */
		/********************************************************/

		return;
	}

	/**
	 * Perform any requred cleanup at the end of code generation. Called once at
	 * the end of code generation.
	 * 
	 * @throws MemoryAddressException
	 *             from Machine.writeMemory
	 * @throws ExecutionException
	 *             from Machine.writeMemory
	 */
	void Finalize() throws MemoryAddressException, ExecutionException // from
																		// Machine.writeMemory
	{
		/********************************************************/
		/* Finalization code for the code generator GOES HERE. */
		/*                                                      */
		/* This procedure is called once at the end of code */
		/* generation */
		/********************************************************/

		Machine.setPC((short) END_PRINT_STR); /* where code to be executed begins */
		Machine.setMSP(generationAddress); /* where memory stack begins */
		Machine.setMLP((short) (Machine.memorySize - 1));

		return;
	}

	/**
	 * Procedure to implement code generation based on code generation action
	 * number
	 * 
	 * @param actionNumber
	 *            code generation action to perform
	 */
	void generateCode(int actionNumber) {
		if (traceCodeGen) {
			// output the standard trace stream
			Main.traceStream.println("CodeGen: C" + actionNumber);
		}

		/****************************************************************/
		/* Code to implement the code generation actions GOES HERE */
		/* This dummy code generator just prints the actionNumber */
		/* In Assignment 5, you'll implement something more interesting */
		/*                                                               */
		/* FEEL FREE TO ignore or replace this procedure */
		/****************************************************************/

		System.out.println("Codegen: C" + actionNumber);
		return;
	}

	// ADDITIONAL FUNCTIONS TO IMPLEMENT CODE GENERATION GO HERE

	// helper functions for each machine instruction

	private void outputExecutionError(ExecutionException e) {
		String msg = String.format("EXECUTION ERROR: %s", e);
	}

	private void outputMemoryAddressError(MemoryAddressException e) {
		String msg = String.format("MEMORY ERROR: %s", e);
	}

	private void assembly(short opcode, short... args) {
		try {
			Machine.writeMemory(generationAddress++, opcode);
			for (short s : args) {
				Machine.writeMemory(generationAddress++, s);
			}
		} catch (MemoryAddressException e) {
			outputMemoryAddressError(e);
		} catch (ExecutionException e) {
			outputExecutionError(e);
		}
	}

	private void HALT() {
		assembly(Machine.HALT);
	}

	private void ADDR(short LL, short ON) {
		assembly(Machine.ADDR, LL, ON);
	}

	private void ADDR(int LL, short ON) {
		assembly(Machine.ADDR, (short)LL, ON);
	}

	private void ADDR(Integer LL, Integer ON) {
		ADDR(LL.shortValue(), ON.shortValue());
	}

	private void LOAD() {
		assembly(Machine.LOAD);
	}

	private void STORE() {
		assembly(Machine.STORE);
	}

	private void PUSH(short V) {
		assembly(Machine.PUSH, V);
	}

	private void PUSH(char c) {
		PUSH(new Integer(c).shortValue());
	}

	private void PUSH(int i) {
		PUSH(new Integer(i).shortValue());
	}

	private void PUSHMT() {
		assembly(Machine.PUSHMT);
	}

	private void SETD(short LL) {
		assembly(Machine.SETD, LL);
	}

	private void SETD(int LL) {
		SETD(new Integer(LL).shortValue());
	}

	private void POP() {
		assembly(Machine.POP);
	}

	private void POPN() {
		assembly(Machine.POPN);
	}

	private void DUP() {
		assembly(Machine.DUP);
	}

	private void DUPN() {
		assembly(Machine.DUPN);
	}

	private void BR() {
		assembly(Machine.BR);
	}

	private void BF() {
		assembly(Machine.BF);
	}

	private void NEG() {
		assembly(Machine.NEG);
	}

	private void ADD() {
		assembly(Machine.ADD);
	}

	private void SUB() {
		assembly(Machine.SUB);
	}

	private void MUL() {
		assembly(Machine.MUL);
	}

	private void DIV() {
		assembly(Machine.DIV);
	}

	private void EQ() {
		assembly(Machine.EQ);
	}

	private void LT() {
		assembly(Machine.LT);
	}

	private void OR() {
		assembly(Machine.OR);
	}

	private void SWAP() {
		assembly(Machine.SWAP);
	}

	private void READC() {
		assembly(Machine.READC);
	}

	private void READI() {
		assembly(Machine.READI);
	}

	private void PRINTC() {
		assembly(Machine.PRINTC);
	}

	private void PRINTI() {
		assembly(Machine.PRINTI);
	}

	private void TRON() {
		assembly(Machine.TRON);
	}

	private void TROFF() {
		assembly(Machine.TROFF);
	}

	private void ILIMIT(short V) {
		assembly(Machine.ILIMIT, V);
	}

	private void patchAddress(short addr, short newAddress) {
		try {
			Machine.writeMemory(addr, newAddress);
		} catch (MemoryAddressException e) {
			outputMemoryAddressError(e);
		} catch (ExecutionException e) {
			outputExecutionError(e);
		}
	}

	// simple helper function to map to our design document closer
	private void CODEGEN(AST a) {
		a.accept(this);
	}

	private void CODEGEN_ADDR(Expn e) {
		if (e instanceof IdentExpn) {
			CODEGEN_ADDR((IdentExpn) e);
		} else if (e instanceof SubsExpn) {
			CODEGEN_ADDR((SubsExpn) e);
		}
	}
	
	private SymbolTableEntry lookThroughAncestors(String ident) {
		SymbolTableEntry e = currentScope.lookup(ident);
		
		MajorScope ancestor = currentScope.getParent();
		while(ancestor != null &&
				e == null) {
			e = ancestor.lookup(ident);
			ancestor = ancestor.getParent();
		}
		
		return e;
	}

	private void CODEGEN_ADDR(IdentExpn expn) {
		SymbolTableEntry e = lookThroughAncestors(expn.getIdent());
		
		// look up the scopes
		ADDR(e.getLexicalLevel(), e.getOffset());
	}

	private void CODEGEN_ADDR(SubsExpn expn) {

		SymbolTableEntry e = lookThroughAncestors(expn.getVariable());
		
		Expn expr1 = expn.getSubscript1();
		Expn expr2 = expn.getSubscript2();
		
		ArrayDeclPart decl = (ArrayDeclPart) e.getNode();
		int lowerBound1 = decl.getLowerBoundary1();

		if (decl.isTwoDimensional()) {
			/* 2-D Array */
			int upperBound2 = decl.getUpperBoundary2();
			int lowerBound2 = decl.getLowerBoundary2();
			
			PUSH(upperBound2 - lowerBound2 + 1);
			
			CODEGEN(expr1);  // (expr1 - LB1 + 1)
			PUSH(lowerBound1); // 0 indexed
			SUB();
			
			MUL(); // (UB2 - LB2 + 1) * (expr1 - LB1 + 1)
			
			CODEGEN(expr2); 
			PUSH(lowerBound2);
			SUB(); // (expr2 - LB2)
			
			ADD(); 
			
			// get the address of the start of the array
			ADDR(e.getLexicalLevel(), e.getOffset());
			ADD();
		} else {
			/* 1-D Array */
			CODEGEN(expr1); 
			PUSH(lowerBound1);
			SUB();
			
			// get the address of the start of the array
			ADDR(e.getLexicalLevel(), e.getOffset());
			ADD();
		}
		
		// top of the stack is now the address of the element
	}

	@Override
	public Void visit(AST node) {
		// Handled in subcases.
		return null;
	}

	@Override
	public <A extends AST> Void visit(ASTList<A> list) {
		for (A a : list) {
			CODEGEN(a);
		}

		return null;
	}

	@Override
	public Void visit(ArrayDeclPart decl) { return null; }

	@Override 
	public Void visit(Declaration decl) { return null; }

	@Override
	public Void visit(MultiDeclarations decl) { return null; }

	@Override
	public Void visit(RoutineDecl decl) {
		// Mark routine for codegen.
		pendingRoutines.get(pendingRoutines.size()-1).add(decl);
		// See the Program visitor or pendingRoutines for more info.
		return null;
	}

	@Override
	public Void visit(ScalarDecl decl) { return null; }

	@Override
	public Void visit(AnonFuncExpn expn) {
		// return value
		PUSH(Machine.UNDEFINED);
		
		// return address
		PUSH(Machine.UNDEFINED);
		short returnAddress = (short)(generationAddress - 1);

		// Save dynamic link
		ADDR(currentScope.getLexicalLevel(), 0); // WRONG

		// Allocate storage for variables.
		if(expn.getScope().getVariableSize() > 0) {
			PUSH(Machine.UNDEFINED);
			PUSH(expn.getScope().getVariableSize());
			DUPN();
		}
		
		// save display register
		ADDR(currentScope.getLexicalLevel(), 0);

		// set the display register of the anonymous functions lexical level
		PUSHMT();
		PUSH(expn.getScope().getVariableSize() + 3);
		SUB();
		
		// set the scope correctly
		MajorScope before = currentScope;
		currentScope = expn.getScope();
		
		// use the expressions scope
		SETD(currentScope.getLexicalLevel());
		
		// generate the body code
		CODEGEN(expn.getBody());
		
		// generate the yield expression
		CODEGEN(expn.getExpn());
		
		// Exit code
		// store the return value at the top of the stack, below the activation record
		ADDR(currentScope.getLexicalLevel(), -1);
		SWAP();
		STORE();

		// Reset display register.
		SETD(currentScope.getLexicalLevel());
		
		// de allocate
		if(currentScope.getVariableSize() > 0) {
			PUSH(currentScope.getVariableSize());
			POPN();
		}
		
		// remove dynamic link
		POP();
		
		// return to return address
		BR();
		
		// reset the scope
		currentScope = before;
		
		// patch to here
		patchAddress(returnAddress, generationAddress);
		
		return null;
	}

	@Override
	public Void visit(ArithExpn expn) {
		String op = expn.getOpSymbol();
		CODEGEN(expn.getLeft());
		CODEGEN(expn.getRight());

		if (op.equals("+")) {
			ADD();
		} else if (op.equals("-")) {
			SUB();
		} else if (op.equals("*")) {
			MUL();
		} else if (op.equals("/")) {
			DIV();
		}

		return null;
	}

	@Override
	public Void visit(BinaryExpn expn) {
		// Handled in subcases.
		return null;
	}

	@Override
	public Void visit(BoolConstExpn expn) {
		short machineVal = expn.getValue() ? 
				Machine.MACHINE_TRUE
				: Machine.MACHINE_FALSE;
		PUSH(machineVal);

		return null;
	}

	@Override
	public Void visit(BoolExpn expn) {
		String op = expn.getOpSymbol();
		short addr;
		if (op.equals("&")) {
			CODEGEN(expn.getLeft());
			DUP();
			addr = generationAddress;
			PUSH(++addr);
			BF();
			
			// Demorgans
			PUSH(Machine.MACHINE_FALSE);
			EQ();
			CODEGEN(expn.getRight());
			PUSH(Machine.MACHINE_FALSE);
			EQ();
			OR();
			PUSH(Machine.MACHINE_FALSE);
			EQ();
			patchAddress(addr, generationAddress); 
		} else if (op.equals("|")) {
			CODEGEN(expn.getLeft());
			DUP();
			PUSH(Machine.MACHINE_FALSE);
			EQ();
			addr = generationAddress;
			PUSH(++addr);
			BF();
			CODEGEN(expn.getRight());
			OR();
			patchAddress(addr, generationAddress); 
		}

		return null;
	}

	@Override
	public Void visit(CompareExpn expn) {
		String op = expn.getOpSymbol();

		if (op.equals("<")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			LT();
		} else if (op.equals(">")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			LT();
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			EQ();
			OR();
			PUSH(Machine.MACHINE_FALSE);
			EQ();
		} else if (op.equals("<=")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			LT();
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			EQ();
			OR();
		} else if (op.equals(">=")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			LT();
			PUSH(Machine.MACHINE_FALSE);
			EQ();
		}

		return null;
	}

	@Override
	public Void visit(ConstExpn expn) {
		return null;
	}

	@Override
	public Void visit(EqualsExpn expn) {
		String op = expn.getOpSymbol();

		if (op.equals("=")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			EQ();
		} else if (op.equals("!=")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			EQ();
			PUSH(Machine.MACHINE_FALSE);
			EQ();
		}

		return null;
	}

	@Override
	public Void visit(FunctionCallExpn expn) {
		// Return value
		PUSH(Machine.UNDEFINED);

		// Return address. Will patch after BR statement is written.
		PUSH(Machine.UNDEFINED);
		short returnAddressPatch = (short)(generationAddress - 1);
		
		// Dynamic link
		ADDR(currentScope.getLexicalLevel(), 0);
		// Arguments.
		for (Expn argument : expn.getArguments()) {
			CODEGEN(argument);
		}

		// Need the routine symbol to mark the reference for patching.
		SymbolTableEntry routineSymbol = lookThroughAncestors(expn.getIdent());
		RoutineDecl routineDecl = (RoutineDecl)routineSymbol.getNode();

		// Branch to called function. For now, just mark it for patching.
		PUSH(Machine.UNDEFINED);
		routinePatchAddresses.get(routinePatchAddresses.size() - 1).add(
			new RoutinePatchEntry(
				routineSymbol
				, (short)(generationAddress - 1)));
		BR();

		// Update the return address.
		patchAddress(returnAddressPatch, generationAddress);
		
		return null;
	}

	@Override
	public Void visit(IdentExpn expn) {
		// TODO: check if name of function or procedure
		// if symbol == procedure, make fake ProcedureCallStmt, visit
		// if symbol == function, make fake FunctionCallExpn, visit
		// else do this
		SymbolTableEntry entry = lookThroughAncestors(expn.getIdent());
		if (entry == null){
			// look up parent
		}
		
		// check if the identifier is a procedure
		if(entry.getKind() == SymbolKind.FUNCTION) {
			ASTList<Expn> argument = new ASTList<Expn>();
			FunctionCallExpn fc = new FunctionCallExpn(expn.getIdent(), argument);
			visit(fc);
		} else {
			CODEGEN_ADDR(expn);
			LOAD();
		}
		return null;
	}

	@Override
	public Void visit(IntConstExpn expn) {
		// C80
		PUSH(expn.getValue());

		return null;
	}

	@Override
	public Void visit(NotExpn expn) {
		
		CODEGEN(expn.getOperand());
		PUSH(Machine.MACHINE_FALSE);
		EQ();
		
		return null;
	}

	@Override
	public Void visit(SkipConstExpn expn) {
		// C53
		PUSH('\n');
		PRINTC();
		return null;
	}

	@Override
	public Void visit(SubsExpn expn) {
		
		CODEGEN_ADDR(expn);
		LOAD();
		
		return null;
	}

	private void makePrintStringProcedure() {
		// make the address of the function independent of where I call this
		// function
		BEGIN_PRINT_STR = generationAddress;

		// dup the character
		short loopAddr = generationAddress;
		DUP();

		// check if the character is 0
		PUSH(0);
		EQ();

		// negate this
		PUSH(Machine.MACHINE_FALSE);
		EQ();

		// branch to the end of this loop
		short curAddr = generationAddress;
		PUSH(curAddr + 7);
		BF();

		// Print the character
		PRINTC();

		// loop back to DUP;
		PUSH(loopAddr);
		BR();

		// remove the 0
		POP();

		// branch back to the address given
		BR();

		END_PRINT_STR = generationAddress;
	}

	@Override
	public Void visit(TextConstExpn expn) {
		// C52

		// push the return instruction address
		short addressToPatch = generationAddress;
		PUSH(++addressToPatch);

		// push all the characters
		char[] bytes = new StringBuilder(expn.getValue()).reverse().toString()
				.toCharArray();
		PUSH(0);
		for (char c : bytes) {
			PUSH(c);
		}

		// branch to the address of the PRINT_STR procedure
		PUSH(BEGIN_PRINT_STR);
		BR();

		// patch the incorrect return address created up above
		patchAddress(addressToPatch, generationAddress);

		return null;
	}

	@Override
	public Void visit(UnaryExpn expn) {
		return null;
	}

	@Override
	public Void visit(UnaryMinusExpn expn) {
		
		CODEGEN(expn.getOperand());
		NEG();
		
		return null;
	}

	/**
	 * Performs code generation for assignment of variables(Scalar or 1-D/2-D
	 * arrays).
	 * @param stmt				The assignment statement
	 */
	@Override
	public Void visit(AssignStmt stmt) {
		CODEGEN_ADDR(stmt.getLval());
		CODEGEN(stmt.getRval());
		STORE();
		
		return null;
	}
	
	
	/**
	 * Performs code generation for exit/exit when statement by setting the address of 
	 * exit to the array patch_addr and patch the address when the loop end.
	 * @param stmt				The exit statement
	 */
	@Override
	public Void visit(ExitStmt stmt) {
		short exit_addr = generationAddress;
		//exit when
		if (stmt.getExpn() != null){
			CODEGEN(stmt.getExpn());
			PUSH(Machine.MACHINE_FALSE);
			EQ();
			exit_addr = generationAddress;
			PUSH(exit_addr);
			BF();
		}else{
			//exit
			PUSH(exit_addr);
			BR();
		}
		//The exit address that needs to be patch
		loopPatchAddresses.get(loopPatchAddresses.size() - 1).add((int) exit_addr);
		
		return null;
	}

	@Override
	public Void visit(GetStmt stmt) {
		for (Expn expn : stmt.getInputs()) {
			CODEGEN_ADDR(expn);
			READI();
			STORE();
		}
		return null;
	}

	/**
	 * Handles the if statement with or without else. By patching the correct
	 * address to branching after code generation.
	 * @param stmt				If statement
	 */
	@Override
	public Void visit(IfStmt stmt) {
		// If statement without else.
		CODEGEN(stmt.getCondition());
		// If there is no else statement in the if statement,
		// then the else_addr is the end of the if statement.
		short else_addr = generationAddress;
		PUSH(else_addr);
		BF();
		CODEGEN(stmt.getWhenTrue());

		// If statement with else
		if (stmt.getWhenFalse() != null) {
			short end_of_ifstmt_addr = generationAddress;
			PUSH(end_of_ifstmt_addr);
			BR();
			patchAddress(++else_addr, generationAddress);
			CODEGEN(stmt.getWhenFalse());
			patchAddress(++end_of_ifstmt_addr, generationAddress);
		} else {
			patchAddress(++else_addr, generationAddress);
		}
		return null;
	}

	@Override
	public Void visit(LoopingStmt stmt) {
		return null;
	}

	/**
	 * Code generates loop by first code generate the body of the loop. Then,
	 * branch back to the top of the loop afterward. It also performs patching 
	 * for exit when/exit statements when code generation of loops is finished. 
	 * @param stmt				The loop statement
	 */
	@Override
	public Void visit(LoopStmt stmt) {
		short start_loop = generationAddress;
		
		loopPatchAddresses.add(new ArrayList<Integer>());
		
		CODEGEN(stmt.getBody());
		PUSH(start_loop);
		BR();
		//Patching the address for exit/exit when
		for (int addr: loopPatchAddresses.get(loopPatchAddresses.size() - 1)){
			addr ++;
			patchAddress((short)addr, generationAddress);
		}
		//Clean up the array list
		loopPatchAddresses.remove(loopPatchAddresses.size() - 1);
		return null;
	}

	@Override
	public Void visit(ProcedureCallStmt stmt) {
		// Return address
		PUSH(Machine.UNDEFINED);
		short returnAddressPatch = (short)(generationAddress - 1);
		
		// Dynamic link
		ADDR(currentScope.getLexicalLevel(), 0);
		
		// Arguments.
		for (Expn argument : stmt.getArguments()) {
			CODEGEN(argument);
		}

		// Need the routine symbol to mark the reference for patching.
		SymbolTableEntry routineSymbol = lookThroughAncestors(stmt.getName());
		RoutineDecl routineDecl = (RoutineDecl)routineSymbol.getNode();

		// Branch to called function. For now, just mark it for patching.
		PUSH(Machine.UNDEFINED);
		routinePatchAddresses.get(routinePatchAddresses.size() - 1).add(
			new RoutinePatchEntry(
				routineSymbol
				, (short)(generationAddress - 1)));
		BR();

		// Update the return address.
		patchAddress(returnAddressPatch, generationAddress);
		return null;
	}

	@Override
	public Void visit(Program stmt) {
		// generate PRINT_STR instructions
		makePrintStringProcedure();

		// New (pseudo-)routine, new routinePatch list.
		routinePatchAddresses.add(new ArrayList<RoutinePatchEntry>());
		pendingRoutines.add(new ArrayList<RoutineDecl>());

//		TRON();
		// C00
		PUSHMT();
		SETD(0);
		
		currentScope = stmt.getScope();

		// C30, C31, C32, C37
		PUSH(Machine.UNDEFINED);
		PUSH(currentScope.getVariableSize() + currentScope.getParameterSize());
		DUPN();

		CODEGEN(stmt.getBody());

		// C01
		HALT();

		// Generate code for any local functions.
		visitPendingRoutines();
//		TROFF();

		// C02
		// finalize the machine state.
		try {
			this.Finalize();
		} catch (MemoryAddressException e) {
			outputMemoryAddressError(e);
		} catch (ExecutionException e) {
			outputExecutionError(e);
		}

		return null;
	}

	/**
	 * Generates code for all of the pending routines of the function most
	 * recently having finished codegen.
	 */
	private void visitPendingRoutines() {
		// Process all local routines.
		List<RoutineDecl> pendingLocalRoutines =
			pendingRoutines.get(pendingRoutines.size() - 1);
		for (RoutineDecl routineDecl : pendingLocalRoutines) {
			// Will recursively generate code for all local functions as well.
			visitPendingRoutine(routineDecl);
		}
		// Local routines are done processing, remove from list.
		pendingRoutines.remove(pendingRoutines.size() - 1);

		// Patch any dangling references.
		for (RoutinePatchEntry routinePatchEntry
				: routinePatchAddresses.get(routinePatchAddresses.size() - 1)) {
			patchAddress(
				routinePatchEntry.memoryAddress
				, (short)routinePatchEntry.routineSymbol.getOffset());
		}
		// Done patching references.
		routinePatchAddresses.remove(routinePatchAddresses.size() - 1);
	}

	/**
	 * Generates instruction code for the given routine.
	 */
	private void visitPendingRoutine(RoutineDecl routineDecl) {
		// New routine, new routinePatch list.
		routinePatchAddresses.add(new ArrayList<RoutinePatchEntry>());
		pendingRoutines.add(new ArrayList<RoutineDecl>());

		// Mark the start of the routine.
		SymbolTableEntry routineSymbol =
				lookThroughAncestors(routineDecl.getName());
		routineSymbol.setOffset(generationAddress);

		Scope routineBody = routineDecl.getBody();
		// Update scope
		MajorScope previousScope = currentScope;
		currentScope = routineBody.getScope();
		
		// Allocate local variable space.
		if(currentScope.getVariableSize() > 0){
			PUSH(Machine.UNDEFINED);
			PUSH(currentScope.getVariableSize());
			DUPN();
		}

		// Save the display register.
		ADDR(currentScope.getLexicalLevel(), 0);
		// Update the display register.
		PUSHMT();
		//     NOTE: sizeof(vars)
		//           + 1 (displaySave)
		//           + 1 (dynamicLink)
		//           + 1 (returnAddress)
		PUSH(currentScope.getVariableSize() + currentScope.getParameterSize() + 3);
		SUB();
		SETD(currentScope.getLexicalLevel());

		// Stack is now ready for function code.		
		// Generate the code for the routine body.
		CODEGEN(routineBody);
		
		short cleanupSectionAddress = generationAddress;

		// Handle the return value.
		if (routineDecl.getType() != null)
		{
			// Routine is a function.
			// The return value of the function is on the top of the stack.
			// Store it in the return value section of the stack frame.
			ADDR(currentScope.getLexicalLevel(), -1);
			SWAP();
			STORE();
		}

		// Restore the display register.
		SETD(currentScope.getLexicalLevel());

		// Pop the parameters and variables.
		PUSH(currentScope.getVariableSize() + currentScope.getParameterSize());
		POPN();

		// Pop the dynamic link.
		POP();

		// Branch to the return address, leaving the return value on the top of
		// the stack.
		BR();
		
		// patch the return addresses in this routine (hopefully)
		for(Short address : returnPatchAddresses) {
			patchAddress(address, cleanupSectionAddress);
		}
		returnPatchAddresses.clear();

		// Recurse to define any local routines. Recursion stops when no local
		// routines are declared.
		visitPendingRoutines();
		
		// Reset scope.
		currentScope = previousScope;
	}

	@Override
	public Void visit(PutStmt stmt) {
		for (Printable p : stmt.getOutputs()) {
			CODEGEN(p);

			// SkipConstExpn and TextConstExpn already print directly
			if (p instanceof SkipConstExpn || p instanceof TextConstExpn) {
				continue;
			}

			// any other integer expression will leave its value on the stack

			// C51
			PRINTI();
		}
		return null;
	}

	/**
	 * Code generates the return statements for functions and procedures. This
	 * will be two cases. 1.For procedures: Just patch the return address, then
	 * branch. 2.For functions: Code generate the expression first, then patch
	 * the address before branch.
	 * @param stmt				Return statement
	 */
	@Override
	public Void visit(ReturnStmt stmt) {
		// This is a function
		if (stmt.getValue() != null) {
			CODEGEN(stmt.getValue());
		}
		PUSH(Machine.UNDEFINED);
		short addr = (short)(generationAddress - 1);
		
		returnPatchAddresses.add(addr);
		
		BR();
		return null;
	}

	@Override
	public Void visit(Scope stmt) {
		// C03
		MajorScope old = currentScope;
		currentScope = stmt.getScope();
		
		// generate code for the body
		CODEGEN(stmt.getBody());

		// C04
		currentScope = old;

		return null;
	}

	@Override
	public Void visit(Stmt stmt) {
		// Handled by subclasses.
		return null;
	}
	/**
	 * Performs code generation for while loops by code generating the expression
	 * first. If the expression is false, then immediately branch out of the loop.
	 * If it's true, then after code generating the body of the while loop branch
	 * back to the start of the loop
	 * @param stmt				While loop statement
	 */
	@Override
	public Void visit(WhileDoStmt stmt) {

		loopPatchAddresses.add(new ArrayList<Integer>());
		
		short start_while = generationAddress;
		CODEGEN(stmt.getExpn());
		short end_of_while_addr = generationAddress;
		PUSH(end_of_while_addr);
		BF();
		CODEGEN(stmt.getBody());
		PUSH(start_while);
		BR();
		
		//Patching the address for exit/exit when
		for (int addr: loopPatchAddresses.get(loopPatchAddresses.size() - 1)){
			addr ++;
			patchAddress((short)addr, generationAddress);
		}
		//Clean up the array list
		loopPatchAddresses.remove(loopPatchAddresses.size() - 1);
		
		patchAddress(++end_of_while_addr, generationAddress);
		return null;
	}

	@Override
	public Void visit(BooleanType type) {
		// Handled by semantics.
		return null;
	}

	@Override
	public Void visit(IntegerType type) {
		// Handled by semantics.
		return null;
	}

}
