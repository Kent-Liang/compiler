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
import compiler488.runtime.ExecutionException ;
import compiler488.symbol.MajorScope;
import compiler488.symbol.SymbolTableEntry;

/**      CodeGenerator.java
 *<pre>
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
 *  @author  <B>
                Benson Quach (g0quachb)
                Eric Chen (g3chencm)
                Eric Snyder (g4snyder)
                Francesco Gramano (g2graman)
                Nicholas Dujay (c4dujayn)
                Winston Yeung (g2yeungw)
            </B>
 */

public class CodeGen implements ASTVisitor<Void>
    {

    /** initial value for memory stack pointer */
    private short startMSP;
    /** initial value for program counter */
    private short startPC;
    /** initial value for memory limit pointer */
    private short startMLP;

    /** flag for tracing code generation */
    private boolean traceCodeGen = Main.traceCodeGen ;


    private short currentAddress;
    
    private MajorScope currentScope;
    
    // addresses for the PRINT_STR function
    private short BEGIN_PRINT_STR;
	private short END_PRINT_STR;

    /**
     *  Constructor to initialize code generation
     */
    public CodeGen()
	{
	// YOUR CONSTRUCTOR GOES HERE.
    	currentAddress = 0;
	}

    // Utility procedures used for code generation GO HERE.

    /**
     *  Additional intialization for gode generation.
     *  Called once at the start of code generation.
     *  May be unnecesary if constructor does everything.
     */

   /** Additional initialization for Code Generation (if required) */
   void Initialize()
	{
	/********************************************************/
	/* Initialization code for the code generator GOES HERE */
	/* This procedure is called once before codeGeneration  */
	/*                                                      */
	/********************************************************/

	return;
	}


    /**
     *  Perform any requred cleanup at the end of code generation.
     *  Called once at the end of code generation.
     *  @throws MemoryAddressException  from Machine.writeMemory
     *  @throws ExecutionException      from Machine.writeMemory
     */
    void Finalize()
        throws MemoryAddressException, ExecutionException     // from Machine.writeMemory
	{
	/********************************************************/
	/* Finalization code for the code generator GOES HERE.  */
	/*                                                      */
	/* This procedure is called once at the end of code     */
	/* generation                                           */
	/********************************************************/

	 	Machine.setPC( (short) END_PRINT_STR ) ;		/* where code to be executed begins */
		Machine.setMSP(currentAddress );   	/* where memory stack begins */
		Machine.setMLP((short) ( Machine.memorySize -1 ));

	return;
	}

    /** Procedure to implement code generation based on code generation
     *  action number
     * @param actionNumber  code generation action to perform
     */
    void generateCode( int actionNumber )
	{
	if( traceCodeGen )
	    {
		//output the standard trace stream
		Main.traceStream.println("CodeGen: C" +  actionNumber );
	    }

	/****************************************************************/
	/*  Code to implement the code generation actions GOES HERE     */
	/*  This dummy code generator just prints the actionNumber      */
	/*  In Assignment 5, you'll implement something more interesting */
        /*                                                               */
        /*  FEEL FREE TO ignore or replace this procedure                */
	/****************************************************************/

        System.out.println("Codegen: C" + actionNumber );
	return;
	}


    //  ADDITIONAL FUNCTIONS TO IMPLEMENT CODE GENERATION GO HERE

    // helper functions for each machine instruction

    private void outputExecutionError(ExecutionException e) {
    	String msg = String.format("EXECUTION ERROR: %s", e);
    }

    private void outputMemoryAddressError(MemoryAddressException e) {
    	String msg = String.format("MEMORY ERROR: %s", e);
    }

    private void assembly(short opcode, short... args) {
    	try {
			Machine.writeMemory(currentAddress++, opcode);
			for(short s : args) {
				Machine.writeMemory(currentAddress++, s);
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
    
    private void CODEGEN_ADDR(IdentExpn e) {
    	// TODO fill in body for CODEGEN_ADDR
   
    	//return null;
    }

    private void CODEGEN_ADDR(SubsExpn e) {
    	// TODO fill in body for CODEGEN_ADDR - SymbolTable current scope offset
    	
    	//return null;
    }
    
    @Override
	public Void visit(AST node) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <A extends AST> Void visit(ASTList<A> list) {
		for(A a : list) {
			CODEGEN(a);
		}
		
		return null;
	}

	@Override
	public Void visit(ArrayDeclPart decl) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(Declaration decl) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(MultiDeclarations decl) {
		return null;
	}

	@Override
	public Void visit(RoutineDecl decl) {
		// C34
		SymbolTableEntry e = currentScope.lookup(decl.getName());
		e.setOffset(currentAddress);
		
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(ScalarDecl decl) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(AnonFuncExpn expn) {
		// TODO Auto-generated method stub
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
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(BoolConstExpn expn) {
		short machineVal = expn.getValue() ? Machine.MACHINE_TRUE : Machine.MACHINE_FALSE;
		PUSH(machineVal);
		
		return null;
	}

	@Override
	public Void visit(BoolExpn expn) {
		String op = expn.getOpSymbol(); 
		
		if (op.equals("&")) {
			CODEGEN(expn.getLeft());
			PUSH(Machine.MACHINE_FALSE);
	        EQ();
			CODEGEN(expn.getRight());
			PUSH(Machine.MACHINE_FALSE);
	        EQ();
	        OR();
	        PUSH(Machine.MACHINE_FALSE);
	        EQ();
		} else if (op.equals("|")) {
			CODEGEN(expn.getLeft());
			CODEGEN(expn.getRight());
			OR();
		} 
		
		// TODO: SHORT-CIRCUITING
		
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
		// TODO Auto-generated method stub
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
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IdentExpn expn) {
		// TODO Auto-generated method stub
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
		// TODO Auto-generated method stub
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
		// TODO Auto-generated method stub
		return null;
	}
	
	private void makePrintStringProcedure() {
		// make the address of the function independent of where I call this function
		BEGIN_PRINT_STR = currentAddress;
		
		// dup the character
		short loopAddr = currentAddress;
		DUP();

		// check if the character is 0
		PUSH(0);
		EQ();
		
		// negate this
		PUSH(Machine.MACHINE_FALSE);
		EQ();
		
		// branch to the end of this loop
		short curAddr = currentAddress;
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
		
		END_PRINT_STR = currentAddress;
	}

	@Override
	public Void visit(TextConstExpn expn) {
		//C52
		
		// push the return instruction address
		short addressToPatch = currentAddress;
		PUSH(++addressToPatch);
		
		// push all the characters
		char[] bytes = new StringBuilder(expn.getValue())
			.reverse()
			.toString()
			.toCharArray();
		PUSH(0);
		for(char c : bytes) {
			PUSH(c);
		}
		
		// branch to the address of the PRINT_STR procedure
		PUSH(BEGIN_PRINT_STR);
		BR();
		
		// patch the incorrect return address created up above
		patchAddress(addressToPatch, currentAddress);
		
		return null;
	}

	@Override
	public Void visit(UnaryExpn expn) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(UnaryMinusExpn expn) {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * Performs code generation for assignment of 
	 * variables(Scalar or 1-D/2-D arrays).
	 * @param stmt 	The assignment statement
	 */
	@Override

	public Void visit(AssignStmt stmt) {
		// TODO Auto-generated method stub
		Expn LHS = stmt.getLval();
		
		if (LHS instanceof IdentExpn){
			CODEGEN_ADDR((IdentExpn) LHS);
		}else{
			CODEGEN_ADDR((SubsExpn) LHS);
		}
		CODEGEN(stmt.getRval());
		STORE();
		return null;
	}

	@Override
	public Void visit(ExitStmt stmt) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(GetStmt stmt) {
		// TODO Auto-generated method stub
		for (Expn expn : stmt.getInputs()){
			CODEGEN_ADDR((IdentExpn) expn);
			READI();
			STORE();
		}
		return null;
	}

	@Override
	public Void visit(IfStmt stmt) {
		// TODO Auto-generated method stub
		//If statement without else. 
		CODEGEN(stmt.getCondition());
		short end_of_if_addr = currentAddress;
		PUSH(end_of_if_addr);
		BF();
		CODEGEN(stmt.getWhenTrue());
		patchAddress(++end_of_if_addr, currentAddress);
		return null;
	}

	@Override
	public Void visit(LoopingStmt stmt) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Code generates loop by first code generate the body of 
	 * the loop. Then, branch back to the top of the loop afterward.
	 * @param stmt		The loop statement
	 * @return
	 */
	@Override
	public Void visit(LoopStmt stmt) {
		// TODO Auto-generated method stub
		int address = currentAddress;
		CODEGEN(stmt.getBody());
		PUSH(address);
		BR();
		return null;
	}

	@Override
	public Void visit(ProcedureCallStmt stmt) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(Program stmt) {
		// generate PRINT_STR instructions
		makePrintStringProcedure();
		
		// C00
		PUSHMT();
		SETD(0);
		
		this.visit((Scope) stmt);
		
		// C01
		HALT();
		
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
	
	@Override
	public Void visit(PutStmt stmt) {
		for(Printable p : stmt.getOutputs()) {
			CODEGEN(p);
			
			 // SkipConstExpn and TextConstExpn already print directly
			if(p instanceof SkipConstExpn
					|| p instanceof TextConstExpn) {
				continue;
			}
			
			// any other integer expression will leave its value on the stack
			
			// C51
			PRINTI();
		}
		return null;
	}
	/**
	 * Code generates the return statements for functions and procedures.
	 * This will be two cases. 
	 * 1.For procedures:
	 * Just patch the exit address, then branch. 
	 * 2.For functions:
	 * Code generate the expression first, then patch the address before branch. 
	 * @param stmt
	 * @return
	 */
	@Override
	public Void visit(ReturnStmt stmt) {
		// TODO Auto-generated method stub
		//This is a function
		if (stmt.getValue() != null){
			CODEGEN(stmt.getValue());
		}
		short addr = 0; //edit later.
		PUSH(addr);
		BR();
		return null;
	}

	@Override
	public Void visit(Scope stmt) {
		// C03
		currentScope = stmt.getScope();
		
		// C30, C31, C32, C37
		PUSH(Machine.UNDEFINED);
		PUSH(currentScope.getTotalVariables());
		DUPN();
		
		// generate code for the body
		CODEGEN(stmt.getBody());
		
		// C04
		
		return null;
	}

	@Override
	public Void visit(Stmt stmt) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(WhileDoStmt stmt) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(BooleanType type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Void visit(IntegerType type) {
		// TODO Auto-generated method stub
		return null;
	}

}
