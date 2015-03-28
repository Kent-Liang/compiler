package compiler488.symbol;

import compiler488.ast.AST;
import compiler488.ast.decl.ArrayDeclPart;
import compiler488.ast.decl.ScalarDecl;
import compiler488.ast.type.Type;

public class SymbolTableEntry {
	
	public static enum SymbolKind {
		SCALAR,
		ARRAY,
		FUNCTION,
		PROCEDURE
	}
	
	private String varname;
	private Type type;
	private SymbolKind kind; // can be either array, variable, function, procedure
	private AST node;
	private short offset;
	private int lexicalLevel;

	public SymbolTableEntry(String varname, Type type, SymbolKind identifierType,
			AST node, short offset) {
		this.varname = varname;
		this.type = type;
		this.kind = identifierType;
		this.node = node;
		this.offset = offset;
	}

	public String getVarname() {
		return varname;
	}

	public Type getType() {
		return type;
	}

	public SymbolKind getKind() {
		return kind;
	}

	public AST getNode() {
		return node;
	}
	
	public short getOffset() {
		return offset;
	}


	public void setOffset(short offset) {
		this.offset = offset;
	}

	@Override
	public String toString() {
		return String.format("%s %s %s, offset: %d, LL: %d", 
				this.kind, this.type, this.varname, this.offset, this.lexicalLevel);
	}

	public int getSize() {
		if(this.kind == SymbolKind.PROCEDURE 
				|| this.kind == SymbolKind.FUNCTION) {
			return 0;
		}
		
		// calculate the size for other variable offset calculations
		if(this.node instanceof ArrayDeclPart) {
			return ((ArrayDeclPart) this.node).getSize();
		} else if(this.node instanceof ScalarDecl) {
			return 1; // size of scalars are constant
		}
		
		return 0;
	}

	public void setLexicalLevel(int lexicalLevel) {
		this.lexicalLevel = lexicalLevel;
	}
	
	public int getLexicalLevel() {
		return this.lexicalLevel;
	}
}
