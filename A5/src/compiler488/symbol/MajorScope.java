package compiler488.symbol;

import java.util.ArrayList;
import java.util.List;

import compiler488.ast.AST;
import compiler488.ast.decl.RoutineDecl;
import compiler488.ast.type.Type;
import compiler488.symbol.SymbolTableEntry.DeclarationContext;
import compiler488.symbol.SymbolTableEntry.SymbolKind;

/** Symbol Table
 *  This almost empty class is a framework for implementing
 *  a Symbol Table class for the CSC488S compiler
 *
 *  Each implementation can change/modify/delete this class
 *  as they see fit.
 *
 *  @author  <B> 
                Benson Quach (g0quachb)
                Eric Chen (g3chencm)
                Eric Snyder (g4snyder)
                Francesco Gramano (g2graman)
                Nicholas Dujay (c4dujayn)
                Winston Yeung (g2yeungw) 
            </B>
 */

public class MajorScope {

	public static enum ScopeKind {
		PROCEDURE(2), //return address, dynamic link, variables/params, display register
		FUNCTION(2), //return address, dynamic link, variables/params, display register
		PROGRAM(0),
		NORMAL(0);
		
		private int offset;
		ScopeKind(int offset) {
			this.offset = offset;
		}
		
		public int getOffset() {
			return offset;
		}
	}

	private SymbolTable symbolTable;
	private ScopeKind kind;
	private RoutineDecl routine;
	
	private int variableSize;
	private int parameterSize;
	private int lexicalLevel;
	
	// tree structure to keep the symbol table persistent
	private MajorScope parent;
	private List<MajorScope> children;

    public MajorScope(MajorScope parent, ScopeKind kind) {
    	this(parent, kind, null);
    }

    public MajorScope(MajorScope parent, ScopeKind kind, RoutineDecl routine) {
    	this.setParent(parent);
        this.symbolTable = new SymbolTable();
        this.kind = kind;
        this.routine = routine;
        this.children = new ArrayList<MajorScope>(0);
        
        this.parameterSize = 0;
        this.variableSize = 0;
    }

	public SymbolTableEntry lookup(String varname) {
		return symbolTable.lookup(varname);
	}

	public void addEntry(String varname, Type type, SymbolKind kind, AST node,
			DeclarationContext context) {
		int offset = this.kind.getOffset() + parameterSize + variableSize;
		
		SymbolTableEntry e = symbolTable.addEntry(varname, type, kind, node, offset, context);
		e.setLexicalLevel(this.lexicalLevel);
		
		// parameter size already calculated
//		System.out.println(varname + " " + offset);
		if(context == DeclarationContext.VARIABLE) {
			variableSize += e.getSize();
		} else if(context == DeclarationContext.PARAMETER) {
			parameterSize += e.getSize();
		}
	}

	public ScopeKind getKind() {
		return kind;
	}

	public void setKind(ScopeKind kind) {
		this.kind = kind;
	}

	public RoutineDecl getRoutine() {
		return routine;
	}

	public void setRoutine(RoutineDecl routine) {
		this.routine = routine;
	}
	
	public MajorScope getMajorAncestor() {
		MajorScope parent = this.parent;
		while(parent != null 
				&& parent.kind == ScopeKind.NORMAL) {
			parent = parent.parent;
		}
		return parent;
	}

	public MajorScope getParent() {
		return parent;
	}

	public void setParent(MajorScope parent) {
		this.parent = parent;
	}

	public boolean addChild(MajorScope e) {
		e.setLexicalLevel(this.lexicalLevel + 1);
		return children.add(e);
	}

	public MajorScope getChild(int index) {
		return children.get(index);
	}

	public int getLexicalLevel() {
		return lexicalLevel;
	}

	public void setLexicalLevel(int lexicalLevel) {
		this.lexicalLevel = lexicalLevel;
	}
	
	public int getVariableSize() {
		return this.variableSize;
	}
	
	public int getParameterSize() {
		return this.parameterSize;
	}

	@Override
	public String toString() {
		return String.format("%s: %s\n==============\n%s",
				kind,
				routine != null ? routine.getName() : "",
				symbolTable);
	}

	public void merge(MajorScope scope) {
		for(SymbolTableEntry e : scope.symbolTable.getEntries()) {
			int offset = kind.getOffset() + parameterSize + variableSize;
			
			e.setLexicalLevel(this.lexicalLevel);
			e.setOffset(offset);
			symbolTable.addEntry(e);

			if(e.getContext() == DeclarationContext.VARIABLE) {
				variableSize += e.getSize();
			} else {
				parameterSize += e.getSize();
			}
		}
	}
}
