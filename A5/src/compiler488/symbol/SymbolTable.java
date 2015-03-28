package compiler488.symbol;

import java.util.Collection;
import java.util.Map;
import java.util.HashMap;

import compiler488.ast.AST;
import compiler488.ast.type.Type;
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

public class SymbolTable {

	/** Symbol Table  constructor
         *  Create and initialize a symbol table
	 */
    private Map<String, SymbolTableEntry> entries;
    public SymbolTable  (){
        this.entries = new HashMap<String, SymbolTableEntry>();
    }


	/** The rest of Symbol Table
	 *  Data structures, public and private functions
 	 *  to implement the Symbol Table
	 *  GO HERE.
	 */

    /**
     * @param varname The name of the variable to lookup
     * @return The symbol table entry that corresponds to the specific variable name. Returns null
     *         if the variable does not exist in this scope.
     */
    public SymbolTableEntry lookup(String varname) {
        return entries.get(varname);
    }

    /**
     * Add the symbol table entry to the symbol table.
     * @param varname The name of the variable to insert into the table
     * @param type The type(boolean or string or integer) of the variable
     * @param kind The kind(procedure, function, array or scalar) of the variable
     * @param node The AST node
     * @param offset 
     */
    public SymbolTableEntry addEntry(String varname, Type type, SymbolKind kind, AST node, int offset) {
        SymbolTableEntry st_entry = new SymbolTableEntry(varname, type, kind, node, (short)offset);
        entries.put(varname, st_entry);
        return st_entry;
    }

    @Override
    public String toString() {
    	StringBuilder bldr = new StringBuilder();
    	for(String var : entries.keySet()) {
    		bldr.append(entries.get(var).toString());
    		bldr.append('\n');
    	}
    	return bldr.toString();
    }
    
    public Collection<SymbolTableEntry> getEntries() {
    	return entries.values();
    }


	public void addEntry(SymbolTableEntry e) {
		this.entries.put(e.getVarname(), e);
	}
}
