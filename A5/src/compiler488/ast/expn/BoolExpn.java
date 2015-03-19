package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;


/**
 * Place holder for all binary expression where both operands must be boolean
 * expressions.
 */
public class BoolExpn extends BinaryExpn {
    public final static String OP_OR 	= "|";
    public final static String OP_AND	= "&";

    public BoolExpn(String opSymbol, Expn left, Expn right) {
        super(opSymbol, left, right);

        assert ((opSymbol == OP_OR) ||
                (opSymbol == OP_AND));
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
    
}
