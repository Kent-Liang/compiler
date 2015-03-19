package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;


/**
 * Place holder for all binary expression where both operands could be either
 * integer or boolean expressions. e.g. = and != comparisons
 */
public class EqualsExpn extends BinaryExpn {
    public final static String OP_EQUAL 	= "=";
    public final static String OP_NOT_EQUAL	= "!=";

    public EqualsExpn(String opSymbol, Expn left, Expn right) {
        super(opSymbol, left, right);

        assert ((opSymbol == OP_EQUAL) ||
                (opSymbol == OP_NOT_EQUAL));
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
