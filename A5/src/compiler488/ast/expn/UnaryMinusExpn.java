package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;

/**
 * Represents negation of an integer expression
 */
public class UnaryMinusExpn extends UnaryExpn {
    public UnaryMinusExpn(Expn operand) {
        super(UnaryExpn.OP_MINUS, operand);
    }

    public Boolean parentAccept(ASTVisitor<Boolean> visitor) {
    	return super.accept(visitor);
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
