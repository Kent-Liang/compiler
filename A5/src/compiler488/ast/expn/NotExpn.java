package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;


/**
 * Represents the boolean negation of an expression.
 */
public class NotExpn extends UnaryExpn {
    public NotExpn(Expn operand) {
        super(UnaryExpn.OP_NOT, operand);
    }

    public Boolean parentAccept(ASTVisitor<Boolean> visitor) {
    	return super.accept(visitor);
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
    
}
