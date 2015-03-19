package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;


/**
 * Represents a literal integer constant.
 */
public class IntConstExpn extends ConstExpn {
    /**
     * The value of this literal.
     */
    private Integer value;

    public IntConstExpn(Integer value) {
        super();
        
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }

    @Override
    public String toString () {
        return value.toString();
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
    
}
