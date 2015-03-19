package compiler488.ast.expn;

import compiler488.ast.ASTVisitor;
import compiler488.ast.Printable;

/**
 * Represents the special literal constant associated with writing a new-line
 * character on the output device.
 */
public class SkipConstExpn extends ConstExpn implements Printable {
    public SkipConstExpn() {
        super();
    }

    @Override
    public String toString() {
        return "skip";
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
