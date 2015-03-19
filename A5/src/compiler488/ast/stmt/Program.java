package compiler488.ast.stmt;

import compiler488.ast.ASTList;
import compiler488.ast.ASTVisitor;

/**
 * Placeholder for the scope that is the entire program
 */
public class Program extends Scope {
    public Program(ASTList<Stmt> body) {
        super(body);
    }

    public Program(Scope scope) {
        super(scope.getBody());
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
