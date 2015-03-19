package compiler488.ast.stmt;

import compiler488.ast.ASTVisitor;
import compiler488.ast.ASTList;
import compiler488.ast.PrettyPrinter;
import compiler488.symbol.MajorScope;

/**
 * Represents the declarations and instructions of a scope construct.
 */
public class Scope extends Stmt {
    /** Body of the scope, mixed list of declarations and statements. */
    protected ASTList<Stmt> body;
    private MajorScope scope = null;

    public Scope() {
        super();

        this.body = null;
    }

    public Scope(ASTList<Stmt> body) {
        this();

        this.body = body;
    }

    public ASTList<Stmt> getBody() {
        return body;
    }

    @Override
    public void prettyPrint(PrettyPrinter p) {
        p.println("begin");
        if (body != null && body.size() > 0) {
            body.prettyPrintBlock(p);
        }
        p.print("end");
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visit(this);
    }

	public MajorScope getScope() {
		return scope;
	}

	public void setScope(MajorScope scope) {
		this.scope = scope;
	}
}
