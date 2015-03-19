package compiler488.ast;

import compiler488.ast.decl.ArrayDeclPart;
import compiler488.ast.decl.Declaration;
import compiler488.ast.decl.MultiDeclarations;
import compiler488.ast.decl.RoutineDecl;
import compiler488.ast.decl.ScalarDecl;
import compiler488.ast.expn.AnonFuncExpn;
import compiler488.ast.expn.ArithExpn;
import compiler488.ast.expn.BinaryExpn;
import compiler488.ast.expn.BoolConstExpn;
import compiler488.ast.expn.BoolExpn;
import compiler488.ast.expn.CompareExpn;
import compiler488.ast.expn.ConstExpn;
import compiler488.ast.expn.EqualsExpn;
import compiler488.ast.expn.FunctionCallExpn;
import compiler488.ast.expn.IdentExpn;
import compiler488.ast.expn.IntConstExpn;
import compiler488.ast.expn.NotExpn;
import compiler488.ast.expn.SkipConstExpn;
import compiler488.ast.expn.SubsExpn;
import compiler488.ast.expn.TextConstExpn;
import compiler488.ast.expn.UnaryExpn;
import compiler488.ast.expn.UnaryMinusExpn;
import compiler488.ast.stmt.AssignStmt;
import compiler488.ast.stmt.ExitStmt;
import compiler488.ast.stmt.GetStmt;
import compiler488.ast.stmt.IfStmt;
import compiler488.ast.stmt.LoopStmt;
import compiler488.ast.stmt.LoopingStmt;
import compiler488.ast.stmt.ProcedureCallStmt;
import compiler488.ast.stmt.Program;
import compiler488.ast.stmt.PutStmt;
import compiler488.ast.stmt.ReturnStmt;
import compiler488.ast.stmt.Scope;
import compiler488.ast.stmt.Stmt;
import compiler488.ast.stmt.WhileDoStmt;
import compiler488.ast.type.BooleanType;
import compiler488.ast.type.IntegerType;

public interface ASTVisitor<T> {

    public T visit(AST node);
    public <A extends AST> T visit(ASTList<A> list);

    public T visit(ArrayDeclPart decl);
    public T visit(Declaration decl);
    public T visit(MultiDeclarations decl);
    public T visit(RoutineDecl decl);
    public T visit(ScalarDecl decl);

    public T visit(AnonFuncExpn expn);
    public T visit(ArithExpn expn);
    public T visit(BinaryExpn expn);
    public T visit(BoolConstExpn expn);
    public T visit(BoolExpn expn);
    public T visit(CompareExpn expn);
    public T visit(ConstExpn expn);
    public T visit(EqualsExpn expn);
    public T visit(FunctionCallExpn expn);
    public T visit(IdentExpn expn);
    public T visit(IntConstExpn expn);
    public T visit(NotExpn expn);

    public T visit(SkipConstExpn expn);
    public T visit(SubsExpn expn);
    public T visit(TextConstExpn expn);
    public T visit(UnaryExpn expn);
    public T visit(UnaryMinusExpn expn);

    public T visit(AssignStmt stmt);
    public T visit(ExitStmt stmt);
    public T visit(GetStmt stmt);
    public T visit(IfStmt stmt);
    public T visit(LoopingStmt stmt);
    public T visit(LoopStmt stmt);
    public T visit(ProcedureCallStmt stmt);
    public T visit(Program stmt);
    public T visit(PutStmt stmt);
    public T visit(ReturnStmt stmt);
    public T visit(Scope stmt);
    public T visit(Stmt stmt);
    public T visit(WhileDoStmt stmt);

    public T visit(BooleanType type);
    public T visit(IntegerType type);
}
