package compiler488.ast.expn;

import compiler488.ast.BaseAST;
import compiler488.ast.Printable;
import compiler488.ast.type.Type;

/**
 * A placeholder for all expressions.
 */
public abstract class Expn extends BaseAST implements Printable {
    private Type type;

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getTypeString() {
        if(type == null) {
          return null;
        }
        return type.toString();
    }

    public boolean isType(String typeString) {
        if(type == null) {
          return false;
        }
        return type.toString().equals(typeString);
    }

    public boolean isType(Type type) {
        if(type == null) {
          return false;
        }
        return isType(type.toString());
    }

    public boolean isBoolean() {
        return isType(Type.BOOL_TYPE);
    }

    public boolean isInteger() {
        return isType(Type.INT_TYPE);
    }
}
