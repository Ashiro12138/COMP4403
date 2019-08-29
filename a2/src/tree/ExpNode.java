package tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import java_cup.runtime.ComplexSymbolFactory.Location;
import syms.Predefined;
import syms.SymEntry;
import syms.Type;

/**
 * class ExpNode - Abstract Syntax Tree representation of expressions.
 * Abstract class representing expressions.
 * Actual expression nodes extend ExpNode.
 * All expression nodes have a location and a type.
 */
public abstract class ExpNode {
    /**
     * Location in the source code of the expression
     */
    private final Location loc;
    /**
     * Type of the expression (usually determined by static checker)
     */
    Type type;

    /**
     * Constructor when type is known
     */
    ExpNode(Location loc, Type type) {
        this.loc = loc;
        this.type = type;
    }

    /**
     * Constructor when type as yet unknown
     */
    ExpNode(Location loc) {
        this(loc, Type.ERROR_TYPE);
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public Location getLocation() {
        return loc;
    }

    /**
     * Each subclass of ExpNode must provide a transform method
     * to do type checking and transform the expression node to
     * handle type coercions, etc.
     *
     * @param visitor object that implements a traversal.
     * @return transformed expression node
     */
    public abstract ExpNode transform(ExpTransform<ExpNode> visitor);


    /**
     * Each subclass of ExpNode must provide a genCode method
     * to visit the expression node to handle code generation.
     *
     * @param visitor object that implements a traversal.
     * @return generated code
     */
    public abstract Code genCode(ExpTransform<Code> visitor);

    /**
     * Tree node representing an erroneous expression.
     */
    public static class ErrorNode extends ExpNode {

        public ErrorNode(Location loc) {
            super(loc, Type.ERROR_TYPE);
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitErrorExpNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitErrorExpNode(this);
        }

        @Override
        public String toString() {
            return "ErrorNode";
        }
    }

    /**
     * Tree node representing a constant within an expression.
     */
    public static class ConstNode extends ExpNode {
        /**
         * constant's value
         */
        private final int value;

        public ConstNode(Location loc, Type type, int value) {
            super(loc, type);
            this.value = value;
        }

        public int getValue() {
            return value;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitConstNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitConstNode(this);
        }

        @Override
        public String toString() {
            return Integer.toString(value);
        }
    }

    /**
     * Identifier node is used until the identifier can be resolved
     * to be either a constant or a variable during the static
     * semantics check phase.
     */
    public static class IdentifierNode extends ExpNode {
        /**
         * Name of the identifier
         */
        private final String id;

        public IdentifierNode(Location loc, String id) {
            super(loc);
            this.id = id;
        }

        public String getId() {
            return id;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitIdentifierNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitIdentifierNode(this);
        }

        @Override
        public String toString() {
            return "IdentifierNode(" + id + ")";
        }
    }

    /**
     * Tree node representing a variable.
     */
    public static class VariableNode extends ExpNode {
        /**
         * Symbol table entry for the variable
         */
        final SymEntry.VarEntry variable;

        public VariableNode(Location loc, SymEntry.VarEntry variable) {
            super(loc, variable.getType());
            this.variable = variable;
        }

        public SymEntry.VarEntry getVariable() {
            return variable;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitVariableNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitVariableNode(this);
        }

        @Override
        public String toString() {

            return variable.getIdent();
        }
    }

    /**
     * Tree node representing a "read" expression.
     */
    public static class ReadNode extends ExpNode {

        public ReadNode(Location loc) {
            super(loc, Predefined.INTEGER_TYPE);
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitReadNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitReadNode(this);
        }

        @Override
        public String toString() {
            return "Read";
        }
    }

    /**
     * Tree node for a binary operator
     */
    public static class BinaryNode extends ExpNode {
        /**
         * Binary operator
         */
        final Operator op;
        /**
         * Arguments for operator
         */
        ExpNode left, right;

        public BinaryNode(Location loc, Operator op, ExpNode left, ExpNode right) {
            super(loc);
            this.op = op;
            this.left = left;
            this.right = right;
        }

        public Operator getOp() {
            return op;
        }

        public ExpNode getLeft() {
            return left;
        }

        public void setLeft(ExpNode left) {
            this.left = left;
        }

        public ExpNode getRight() {
            return right;
        }

        public void setRight(ExpNode right) {
            this.right = right;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitBinaryNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitBinaryNode(this);
        }

        @Override
        public String toString() {
            return op + "(" + left + "," + right + ")";
        }
    }

    /**
     * Tree node for a unary operator
     */
    public static class UnaryNode extends ExpNode {
        /**
         * Unary operator
         */
        private final Operator op;
        /**
         * Argument for operator
         */
        private ExpNode arg;

        public UnaryNode(Location loc, Operator op, ExpNode arg) {
            super(loc);
            this.op = op;
            this.arg = arg;
        }

        public Operator getOp() {
            return op;
        }

        public ExpNode getArg() {
            return arg;
        }

        public void setArg(ExpNode arg) {
            this.arg = arg;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitUnaryNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitUnaryNode(this);
        }

        @Override
        public String toString() {
            return op + "(" + arg + ")";
        }
    }

    /**
     * Tree node for dereference of an LValue.
     * A Dereference node references an ExpNode node and represents the
     * dereference of the "address" given by the leftValue to give the value
     * at that address. The type of the leftValue must be a ReferenceType.
     */
    public static class DereferenceNode extends ExpNode {
        /**
         * LValue to dereference
         */
        private ExpNode leftValue;

        /* The type of the Dereference node is the base type of the type
         * of the expression being dereferenced.
         */
        public DereferenceNode(ExpNode exp) {
            super(exp.getLocation(), exp.getType().optDereferenceType());
            assert exp.getType() instanceof Type.ReferenceType;
            this.leftValue = exp;
        }

        public ExpNode getLeftValue() {
            return leftValue;
        }

        public void setLeftValue(ExpNode leftValue) {
            this.leftValue = leftValue;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitDereferenceNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitDereferenceNode(this);
        }

        @Override
        public String toString() {
            return "Dereference(" + leftValue + ")";
        }
    }

    /**
     * Tree node representing a coercion that narrows a subrange
     */
    public static class NarrowSubrangeNode extends ExpNode {
        /**
         * Expression to be narrowed
         */
        private ExpNode exp;

        /* @requires type instance of Type.SubrangeType &&
         *           exp.getType().equals(type.getBaseType()) */
        public NarrowSubrangeNode(Location loc, Type.SubrangeType type,
                                  ExpNode exp) {
            super(loc, type);
            assert type.getBaseType() == Type.ERROR_TYPE ||
                    exp.getType().equals(type.getBaseType());
            this.exp = exp;
        }

        public Type.SubrangeType getSubrangeType() {
            return (Type.SubrangeType) getType();
        }

        public ExpNode getExp() {
            return exp;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitNarrowSubrangeNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitNarrowSubrangeNode(this);
        }

        @Override
        public String toString() {
            return "NarrowSubrange(" + exp + ":" + type + ")";
        }
    }

    /**
     * Tree node representing a widening of a subrange
     */
    public static class WidenSubrangeNode extends ExpNode {
        /**
         * Expression to be widened
         */
        private final ExpNode exp;

        /* @requires exp.getType() instanceof Type.SubrangeType */
        public WidenSubrangeNode(ExpNode exp) {
            super(exp.getLocation(), ((Type.SubrangeType) exp.getType()).getBaseType());
            this.exp = exp;
        }

        public ExpNode getExp() {
            return exp;
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitWidenSubrangeNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return visitor.visitWidenSubrangeNode(this);
        }

        @Override
        public String toString() {
            return "WidenSubrange(" + exp + ":" + getType() + ")";
        }
    }

    /* -------------------- Added Nodes --------------------*/

    /**
     * Tree node representing an ExpList
     */
    public static class ExpListNode extends ExpNode {
        private ArrayList<ExpNode> expList;

        public ExpListNode(Location loc, Type type) {
            super(loc, type);
        }

        public ExpListNode(Location loc) {
            super(loc);
            this.expList = new ArrayList<>();
        }

        public void add(ExpNode exp) {
            expList.add(exp);
        }

        public ArrayList<ExpNode> getExpList() {
            return expList;
        }

        @Override
        public Type getType() {
            return super.getType();
        }

        @Override
        public void setType(Type type) {
            super.setType(type);
        }

        @Override
        public Location getLocation() {
            return super.getLocation();
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitExpListNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return null;
        }
    }

    /**
     * Tree node representing a record made from the constructor
     */
    public static class NewRecordNode extends ExpNode {
        private ExpNode initFields;
        private Type type;
        private HashMap<String, ExpNode> record;

        public NewRecordNode(Location loc, Type type, ExpNode initFields) {
            super(loc, type);
            this.type = type;
            this.initFields = initFields;
            this.record = new HashMap<>();
        }

        /*
         * Used during static checking to enter valid fields into node
         */
        public void enter(Type.Field field, ExpNode value) {
            record.put(field.getId(), value);
        }


        public ExpNode getFields() {
            return initFields;
        }

        @Override
        public Type getType() {
            return this.type;
        }

        @Override
        public void setType(Type type) {
            this.type = type;
        }

        @Override
        public Location getLocation() {
            return super.getLocation();
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitNewRecordNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return null;
        }
    }

    /**
     * Tree node representing a new pointer
     */
    public static class NewPointerNode extends ExpNode {
        Type pointerType;

        public NewPointerNode(Location loc, Type type) {
            super(loc, type);
            this.pointerType = type;
        }

        @Override
        public Type getType() {
            return super.getType();
        }

        @Override
        public void setType(Type type) {
            super.setType(type);
        }

        @Override
        public Location getLocation() {
            return super.getLocation();
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitNewPointerNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return null;
        }
    }

    /**
     * Tree node representing a record field reference
     */
    public static class RecordFieldAccessNode extends ExpNode {
        ExpNode value;
        String id;

        public RecordFieldAccessNode(Location loc, Type type) {
            super(loc, type);
        }

        public RecordFieldAccessNode(Location loc, ExpNode value, String id) {
            super(loc);
            this.id = id;
            this.value = value;
        }

        public ExpNode getValue() {
            return value;
        }

        public String getId() {
            return id;
        }

        @Override
        public Type getType() {
            return super.getType();
        }

        @Override
        public void setType(Type type) {
            super.setType(type);
        }

        @Override
        public Location getLocation() {
            return super.getLocation();
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitRecordFieldAccessNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return null;
        }
    }

    /**
     * Tree node representing a deferenced pointer
     */
    public static class PointerDereferenceNode extends ExpNode {
        ExpNode lval;

        public PointerDereferenceNode(Location loc, Type type, ExpNode lval) {
            super(loc, type);
            this.lval = lval;
        }

        public PointerDereferenceNode(Location loc, ExpNode lval) {
            super(loc);
            this.lval = lval;
        }

        @Override
        public Type getType() {
            return super.getType();
        }

        @Override
        public void setType(Type type) {
            super.setType(type);
        }

        @Override
        public Location getLocation() {
            return super.getLocation();
        }

        @Override
        public ExpNode transform(ExpTransform<ExpNode> visitor) {
            return visitor.visitPointerDereferenceNode(this);
        }

        @Override
        public Code genCode(ExpTransform<Code> visitor) {
            return null;
        }
    }

}