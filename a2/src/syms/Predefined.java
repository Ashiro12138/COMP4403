package syms;

import source.ErrorHandler;
import syms.Type.FunctionType;
import syms.Type.PointerType;
import machine.StackMachine;
import syms.Type.ProductType;
import syms.Type.ScalarType;

/**
 * class Predefined - handles the predefined types and symbols.
 */

public class Predefined {
    /**
     * Predefined integer type.
     */
    public static ScalarType INTEGER_TYPE;
    /**
     * Predefined boolean type.
     */
    public static ScalarType BOOLEAN_TYPE;
    /**
     * Predefined type of a nil pointer
     */
    static PointerType NIL_TYPE;

    /**
     * Add the predefined constants, types and operators
     *
     * @param predefined scope to which entries are added
     */
    static void addPredefinedEntries(Scope predefined) {
        // Define types needed for predefined entries
        /* Predefined integer type. */
        INTEGER_TYPE = new ScalarType("int", Type.SIZE_OF_INT,
                Integer.MIN_VALUE, Integer.MAX_VALUE) {
        };
        /* Predefined boolean type. */
        BOOLEAN_TYPE = new ScalarType("boolean", Type.SIZE_OF_BOOLEAN,
                Type.FALSE_VALUE, Type.TRUE_VALUE);
        /* Predefined type of a nil pointer */
        NIL_TYPE = new PointerType(Type.ERROR_TYPE) {
            {
                name = "nil_type";
            }
        };
        ProductType NIL_TYPE_PAIR = new ProductType(NIL_TYPE, NIL_TYPE);
        FunctionType NIL_RELATIONAL_TYPE = new FunctionType(NIL_TYPE_PAIR, BOOLEAN_TYPE);

        ProductType PAIR_INTEGER_TYPE = new ProductType(INTEGER_TYPE, INTEGER_TYPE);
        ProductType PAIR_BOOLEAN_TYPE = new ProductType(BOOLEAN_TYPE, BOOLEAN_TYPE);
        FunctionType ARITHMETIC_BINARY = new FunctionType(PAIR_INTEGER_TYPE, INTEGER_TYPE);
        FunctionType INT_RELATIONAL_TYPE = new FunctionType(PAIR_INTEGER_TYPE, BOOLEAN_TYPE);
        FunctionType LOGICAL_BINARY = new FunctionType(PAIR_BOOLEAN_TYPE, BOOLEAN_TYPE);
        FunctionType ARITHMETIC_UNARY = new FunctionType(INTEGER_TYPE, INTEGER_TYPE);
        FunctionType LOGICAL_UNARY = new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE);
        // Add predefined symbols to predefined scope
        predefined.addType("int", ErrorHandler.NO_LOCATION, INTEGER_TYPE);
        predefined.addType("boolean", ErrorHandler.NO_LOCATION, BOOLEAN_TYPE);
        predefined.addConstant("false", ErrorHandler.NO_LOCATION, BOOLEAN_TYPE,
                Type.FALSE_VALUE);
        predefined.addConstant("true", ErrorHandler.NO_LOCATION, BOOLEAN_TYPE,
                Type.TRUE_VALUE);
        predefined.addOperator("_=_", ErrorHandler.NO_LOCATION, LOGICAL_BINARY);
        predefined.addOperator("_!=_", ErrorHandler.NO_LOCATION, LOGICAL_BINARY);
        predefined.addConstant("nil", ErrorHandler.NO_LOCATION, NIL_TYPE,
                StackMachine.NULL_ADDR);
        predefined.addOperator("_=_", ErrorHandler.NO_LOCATION, NIL_RELATIONAL_TYPE);
        predefined.addOperator("_!=_", ErrorHandler.NO_LOCATION, NIL_RELATIONAL_TYPE);
        predefined.addOperator("-_", ErrorHandler.NO_LOCATION, ARITHMETIC_UNARY);
        predefined.addOperator("_+_", ErrorHandler.NO_LOCATION, ARITHMETIC_BINARY);
        predefined.addOperator("_-_", ErrorHandler.NO_LOCATION, ARITHMETIC_BINARY);
        predefined.addOperator("_*_", ErrorHandler.NO_LOCATION, ARITHMETIC_BINARY);
        predefined.addOperator("_/_", ErrorHandler.NO_LOCATION, ARITHMETIC_BINARY);
        predefined.addOperator("_=_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
        predefined.addOperator("_!=_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
        predefined.addOperator("_>_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
        predefined.addOperator("_<_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
        predefined.addOperator("_>=_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
        predefined.addOperator("_<=_", ErrorHandler.NO_LOCATION, INT_RELATIONAL_TYPE);
    }
}
