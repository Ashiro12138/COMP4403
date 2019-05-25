package tree;

import java.lang.ref.Reference;
import java.util.*;

import source.VisitorDebugger;
import source.Errors;
import java_cup.runtime.ComplexSymbolFactory.Location;
import syms.Predefined;
import syms.SymEntry;
import syms.Scope;
import syms.Type;
import syms.Type.IncompatibleTypes;
import tree.DeclNode.DeclListNode;
import tree.StatementNode.*;

/**
 * class StaticSemantics - Performs the static semantic checks on
 * the abstract syntax tree using a visitor pattern to traverse the tree.
 * See the notes on the static semantics of PL0 to understand the PL0
 * type system in detail.
 */
public class StaticChecker implements DeclVisitor, StatementVisitor,
        ExpTransform<ExpNode> {

    /**
     * The static checker maintains a reference to the current
     * symbol table scope for the procedure currently being processed.
     */
    private Scope currentScope;
    /**
     * Errors are reported through the error handler.
     */
    private final Errors errors;
    /**
     * Debug messages are reported through the visitor debugger.
     */
    private final VisitorDebugger debug;

    /**
     * Construct a static checker for PL0.
     *
     * @param errors is the error message handler.
     */
    public StaticChecker(Errors errors) {
        super();
        this.errors = errors;
        debug = new VisitorDebugger("checking", errors);
    }

    /**
     * The tree traversal starts with a call to visitProgramNode.
     * Then its descendants are visited using visit methods for each
     * node type, which are called using the visitor pattern "accept"
     * method (or "transform" for expression nodes) of the abstract
     * syntax tree node.
     */
    public void visitProgramNode(DeclNode.ProcedureNode node) {
        beginCheck("Program");
        // The main program is a special case of a procedure
        visitProcedureNode(node);
        endCheck("Program");
    }

    /**
     * Procedure, function or main program node
     */
    public void visitProcedureNode(DeclNode.ProcedureNode node) {
        beginCheck("Procedure");
        SymEntry.ProcedureEntry procEntry = node.getProcEntry();
        // Assign a code block to the procedure entry
        procEntry.setBlock(node.getBlock());
        // The local scope is that for the procedure.
        Scope localScope = procEntry.getLocalScope();
        /* Resolve all references to identifiers within the declarations. */
        localScope.resolveScope();
        // Enter the local scope
        currentScope = localScope;
        // Check the block of the procedure.
        visitBlockNode(node.getBlock());
        // Restore the symbol table to the parent scope
        currentScope = currentScope.getParent();
        endCheck("Procedure");
    }

    /**
     * Block node
     */
    public void visitBlockNode(BlockNode node) {
        beginCheck("Block");
        node.getProcedures().accept(this);  // Check the procedures, if any
        node.getBody().accept(this);        // Check the body of the block
        endCheck("Block");
    }

    /**
     * Process the list of procedure declarations
     */
    public void visitDeclListNode(DeclListNode node) {
        beginCheck("DeclList");
        for (DeclNode declaration : node.getDeclarations()) {
            declaration.accept(this);
        }
        endCheck("DeclList");
    }


    /*************************************************
     *  Statement node static checker visit methods
     *************************************************/
    public void visitStatementErrorNode(StatementNode.ErrorNode node) {
        beginCheck("StatementError");
        // Nothing to check - already invalid.
        endCheck("StatementError");
    }

    /**
     * Assignment statement node
     */
    public void visitAssignmentNode(StatementNode.AssignmentNode node) {
        beginCheck("Assignment");
        // Check the left side left value.
        ExpNode left = node.getVariable().transform(this);
        node.setVariable(left);
        // Check the right side expression.
        ExpNode exp = node.getExp().transform(this);
        node.setExp(exp);
        // Validate that it is a true left value and not a constant.
        Type lValType = left.getType();
        if (lValType instanceof Type.ReferenceType) {
            /* Validate that the right side expression is assignment
             * compatible with the left value. This requires that the
             * right side expression is coerced to the base type of
             * type of the left side LValue. */
            Type baseType = ((Type.ReferenceType) lValType).getBaseType();
            node.setExp(baseType.coerceExp(exp));
        } else if (lValType != Type.ERROR_TYPE) {
                staticError("variable expected", left.getLocation());
        }
        endCheck("Assignment");
    }

    /**
     * Write statement node
     */
    public void visitWriteNode(StatementNode.WriteNode node) {
        beginCheck("Write");
        // Check the expression being written.
        ExpNode exp = node.getExp().transform(this);
        // coerce expression to be of type integer,
        // or complain if not possible.
        node.setExp(Predefined.INTEGER_TYPE.coerceExp(exp));
        endCheck("Write");
    }


    /**
     * Call statement node
     */
    public void visitCallNode(StatementNode.CallNode node) {
        beginCheck("Call");
        SymEntry.ProcedureEntry procEntry;
        // Look up the symbol table entry for the procedure.
        SymEntry entry = currentScope.lookup(node.getId());
        if (entry instanceof SymEntry.ProcedureEntry) {
            procEntry = (SymEntry.ProcedureEntry) entry;
            node.setEntry(procEntry);
        } else {
            staticError("Procedure identifier required", node.getLocation());
            endCheck("Call");
            return;
        }

        // Need to also check the parameters
        List<ExpNode> actualParams = node.getActualParamList();
        Type.ProcedureType procType = procEntry.getType();
        List<SymEntry.ParamEntry> formalParams = procType.getFormalParams();
        List idCheck = new ArrayList<String>();

        if (actualParams.size() > formalParams.size()) {
            staticError("Too many parameters", node.getLocation());
            return;
        }

        for (SymEntry.ParamEntry formalParam: formalParams) {

            // id <- exp, where id is a formal parameter of p and exp is its expression
            String formalId = formalParam.getIdent();

            for (ExpNode actual: actualParams) {
                actual.transform(this);
                if (!(actual instanceof ExpNode.ActualParamNode)) {
                    staticError("Expected actual param", actual.getLocation());
                    return;
                }
                ExpNode.ActualParamNode actualNode = (ExpNode.ActualParamNode) actual;

                if (formalId.contentEquals(actualNode.getFormalId())) {
                    ExpNode condition = actualNode.getCondition();
                    //Type.ReferenceType formalType = formalParam.getType();
                    Type formalBaseType = formalParam.getType().getBaseType();
                    Type conditionType = condition.getType();

                    if (formalParam.isRef()) {
                        if (conditionType instanceof Type.ReferenceType) {
                            // if id is a formal reference parameter of type T, exp must be of the type ref(T)
                            Type actualBaseType =
                                    ((Type.ReferenceType) conditionType).getBaseType();
                            if (actualBaseType.equals(formalBaseType)) {
                                idCheck.add(formalId); // valid
                                break; // move onto next formal param
                            } else {
                                staticError("Reference parameter and formal parameter types do " +
                                        "not match", condition.getLocation());
                            }
                        } else {
                            staticError("Condition for reference param is not a reference type",
                                    condition.getLocation());
                        }

                    } else {
                        // if id is a formal value parameter of Type T, exp must be assignment compatible with T
                        // get type and resolve
                        try {
                            formalBaseType.coerceToType(condition); // assignment compatibility
                            idCheck.add(formalId); // valid
                            break; // move onto next formal param
                        } catch (Type.IncompatibleTypes err) {
                            if(conditionType != Type.ERROR_TYPE) {
                                staticError("Cannot coerce type " + conditionType
                                        + "to type " + formalBaseType, condition.getLocation());
                            }
                        }
                    }
                }
            }
            // Unable to find an actual param for this formal param, check if has default
            ExpNode isDefault = formalParam.getDefaultExp();
            if (!(isDefault instanceof ExpNode.ErrorNode)) {
                isDefault.transform(this);

            }
            // A formal parameter to p does not have a corresponding actual parameter in the call
            // If there exists a default expression -
                // default exp must be defined in the declaration scope of the procedure
                // must satosfu the same typing rules as an actual parameter


            // An actual param has a type and a formal param associated with it
            // All formal parameters must be in the actual param list only once


            // Each of these actual params must be valid in the calling scope
            // If the entry is a value param, it must be coercible to the formal param
            // If the entry if a ref param, it must be a ref(type) of type formal param
        }
        endCheck("Call");
    }

    /**
     * Statement list node
     */
    public void visitStatementListNode(StatementNode.ListNode node) {
        beginCheck("StatementList");
        for (StatementNode s : node.getStatements()) {
            s.accept(this);
        }
        endCheck("StatementList");
    }

    /**
     * Check that the expression node can be coerced to boolean
     */
    private ExpNode checkCondition(ExpNode cond) {
        // Check and transform the condition
        cond = cond.transform(this);
        /* Validate that the condition is boolean, which may require
         * coercing the condition to be of type boolean. */
        return Predefined.BOOLEAN_TYPE.coerceExp(cond);
    }

    /**
     * If statement node
     */
    public void visitIfNode(StatementNode.IfNode node) {
        beginCheck("If");
        // Check the condition and replace with (possibly) transformed node
        node.setCondition(checkCondition(node.getCondition()));
        node.getThenStmt().accept(this);  // Check the 'then' part
        node.getElseStmt().accept(this);  // Check the 'else' part.
        endCheck("If");
    }

    /**
     * While statement node
     */
    public void visitWhileNode(StatementNode.WhileNode node) {
        beginCheck("While");
        // Check the condition and replace with (possibly) transformed node
        node.setCondition(checkCondition(node.getCondition()));
        node.getLoopStmt().accept(this);  // Check the body of the loop
        endCheck("While");
    }

    /*************************************************
     *  Expression node static checker visit methods.
     *  The static checking visitor methods for expressions
     *  transform the expression to include resolved identifier
     *  nodes, and add nodes like dereference nodes, and
     *  narrow and widen subrange nodes.
     *  These ensure that the transformed tree is type consistent
     *  and must ensure the type of the node is set appropriately.
     *************************************************/
    public ExpNode visitErrorExpNode(ExpNode.ErrorNode node) {
        beginCheck("ErrorExp");
        // Nothing to do - already invalid.
        endCheck("ErrorExp");
        return node;
    }

    /**
     * Constant expression node
     */
    public ExpNode visitConstNode(ExpNode.ConstNode node) {
        beginCheck("Const");
        // type already set up
        endCheck("Const");
        return node;
    }

    /**
     * Reads an integer value from input
     */
    public ExpNode visitReadNode(ExpNode.ReadNode node) {
        beginCheck("Read");
        // type already set up
        endCheck("Read");
        return node;
    }

    /**
     * Handles binary operators
     */
    public ExpNode visitBinaryNode(ExpNode.BinaryNode node) {
        beginCheck("Binary");
        /* Operators can be overloaded */
        /* Check the arguments to the operator */
        ExpNode left = node.getLeft().transform(this);
        ExpNode right = node.getRight().transform(this);
        /* Lookup the operator in the symbol table to get its type */
        Type opType = currentScope.lookupOperator(
                node.getOp().getName()).getType();
        if (opType instanceof Type.FunctionType) {
            /* The operator is not overloaded. Its type is represented
             * by a FunctionType from its argument's type to its
             * result type.
             */
            Type.FunctionType fType = (Type.FunctionType) opType;
            List<Type> argTypes = ((Type.ProductType)fType.getArgType()).getTypes();
            node.setLeft(argTypes.get(0).coerceExp(left));
            node.setRight(argTypes.get(1).coerceExp(right));
            node.setType(fType.getResultType());
        } else if (opType instanceof Type.IntersectionType) {
            /* The operator is overloaded. Its type is represented
             * by an IntersectionType containing a set of possible
             * types for the operator, each of which is a FunctionType.
             * Each possible type is tried until one succeeds.
             */
            errors.debugMessage("Coercing " + left + " and " + right + " to " + opType);
            errors.incDebug();
            for (Type t : ((Type.IntersectionType) opType).getTypes()) {
                Type.FunctionType fType = (Type.FunctionType) t;
                List<Type> argTypes = ((Type.ProductType)fType.getArgType()).getTypes();
                try {
                    /* Coerce the argument to the argument type for
                     * this operator type. If the coercion fails an
                     * exception will be trapped and an alternative
                     * function type within the intersection tried.
                     */
                    ExpNode newLeft = argTypes.get(0).coerceToType(left);
                    ExpNode newRight = argTypes.get(1).coerceToType(right);
                    /* The coercion succeeded if we get here */
                    node.setLeft(newLeft);
                    node.setRight(newRight);
                    node.setType(fType.getResultType());
                    errors.decDebug();
                    endCheck("Binary");
                    return node;
                } catch (IncompatibleTypes ex) {
                    // Allow "for" loop to try an alternative
                }
            }
            errors.decDebug();
            errors.debugMessage("Failed to coerce " + left + " and " + right +
                    " to " + opType);
            // no match in intersection type
            staticError("Type of argument (" + left.getType().getName() + "*" +
                    right.getType().getName() +
                    ") does not match " + opType.getName(), node.getLocation());
            node.setType(Type.ERROR_TYPE);
        } else {
            errors.fatal("Invalid operator type", node.getLocation());
        }
        endCheck("Binary");
        return node;
    }

    /**
     * Handles unary operators
     */
    public ExpNode visitUnaryNode(ExpNode.UnaryNode node) {
        beginCheck("Unary");
        /* Operators can be overloaded */
        /* Check the argument to the operator */
        ExpNode arg = node.getArg().transform(this);
        /* Lookup the operator in the symbol table to get its type */
        Type opType = currentScope.lookupOperator(
                node.getOp().getName()).getType();
        if (opType instanceof Type.FunctionType) {
            /* The operator is not overloaded. Its type is represented
             * by a FunctionType from its argument's type to its
             * result type.
             */
            Type.FunctionType fType = (Type.FunctionType) opType;
            Type argType = fType.getArgType();
            node.setArg(argType.coerceExp(arg));
            node.setType(fType.getResultType());
        } else if (opType instanceof Type.IntersectionType) {
            /* The operator is overloaded. Its type is represented
             * by an IntersectionType containing a set of possible
             * types for the operator, each of which is a FunctionType.
             * Each possible type is tried until one succeeds.
             */
            errors.debugMessage("Coercing " + arg + " to " + opType);
            errors.incDebug();
            for (Type t : ((Type.IntersectionType) opType).getTypes()) {
                Type.FunctionType fType = (Type.FunctionType) t;
                Type argType = fType.getArgType();
                try {
                    /* Coerce the argument to the argument type for
                     * this operator type. If the coercion fails an
                     * exception will be trapped and an alternative
                     * function type within the intersection tried.
                     */
                    ExpNode newArg = argType.coerceToType(arg);
                    /* The coercion succeeded if we get here */
                    node.setArg(newArg);
                    node.setType(fType.getResultType());
                    errors.decDebug();
                    endCheck("Unary");
                    return node;
                } catch (IncompatibleTypes ex) {
                    // Allow "for" loop to try an alternative
                }
            }
            errors.decDebug();
            errors.debugMessage("Failed to coerce " + arg + " to " + opType);
            // no match in intersection type
            staticError("Type of argument " + arg.getType().getName() +
                    " does not match " + opType.getName(), node.getLocation());
            node.setType(Type.ERROR_TYPE);
        } else {
            errors.fatal("Invalid operator type", node.getLocation());
        }
        endCheck("Unary");
        return node;
    }


    /**
     * A DereferenceNode allows a variable (of type ref(int) say) to be
     * dereferenced to get its value (of type int).
     */
    public ExpNode visitDereferenceNode(ExpNode.DereferenceNode node) {
        beginCheck("Dereference");
        // Check the left value referred to by this dereference node
        ExpNode lVal = node.getLeftValue().transform(this);
        node.setLeftValue(lVal);
        /* The type of the dereference node is the base type of its
         * left value. */
        Type lValueType = lVal.getType();
        if (lValueType instanceof Type.ReferenceType) {
            node.setType(lValueType.optDereferenceType()); // not optional here
        } else if (lValueType != Type.ERROR_TYPE) { // avoid cascading errors
            staticError("cannot dereference an expression which isn't a reference",
                    node.getLocation());
        }
        endCheck("Dereference");
        return node;
    }

    /**
     * When parsing an identifier within an expression one can't tell
     * whether it has been declared as a constant or an identifier.
     * Here we check which it is and return either a constant or
     * a variable node.
     */
    public ExpNode visitIdentifierNode(ExpNode.IdentifierNode node) {
        beginCheck("Identifier");
        // First we look up the identifier in the symbol table.
        ExpNode newNode;
        SymEntry entry = currentScope.lookup(node.getId());
        if (entry instanceof SymEntry.ConstantEntry) {
            // Set up a new node which is a constant.
            debugMessage("Transformed " + node.getId() + " to Constant");
            SymEntry.ConstantEntry constEntry =
                    (SymEntry.ConstantEntry) entry;
            newNode = new ExpNode.ConstNode(node.getLocation(),
                    constEntry.getType(), constEntry.getValue());
        } else if (entry instanceof SymEntry.VarEntry) {
            debugMessage("Transformed " + node.getId() + " to Variable");
            // Set up a new node which is a variable.
            SymEntry.VarEntry varEntry = (SymEntry.VarEntry) entry;
            newNode = new ExpNode.VariableNode(node.getLocation(), varEntry);
        } else {
            // Undefined identifier or a type or procedure identifier.
            // Set up new node to be an error node.
            newNode = new ExpNode.ErrorNode(node.getLocation());
            //System.out.println("Entry = " + entry);
            staticError("Constant or variable identifier required", node.getLocation());
        }
        endCheck("Identifier");
        return newNode;
    }

    /**
     * Variable node is set up by Identifier node - no checks needed
     */
    public ExpNode visitVariableNode(ExpNode.VariableNode node) {
        beginCheck("Variable");
        // Type already set up
        endCheck("Variable");
        return node;
    }

    /**
     * Narrow subrange node constructed during coerce - no checking needed
     */
    public ExpNode visitNarrowSubrangeNode(ExpNode.NarrowSubrangeNode node) {
        beginCheck("NarrowSubrange");
        // Nothing to do.
        endCheck("NarrowSubrange");
        return node;
    }

    /**
     * Widen subrange node constructed during coerce - no checking needed
     */
    public ExpNode visitWidenSubrangeNode(ExpNode.WidenSubrangeNode node) {
        beginCheck("WidenSubrange");
        // Nothing to do.
        endCheck("WidenSubrange");
        return node;
    }

    /**
     * Actual param node
     */
    public ExpNode visitActualParamNode(ExpNode.ActualParamNode node) {
        beginCheck("ActualParams");
        // Only knows the condition
        ExpNode condition = node.getCondition().transform(this);
        node.setCondition(condition);
        // Cannot check anything related to the procedure's declaration scope - see visitCallNode
        endCheck("ActualParams");
        return node;
    }

    //**************************** Support Methods

    /**
     * Push current node onto debug rule stack and increase debug level
     */
    private void beginCheck(String nodeName) {
        debug.beginDebug(nodeName);
    }

    /**
     * Pop current node from debug rule stack and decrease debug level
     */
    private void endCheck(String node) {
        debug.endDebug(node);
    }

    /**
     * Debugging message output
     */
    private void debugMessage(String msg) {
        errors.debugMessage(msg);
    }

    /**
     * Error message handle for parsing errors
     */
    private void staticError(String msg, Location loc) {
        errors.debugMessage(msg);
        errors.error(msg, loc);
    }
}
