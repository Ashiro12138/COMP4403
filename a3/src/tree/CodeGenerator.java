package tree;

import java.util.List;

import machine.Operation;
import machine.StackMachine;
import source.Errors;
import source.VisitorDebugger;
import syms.SymEntry;
import syms.Type;
import tree.StatementNode.*;

/**
 * class CodeGenerator implements code generation using the
 * visitor pattern to traverse the abstract syntax tree.
 */
public class CodeGenerator implements DeclVisitor, StatementTransform<Code>,
        ExpTransform<Code> {
    /**
     * Current static level of nesting into procedures.
     */
    private int staticLevel;

    /**
     * Table of code for each procedure
     */
    private final Procedures procedures;

    /**
     * Error message handler
     */
    private final Errors errors;

    /**
     * Debug messages are reported through the visitor debugger.
     */
    private final VisitorDebugger debug;


    public CodeGenerator(Errors errors) {
        super();
        this.errors = errors;
        debug = new VisitorDebugger("generating", errors);
        procedures = new Procedures();
    }

    /**
     * Main generate code method for the program.
     */
    public Procedures generateCode(DeclNode.ProcedureNode node) {
        beginGen("Program");
        staticLevel = node.getBlock().getBlockLocals().getLevel();
        assert staticLevel == 1;  // Main program is at static level 1
        /* Generate the code for the main program and all procedures */
        visitProcedureNode(node);
        endGen("Program");
        return procedures;
    }

    /* -------------------- Visitor methods ----------------------------*/

    /**
     * Generate code for a single procedure.
     */
    public void visitProcedureNode(DeclNode.ProcedureNode node) {
        beginGen("Procedure");
        // Generate code for the block
        Code code = visitBlockNode(node.getBlock());
            code.generateOp(Operation.RETURN);
        procedures.addProcedure(node.getProcEntry(), code);
        //System.out.println(node.getProcEntry().getIdent() + "\n" + code);
        endGen("Procedure");
    }

    /**
     * Generate code for a block.
     */
    public Code visitBlockNode(BlockNode node) {
        beginGen("Block");
        /* Generate code to allocate space for local variables on
         * procedure entry.
         */
        Code code = new Code();
        code.genAllocStack(node.getBlockLocals().getVariableSpace());
        /* Generate the code for the body */
        code.append(node.getBody().genCode(this));
        /* Generate code for local procedures.
         * Static level is one greater for the procedures.
         */
        staticLevel++;
        node.getProcedures().accept(this);
        staticLevel--;
        endGen("Block");
        return code;
    }

    /**
     * Code generation for a list of procedures
     */
    public void visitDeclListNode(DeclNode.DeclListNode node) {
        beginGen("DeclList");
        for (DeclNode decl : node.getDeclarations()) {
            decl.accept(this);
        }
        endGen("DeclList");
    }


    //**************  Statement node code generation visit methods

    /**
     * Code generation for an erroneous statement should not be attempted.
     */
    public Code visitStatementErrorNode(StatementNode.ErrorNode node) {
        errors.fatal("PL0 Internal error: generateCode for Statement Error Node",
                node.getLocation());
        return null;
    }


    /**
     * Code generation for an assignment statement.
     */
    public Code visitAssignmentNode(StatementNode.AssignmentNode node) {
        beginGen("Assignment");
        /* Generate code to evaluate the expression */
        Code code = node.getExp().genCode(this);
        node.getVariable();
        /* Generate the code to load the address of the variable */
        code.append(node.getVariable().genCode(this));
        /* Generate the store based on the type/size of value */
        code.genStore(node.getExp().getType());
        endGen("Assignment");
        return code;
    }

    /**
     * Generate code for a "write" statement.
     */
    public Code visitWriteNode(StatementNode.WriteNode node) {
        beginGen("Write");
        Code code = node.getExp().genCode(this);
        code.generateOp(Operation.WRITE);
        endGen("Write");
        return code;
    }

    /**
     * Generate code for a "call" statement.
     */
    public Code visitCallNode(StatementNode.CallNode node) {
        beginGen("Call");
        SymEntry.ProcedureEntry proc = node.getEntry();
        Code code = new Code();

        /* Load all formal parameters backwards before load call. For each formal parameter, the
         * value is found as either an actual or a default expression. Visitor generates code
         * for each expression.
         *
         * We want to finish with values of value parameters and absolute addresses of reference
         * parameters pushed onto the stack, for all the formal parameters.
         *
         * This means that if a formal parameter is value parameter, by endGen("Call") it wouldn't
         * matter whether the value came from a Const or a Variable as long as the correct
         * value is loaded onto the stack.
         *
         * genCode for Const loads the value directly, so nothing else needs to be done.
         * However, genCode for Variable loads the absolute address if it is a reference
         * parameter, or it loads the frame relative pointer if it is not. (see
         * visitVariableNode). Loading the "final" value frame is handled in line 221.
         */
        Type.ProcedureType procType = proc.getType();
        List<SymEntry.ParamEntry> formalParams = procType.getFormalParams();
        List<ExpNode> actualParams = node.getActualParamList();
        boolean hasActual;
        boolean isVar = false;
        int offset = 0 - (formalParams.size() + 1);

        // Check whether each formal param has an actual param
        for (SymEntry.ParamEntry formalParam: formalParams) {
            String idToFind = formalParam.getIdent();
            hasActual = false;

            for (ExpNode actualParam: actualParams) {
                ExpNode.ActualParamNode actualAsNode = (ExpNode.ActualParamNode) actualParam;
                if (actualAsNode.getFormalId().equals(idToFind)) {
                    // Use this actual parameter's condition as the value for the formal param
                    code.append(actualAsNode.genCode(this));
                    if (actualAsNode.getCondition() instanceof ExpNode.VariableNode) {
                        isVar = true;
                    }
                    hasActual = true;
                    break;
                }
            }

            if (!hasActual) {
                // If no actual param, load default instead
                code.append(formalParam.getDefaultExp().genCode(this));
                if (formalParam.getDefaultExp() instanceof ExpNode.VariableNode) {
                    isVar = true;
                }

            }
            /* If Const, would have loaded const value onto stack - we skip to loading call.
             */
            if (isVar) {
                if (formalParam.isRef()) {
                    /* If Variable and isRef, would have loaded the offset from the fp onto stack -
                     * need absolute address.
                     */
                    code.generateOp(Operation.TO_GLOBAL);
                } else {
                    /* If Variable and is not ref, would have loaded the absolute address -
                     * need the value at the address.
                     */
                    code.generateOp(Operation.LOAD_ABS);
                }
            }

            formalParam.setOffset(++offset);
        }

        /* Generate the call instruction. The second parameter is the
         * procedure's symbol table entry. The actual address is resolved
         * at load time.
         */
        code.genCall(staticLevel - proc.getLevel(), proc);

        endGen("Call");
        return code;
    }

    /**
     * Generate code for a statement list
     */
    public Code visitStatementListNode(StatementNode.ListNode node) {
        beginGen("StatementList");
        Code code = new Code();
        for (StatementNode s : node.getStatements()) {
            code.append(s.genCode(this));
        }
        endGen("StatementList");
        return code;
    }

    /**
     * Generate code for an "if" statement.
     */
    public Code visitIfNode(StatementNode.IfNode node) {
        beginGen("If");
        Code code = new Code();
        /* Generate the code for the if-then-else
         * from the code for its components */
        code.genIfThenElse(node.getCondition().genCode(this),
                node.getThenStmt().genCode(this),
                node.getElseStmt().genCode(this));
        endGen("If");
        return code;
    }

    /**
     * Generate code for a "while" statement.
     */
    public Code visitWhileNode(StatementNode.WhileNode node) {
        beginGen("While");
        /* Generate the code to evaluate the condition. */
        Code code = node.getCondition().genCode(this);
        /* Generate the code for the loop body */
        Code bodyCode = node.getLoopStmt().genCode(this);
        /* Add a branch over the loop body on false.
         * The offset is the size of the loop body code plus
         * the size of the branch to follow the body.
         */
        code.genJumpIfFalse(bodyCode.size() + Code.SIZE_JUMP_ALWAYS);
        /* Append the code for the body */
        code.append(bodyCode);
        /* Add a branch back to the condition.
         * The offset is the total size of the current code plus the
         * size of a Jump Always (being generated).
         */
        code.genJumpAlways(-(code.size() + Code.SIZE_JUMP_ALWAYS));
        endGen("While");
        return code;
    }
    //************* Expression node code generation visit methods

    /**
     * Code generation for an erroneous expression should not be attempted.
     */
    public Code visitErrorExpNode(ExpNode.ErrorNode node) {
        errors.fatal("PL0 Internal error: generateCode for ErrorExpNode",
                node.getLocation());
        return null;
    }

    /**
     * Generate code for a constant expression.
     */
    public Code visitConstNode(ExpNode.ConstNode node) {
        beginGen("Const");
        Code code = new Code();
        if (node.getValue() == 0) {
            code.generateOp(Operation.ZERO);
        } else if (node.getValue() == 1) {
            code.generateOp(Operation.ONE);
        } else {
            code.genLoadConstant(node.getValue());
        }
        endGen("Const");
        return code;
    }

    /**
     * Generate code for a "read" expression.
     */
    public Code visitReadNode(ExpNode.ReadNode node) {
        beginGen("Read");
        Code code = new Code();
        code.generateOp(Operation.READ);
        endGen("Read");
        return code;
    }

    /**
     * Generate operator operands in order
     */
    private Code genArgsInOrder(ExpNode left, ExpNode right) {
        beginGen("ArgsInOrder");
        Code code = left.genCode(this);
        code.append(right.genCode(this));
        endGen("ArgsInOrder");
        return code;
    }

    /**
     * Generate code for a binary operator expression.
     */
    public Code visitBinaryNode(ExpNode.BinaryNode node) {
        beginGen("Binary");
        Code code;
        ExpNode left = node.getLeft();
        ExpNode right = node.getRight();
        switch (node.getOp()) {
            case ADD_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.ADD);
                break;
            case SUB_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.NEGATE);
                code.generateOp(Operation.ADD);
                break;
            case MUL_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.MPY);
                break;
            case DIV_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.DIV);
                break;
            case EQUALS_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.EQUAL);
                break;
            case LESS_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.LESS);
                break;
            case NEQUALS_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.EQUAL);
                code.genBoolNot();
                break;
            case LEQUALS_OP:
                code = genArgsInOrder(left, right);
                code.generateOp(Operation.LESSEQ);
                break;
            case GREATER_OP:
                /* Generate argument values in reverse order and use LESS */
                code = genArgsInOrder(right, left);
                code.generateOp(Operation.LESS);
                break;
            case GEQUALS_OP:
                /* Generate argument values in reverse order and use LESSEQ */
                code = genArgsInOrder(right, left);
                code.generateOp(Operation.LESSEQ);
                break;
            default:
                errors.fatal("PL0 Internal error: Unknown operator",
                        node.getLocation());
                code = null;
        }
        endGen("Binary");
        return code;
    }
    /**
     * Generate code for a unary operator expression.
     */
    public Code visitUnaryNode(ExpNode.UnaryNode node) {
        beginGen("Unary");
        Code code = node.getArg().genCode(this);
        switch (node.getOp()) {
            case NEG_OP:
                code.generateOp(Operation.NEGATE);
                break;
            default:
                errors.fatal("PL0 Internal error: Unknown operator",
                        node.getLocation());
        }
        endGen("Unary");
        return code;
    }

    /**
     * Generate code to dereference an RValue.
     */
    public Code visitDereferenceNode(ExpNode.DereferenceNode node) {
        beginGen("Dereference");
        Code code = node.getLeftValue().genCode(this);
        code.genLoad(node.getType());
        endGen("Dereference");
        return code;
    }

    /**
     * Generating code for an IdentifierNode is invalid because the
     * static checker should have converted all IdentifierNodes to
     * either ConstNodes or VariableNodes.
     */
    public Code visitIdentifierNode(ExpNode.IdentifierNode node) {
        errors.fatal("Internal error: code generator called on IdentifierNode",
                node.getLocation());
        return null;
    }

    /**
     * Generate code for a variable reference.
     * It pushes the address of the variable as an offset from the frame pointer
     */
    public Code visitVariableNode(ExpNode.VariableNode node) {
        beginGen("Variable");
        SymEntry.VarEntry var = node.getVariable();
        Code code = new Code();

        /* If the node's variable is a reference parameter, we want to load the address
         * that is stored in its frame. To be consistent with the return type, it is converted to a
         * frame pointer relative address.
         */
        if (var instanceof SymEntry.ParamEntry) {
            SymEntry.ParamEntry param = (SymEntry.ParamEntry) var;
            if (param.isRef()) {
                // Need to load absolute address stored in the variable's slot
                code.genLoadConstant(var.getOffset());
                code.generateOp(Operation.LOAD_FRAME);
                // Converts absolute address to frame relative
                code.generateOp(Operation.TO_LOCAL);
                endGen("Variable");
                return code;
            }
        }

        // Need to get address as offset from frame pointer
        code.genMemRef(staticLevel - var.getLevel(), var.getOffset());
        endGen("Variable");
        return code;
    }


    /**
     * Generate code to perform a bounds check on a subrange.
     */
    public Code visitNarrowSubrangeNode(ExpNode.NarrowSubrangeNode node) {
        beginGen("NarrowSubrange");
        Code code = node.getExp().genCode(this);
        code.genBoundsCheck(node.getSubrangeType().getLower(),
                node.getSubrangeType().getUpper());
        endGen("NarrowSubrange");
        return code;
    }

    /**
     * Generate code to widen a subrange to an integer.
     */
    public Code visitWidenSubrangeNode(ExpNode.WidenSubrangeNode node) {
        beginGen("WidenSubrange");
        /* Widening doesn't require anything extra other than
         * generating code for its expression.
         */
        Code code = node.getExp().genCode(this);
        endGen("WidenSubrange");
        return code;
    }

    /**
     * Generate code for an actual parameter
     */
    public Code visitActualParamNode(ExpNode.ActualParamNode node) {
        beginGen("ActualParams");

        // Static checker should have converted to relevant expression type
        Code code = node.getCondition().genCode(this);

        endGen("ActualParams");
        return code;
    }
    //**************************** Support Methods

    /**
     * Push current node onto debug rule stack and increase debug level
     */
    private void beginGen(String nodeName) {
        debug.beginDebug(nodeName);
    }

    /**
     * Pop current node from debug rule stack and decrease debug level
     */
    private void endGen(String node) {
        debug.endDebug(node);
    }

}
