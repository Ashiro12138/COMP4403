package tree;

import java.util.ArrayList;
import java.util.List;

import java_cup.runtime.ComplexSymbolFactory.Location;
import syms.Scope;
import syms.SymEntry;

/**
 * class StatementNode - Abstract syntax tree representation of statements.
 * Classes defined within StatementNode extend it to give nodes for each kind
 * of statement.
 */
public abstract class StatementNode {
    /**
     * Location in the input source program (line and column number effectively).
     * All statements have a location within the original source code.
     */
    private final Location loc;

    /**
     * Constructor
     */
    StatementNode(Location loc) {
        this.loc = loc;
    }

    public Location getLocation() {
        return loc;
    }

    /**
     * All statement nodes provide an accept method to implement the visitor
     * pattern to traverse the tree.
     *
     * @param visitor class implementing the details of the particular
     *                traversal.
     */
    public abstract void accept(StatementVisitor visitor);


    /**
     * Debugging output of a statement at an indent level
     */
    public abstract String toString(int level);

    /**
     * Debugging output at level 0
     */
    @Override
    public String toString() {
        return this.toString(0);
    }

    /**
     * Returns a string with a newline followed by spaces of length 2n.
     */
    public static String newLine(int n) {
        StringBuilder indent = new StringBuilder("\n");
        while (n > 0) {
            indent.append("  ");
            n--;
        }
        return indent.toString();
    }

    /**
     * Statement node representing an erroneous statement.
     */
    public static class ErrorNode extends StatementNode {
        public ErrorNode(Location loc) {
            super(loc);
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitStatementErrorNode(this);
        }

        @Override
        public String toString(int level) {
            return "ERROR";
        }
    }

    /**
     * Node representing a Block consisting of declarations and
     * body of a procedure, function, or the main program.
     */
    public static class BlockNode extends StatementNode {
        private final DeclNode.DeclListNode procedures; // declared within block
        private final StatementNode body; // compound statement body
        private final Scope blockLocals;  // scope of locals within block

        /**
         * Constructor for a block within a procedure
         */
        public BlockNode(Location loc, DeclNode.DeclListNode procedures,
                         StatementNode body, Scope blockLocals) {
            super(loc);
            this.procedures = procedures;
            this.body = body;
            this.blockLocals = blockLocals;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitBlockNode(this);
        }

        public DeclNode.DeclListNode getProcedures() {
            return procedures;
        }

        public StatementNode getBody() {
            return body;
        }

        @Override
        public String toString(int level) {
            return getProcedures().toString(level + 1) +
                    newLine(level) + "BEGIN" +
                    newLine(level + 1) + body.toString(level + 1) +
                    newLine(level) + "END";
        }
    }


    /**
     * Tree node representing an assignment statement.
     */
    public static class AssignmentNode extends StatementNode {
        /**
         * Tree node for expression on left hand side of an assignment.
         */
        private ExpNode lValue;
        /**
         * Tree node for the expression to be assigned.
         */
        private ExpNode exp;

        public AssignmentNode(Location loc, ExpNode variable, ExpNode exp) {
            super(loc);
            this.lValue = variable;
            this.exp = exp;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitAssignmentNode(this);
        }

        public ExpNode getVariable() {
            return lValue;
        }

        public void setVariable(ExpNode variable) {
            this.lValue = variable;
        }

        public ExpNode getExp() {
            return exp;
        }

        public void setExp(ExpNode exp) {
            this.exp = exp;
        }

        String getVariableName() {
            if (lValue instanceof ExpNode.VariableNode) {
                return
                        ((ExpNode.VariableNode) lValue).getVariable().getIdent();
            } else {
                return "<no_name>";
            }
        }

        @Override
        public String toString(int level) {
            return lValue.toString() + " := " + exp.toString();
        }
    }

    /**
     * Tree node representing a "write" statement.
     */
    public static class WriteNode extends StatementNode {
        private ExpNode exp;

        public WriteNode(Location loc, ExpNode exp) {
            super(loc);
            this.exp = exp;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitWriteNode(this);
        }

        public ExpNode getExp() {
            return exp;
        }

        public void setExp(ExpNode exp) {
            this.exp = exp;
        }

        @Override
        public String toString(int level) {
            return "WRITE " + exp.toString();
        }
    }

    /**
     * Tree node representing a "call" statement.
     */
    public static class CallNode extends StatementNode {
        private final String id;
        private SymEntry.ProcedureEntry procEntry;

        public CallNode(Location loc, String id) {
            super(loc);
            this.id = id;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitCallNode(this);
        }

        public String getId() {
            return id;
        }

        public SymEntry.ProcedureEntry getEntry() {
            return procEntry;
        }

        public void setEntry(SymEntry.ProcedureEntry entry) {
            this.procEntry = entry;
        }

        @Override
        public String toString(int level) {
            StringBuilder s = new StringBuilder("CALL " + id);
            return s + ")";
        }
    }

    /**
     * Tree node representing a statement list.
     */
    public static class ListNode extends StatementNode {
        private final List<StatementNode> statements;

        public ListNode(Location loc) {
            super(loc);
            this.statements = new ArrayList<>();
        }

        public void addStatement(StatementNode s) {
            statements.add(s);
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitStatementListNode(this);
        }

        public List<StatementNode> getStatements() {
            return statements;
        }

        @Override
        public String toString(int level) {
            StringBuilder result = new StringBuilder();
            String sep = "";
            for (StatementNode s : statements) {
                result.append(sep).append(s.toString(level));
                sep = ";" + newLine(level);
            }
            return result.toString();
        }
    }

    /**
     * Tree node representing an "if" statement.
     */
    public static class IfNode extends StatementNode {
        private ExpNode condition;
        private final StatementNode thenStmt;
        private final StatementNode elseStmt;

        public IfNode(Location loc, ExpNode condition,
                      StatementNode thenStmt, StatementNode elseStmt) {
            super(loc);
            this.condition = condition;
            this.thenStmt = thenStmt;
            this.elseStmt = elseStmt;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitIfNode(this);
        }

        public ExpNode getCondition() {
            return condition;
        }

        public void setCondition(ExpNode cond) {
            this.condition = cond;
        }

        public StatementNode getThenStmt() {
            return thenStmt;
        }

        public StatementNode getElseStmt() {
            return elseStmt;
        }

        @Override
        public String toString(int level) {
            return "IF " + condition.toString() + " THEN" +
                    newLine(level + 1) + thenStmt.toString(level + 1) +
                    newLine(level) + "ELSE" +
                    newLine(level + 1) + elseStmt.toString(level + 1);
        }
    }

    /**
     * Tree node representing a "while" statement.
     */
    public static class WhileNode extends StatementNode {
        private ExpNode condition;
        private final StatementNode loopStmt;

        public WhileNode(Location loc, ExpNode condition,
                         StatementNode loopStmt) {
            super(loc);
            this.condition = condition;
            this.loopStmt = loopStmt;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitWhileNode(this);
        }

        public ExpNode getCondition() {
            return condition;
        }

        public void setCondition(ExpNode cond) {
            this.condition = cond;
        }

        public StatementNode getLoopStmt() {
            return loopStmt;
        }

        @Override
        public String toString(int level) {
            return "WHILE " + condition.toString() + " DO" +
                    newLine(level + 1) + loopStmt.toString(level + 1);
        }
    }

    /**
     * Tree node representing a "skip" statement.
     */
    public static class SkipNode extends StatementNode {
        public SkipNode(Location loc) {
            super(loc);
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitSkipNode(this);
        }

        @Override
        public String toString(int level) {
            return "skip";
        }
    }

    /**
     * Tree node representing a "multiAssign" statement.
     */
    public static class MultiAssignNode extends StatementNode {
        private ArrayList<AssignmentNode> assignments;

        public MultiAssignNode(Location loc, ArrayList assignments) {
            super(loc);
            this.assignments = assignments;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitMultiAssignNode(this);
        }

        public List<AssignmentNode> getAssignments() {
            return assignments;
        }

        @Override
        public String toString(int level) {
            return "Multiple Assignment";
        }

    }

    /**
     * Tree node representing a "Case" statement.
     */
    public static class CaseNode extends StatementNode {
        private ExpNode condition;
        private ArrayList<StatementNode> branches;
        private StatementNode defaultNode;
        private boolean defaultExists = false;
        private Location endLoc;

        /*
         * Case constructor in the case there is a default statement
         */
        public CaseNode(Location loc, Location endLoc, ExpNode condition,
                        ArrayList<StatementNode> branches,
                        StatementNode defaultNode) {
            super(loc);
            this.condition = condition;
            this.branches = branches;
            this.defaultNode = defaultNode;
            this.defaultExists = true;
            this.endLoc = endLoc;
        }

        /*
         * Case constructor in case there is no default statement
         */
        public CaseNode(Location loc, Location endLoc, ExpNode condition,
                        ArrayList<StatementNode> branches) {
            super(loc);
            this.condition = condition;
            this.branches = branches;
            this.endLoc = endLoc;
        }

        public Location getEndLoc() {return this.endLoc;}

        public ExpNode getConditionNode() {
            return this.condition;
        }

        public void setConditionNode(ExpNode newCondition) {
            this.condition = newCondition;
        }

        public boolean isDefaultExists() {
            return defaultExists;
        }

        public StatementNode getDefaultNode() {
            return defaultNode;
        }

        public ArrayList<StatementNode> getCaseBranches() {
            return branches;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitCaseNode(this);
        }

        @Override
        public String toString(int level) {
            return "Case Statement";
        }
    }

    /**
     * Tree node representing a "CaseBranch" statement.
     */
    public static class BranchNode extends StatementNode {
        private ConstExp amount;
        private StatementNode statementListNode;

        public BranchNode(Location loc, ConstExp amount,
                          StatementNode statementListNode) {
            super(loc);
            this.amount = amount;
            this.statementListNode = statementListNode;
        }

        public ConstExp getConstantNode() {
            return amount;
        }

        public StatementNode getStatementListNode() {
            return statementListNode;
        }

        @Override
        public void accept(StatementVisitor visitor) {
            visitor.visitBranchNode(this);
        }

        @Override
        public String toString(int level) {
            return "Case Branch";
        }

    }
}

