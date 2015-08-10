package edu.gemini.pot.sp.memImpl;

import edu.gemini.pot.sp.ISPProgramVisitor;
import edu.gemini.pot.sp.ISPSequence;
import edu.gemini.pot.sp.SPNodeKey;

public final class MemSequence extends MemProgramNodeBase implements ISPSequence {
    private final MemProgram program;

    public MemSequence(MemProgram prog, SPNodeKey key) {
        super(prog.getDocumentData(), key);
        program = prog;
    }

    public MemSequence(MemProgram prog, ISPSequence that, boolean preserveKeys) {
        super(prog.getDocumentData(), that, preserveKeys);
        program = prog;
    }

    public void accept(ISPProgramVisitor visitor) {
        visitor.visitSequence(this);
    }

    @Override public MemProgram getProgram() { return program; }
}
