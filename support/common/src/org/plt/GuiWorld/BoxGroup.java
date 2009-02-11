package org.plt.GuiWorld;

public class BoxGroup implements Gui {
    // Fill me in


    public void accept(GuiVisitor v) {
	v.visit(this);
    }
}