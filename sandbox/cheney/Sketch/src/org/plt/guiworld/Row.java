package org.plt.guiworld;

public class Row implements Gui {

    private Gui[] items;


    public Row(Gui[] items) {
	this.items = items;
    }


    public Gui[] getItems() {
	return this.items;
    }


    public void accept(GuiVisitor v) {
	v.visit(this);
    }
}
