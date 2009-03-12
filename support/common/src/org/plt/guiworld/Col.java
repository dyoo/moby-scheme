package org.plt.guiworld;

public class Col implements Gui {

	private Gui[] items;

	public Col(Gui[] items) {
		this.items = items;
	}

	public Gui[] getItems() {
		return this.items;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
