package org.plt.guiworld;

import org.plt.world.WorldTransformer;

public class DropDown implements Gui {
	private WorldTransformer valF;

	public DropDown(WorldTransformer valF) {
		this.valF = valF;
	}

	public WorldTransformer getValF() {
		return this.valF;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
