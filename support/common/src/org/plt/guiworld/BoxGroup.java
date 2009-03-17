package org.plt.guiworld;

import org.plt.world.WorldTransformer;

public class BoxGroup implements Gui {

	private WorldTransformer valF;

	private Gui gui;

	public BoxGroup(WorldTransformer valF, Gui gui) {
		this.valF = valF;
		this.gui = gui;
	}

	public WorldTransformer getValF() {
		return this.valF;
	}

	public Gui getGui() {
		return this.gui;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
