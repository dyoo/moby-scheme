package org.plt.guiworld;

import org.plt.world.WorldTransformer;

public class CheckBox implements Gui {

	private WorldTransformer valF;
	private WorldAndObjectTransformer callback;

	public CheckBox(WorldTransformer valF) {
		this.valF = valF;
	}

	public WorldTransformer getValF() {
		return this.valF;
	}

	public WorldAndObjectTransformer getCallback() {
		return this.callback;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
