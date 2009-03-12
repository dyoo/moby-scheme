package org.plt.guiworld;

import org.plt.world.*;

public class CheckBox implements Gui {

	private WorldTransformer valF;
	private WorldAndObjectTransformer callback;

	public CheckBox(WorldTransformer valF, WorldAndObjectTransformer callback) {
		this.valF = valF;
		this.callback = callback;
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
