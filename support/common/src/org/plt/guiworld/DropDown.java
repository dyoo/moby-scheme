package org.plt.guiworld;

import org.plt.world.*;

public class DropDown implements Gui {
	private WorldTransformer valF;
	private WorldAndObjectTransformer callback;

	public DropDown(WorldTransformer valF, WorldAndObjectTransformer callback) {
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
