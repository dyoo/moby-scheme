package org.plt.guiworld;

import org.plt.world.*;

public class DropDown implements Gui {
	private Object[] items;
	private WorldTransformer valF;
	private WorldAndObjectTransformer callback;

	public DropDown(Object[] items, WorldTransformer valF,
			WorldAndObjectTransformer callback) {
		this.items = items;
		this.valF = valF;
		this.callback = callback;
	}

	public Object[] getItems() {
		return this.items;
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
