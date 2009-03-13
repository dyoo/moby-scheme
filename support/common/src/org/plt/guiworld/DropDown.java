package org.plt.guiworld;

import org.plt.world.*;

public class DropDown implements Gui {
	private WorldTransformer valF;
	private WorldTransformer choicesF;
	private WorldAndObjectTransformer callback;

	public DropDown(WorldTransformer valF, WorldTransformer choicesF,
			WorldAndObjectTransformer callback) {
		this.valF = valF;
		this.choicesF = choicesF;
		this.callback = callback;
	}

	public WorldTransformer getValF() {
		return this.valF;
	}

	public WorldTransformer getChoicesF() {
		return this.choicesF;
	}

	public WorldAndObjectTransformer getCallback() {
		return this.callback;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
