package org.plt.guiworld;

import org.plt.world.*;

public class DropDown implements Gui {
	private WorldTransformer choicesF;
	private WorldAndObjectTransformer callback;

	public DropDown(WorldTransformer choicesF,
			WorldAndObjectTransformer callback) {
		this.choicesF = choicesF;
		this.callback = callback;
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
