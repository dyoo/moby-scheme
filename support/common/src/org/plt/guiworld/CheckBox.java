package org.plt.guiworld;

import org.plt.world.*;

public class CheckBox implements Gui {

	private WorldTransformer labelValF;
	private WorldTransformer checkValF;
	private WorldAndObjectTransformer callback;

	public CheckBox(WorldTransformer labelValF, WorldTransformer checkValF,
			WorldAndObjectTransformer callback) {
		this.labelValF = labelValF;
		this.checkValF = checkValF;
		this.callback = callback;
	}

	public WorldTransformer getLabelValF() {
		return this.labelValF;
	}

	public WorldTransformer getCheckValF() {
		return this.checkValF;
	}

	public WorldAndObjectTransformer getCallback() {
		return this.callback;
	}

	public void accept(GuiVisitor v) {
		v.visit(this);
	}
}
