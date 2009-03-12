package org.plt.guiworld;

public class Slider implements Gui {
	private Integer cur;
	private Integer min;
	private Integer max;
	private WorldTransformer valF;
	private WorldAndObjectTransformer callback;

	public Slider(Integer min, Integer max, WorldTransformer valF,
			WorldAndObjectTransformer callback) {
		this.min = min;
		this.max = max;
		this.valF = valF;
		this.callback = callback;
	}

	public Integer getMin() {
		return this.min;
	}

	public Integer getMax() {
		return this.max;
	}

	public Integer getCur() {
		return this.cur;
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
