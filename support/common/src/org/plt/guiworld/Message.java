package org.plt.guiworld;

import org.plt.world.WorldTransformer;


public class Message implements Gui {

    public WorldTransformer valF;

    public void Message(WorldTransformer valF) {
	this.valF = valF;
    }

    public getValF() {
	return this.valF;
    }

    public void accept(GuiVisitor v) {
	v.visit(this);
    }
}
