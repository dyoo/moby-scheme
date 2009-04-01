package org.plt.world.config;

public class OnRedraw implements Config {
    Callable c;

    public OnRedraw(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}