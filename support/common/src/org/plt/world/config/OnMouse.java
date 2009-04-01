package org.plt.world.config;

public class OnMouse implements Config {
    Callable c;

    public OnMouse(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}