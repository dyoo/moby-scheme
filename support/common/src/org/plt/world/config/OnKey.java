package org.plt.world.config;

public class OnKey implements Config {
    public Callable c;

    public OnKey(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}