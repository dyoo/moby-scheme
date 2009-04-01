package org.plt.world.config;

public class OnTick implements Config {
    Callable c;

    public OnTick(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}