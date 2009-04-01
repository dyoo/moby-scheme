package org.plt.world.config;

public class OnLocationChange implements Config {
    Callable c;

    public OnLocationChange(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}