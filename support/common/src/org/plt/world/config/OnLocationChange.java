package org.plt.world.config;

public class OnLocationChange implements Config {
    public Callable c;

    public OnLocationChange(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}