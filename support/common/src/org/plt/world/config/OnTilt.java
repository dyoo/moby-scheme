package org.plt.world.config;

public class OnTilt implements Config {
    Callable c;

    public OnTilt(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}