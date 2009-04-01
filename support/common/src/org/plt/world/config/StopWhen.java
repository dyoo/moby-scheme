package org.plt.world.config;

public class StopWhen implements Config {
    Callable c;

    public StopWhen(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}