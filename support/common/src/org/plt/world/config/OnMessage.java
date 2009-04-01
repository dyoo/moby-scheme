package org.plt.world.config;

public class OnMessage implements Config {
    Callable c;

    public OnMessage(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}