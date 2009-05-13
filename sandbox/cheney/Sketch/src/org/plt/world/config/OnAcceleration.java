package org.plt.world.config;
import org.plt.types.Callable;

public class OnAcceleration implements Config {
    public Callable c;

    public OnAcceleration(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}