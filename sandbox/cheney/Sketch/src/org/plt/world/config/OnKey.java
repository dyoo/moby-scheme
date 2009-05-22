package org.plt.world.config;
import org.plt.types.Callable;
public class OnKey implements Config {
    public Callable c;

    public OnKey(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}