package org.plt.world.config;
import org.plt.types.Callable;
public class OnTick implements Config {
    public Callable c;

    public OnTick(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}