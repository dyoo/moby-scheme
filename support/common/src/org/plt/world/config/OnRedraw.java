package org.plt.world.config;
import org.plt.types.Callable;
public class OnRedraw implements Config {
    public Callable c;

    public OnRedraw(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}