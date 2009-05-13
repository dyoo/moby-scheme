package org.plt.world.config;
import org.plt.types.Callable;
public class OnMouse implements Config {
    public Callable c;

    public OnMouse(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}