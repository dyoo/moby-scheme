package org.plt.world.config;
import org.plt.types.Callable;
public class OnLocationChange implements Config {
    public Callable c;

    public OnLocationChange(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}