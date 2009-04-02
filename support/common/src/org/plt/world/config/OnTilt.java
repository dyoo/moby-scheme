package org.plt.world.config;
import org.plt.types.Callable;
public class OnTilt implements Config {
    public Callable c;

    public OnTilt(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}