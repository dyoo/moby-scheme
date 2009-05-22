package org.plt.world.config;
import org.plt.types.Callable;
public class StopWhen implements Config {
    public Callable c;

    public StopWhen(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}