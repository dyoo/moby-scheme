package org.plt.world.config;
import org.plt.types.Callable;
public class OnMessage implements Config {
    public Callable c;

    public OnMessage(Callable c) {
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}