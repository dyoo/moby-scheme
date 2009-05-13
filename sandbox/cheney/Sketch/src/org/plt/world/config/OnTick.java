package org.plt.world.config;

import org.plt.types.Callable;
import org.plt.types.Number;

public class OnTick implements Config {
    public Number delay;
    public Callable c;

    public OnTick(Number delay, Callable c) {
	this.delay = delay;
	this.c = c;
    }

    public void accept(ConfigVisitor visitor) {
	visitor.visit(this);
    }
}