package org.plt.world.config;

public interface Config {
    void accept(ConfigVisitor visitor);
}
