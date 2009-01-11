package org.plt.world;

// Things that consume a world and don't return anything.
public interface WorldConsumer {
    void consume(Object world);
}
