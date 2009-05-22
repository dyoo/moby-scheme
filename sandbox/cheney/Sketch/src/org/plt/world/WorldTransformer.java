package org.plt.world;

// Things that consume a world and produce some other object.
public interface WorldTransformer {
    Object transform(Object world);
}
