package org.plt.world;

// Things that consume a world and produce some other object.
public interface WorldAndObjectTransformer {
	Object transform(Object world, Object obj);
}
