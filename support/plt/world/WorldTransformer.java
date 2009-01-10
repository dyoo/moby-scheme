package plt.world;

// Things that consume a world and produce either another world or a scene.
public interface WorldTransformer {
    Object transform(Object world);
}
