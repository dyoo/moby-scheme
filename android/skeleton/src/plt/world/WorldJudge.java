package plt.world;

// Things that consume a world and say yes or no
public interface WorldJudge {
    boolean judge(Object world);
}
