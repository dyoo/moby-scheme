package org.plt.world;

public class World {
    private Object world;
    private long delay;
    private boolean stopped;

    private WorldTransformer onTick;
    private WorldJudge stopWhen;
    private WorldConsumer changeListener;

    public World(Object initialWorld) {
	this.world = initialWorld;
	this.delay = 0;
	this.onTick = null;
	this.stopWhen = null;
	this.changeListener = null;
    }

    public void setDelay(long delay) {
	this.delay = delay;
    }

    public void setOnTick(WorldTransformer onTick) {
	this.onTick = onTick;
    }

    public void setStopWhen(WorldJudge stopWhen) {
	this.stopWhen = stopWhen;
    }

    public void setChangeListener(WorldConsumer changeListener) {
	this.changeListener = changeListener;
    }

    public void start() {
	new Thread
	    (new Runnable() {
		    public void run() {
			boolean stopped = false;
			while(! stopped) {
			    if (stopWhen != null &&
				stopWhen.judge(world)) {
				stopped = true;
			    } else {
				if (delay > 0) {
				    try {
					Thread.sleep(delay);
				    } catch (InterruptedException e) {}
				    synchronized(world) {
					if (onTick != null)
					    world = onTick.transform(world);
				    }
				    notifyWorldChange();
				}
			    }
			}
		    }
		}).start();
    }


    private void notifyWorldChange() {
	if (this.changeListener != null)
	    this.changeListener.consume(this.world);
    }
    

    public Object getWorld() {
	return this.world;
    }


    public void asyncTransform(WorldTransformer t) {
	synchronized(world) {
	    this.world = t.transform(world);
	}
	notifyWorldChange();
    }
}
