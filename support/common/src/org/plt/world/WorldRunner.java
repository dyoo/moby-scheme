package org.plt.world;

import org.plt.types.List;
import org.plt.types.Pair;
import org.plt.types.Empty;
import org.plt.Kernel;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class WorldRunner {
    private Object world;
    private long delay;
    private boolean stopped;

    private WorldTransformer onTick;
    private WorldJudge stopWhen;

    private List listeners; // listof WorldConsumer
    private BlockingQueue eventQueue; // queueof WorldTransformer

    public WorldRunner() {
	this.world = null;
	this.delay = 0;
	this.onTick = null;
	this.stopWhen = null;
	this.listeners = Empty.EMPTY;
	this.stopped = false;
	this.eventQueue = new LinkedBlockingQueue();
    }

    public boolean isStopped() {
	return this.stopped;
    }

    public void stop() {
	this.stopped = true;
	queueTransformer(new StopEvent());
    }

    public Object getWorld() {
	return this.world;
    }

    public void setWorld(Object world) {
	synchronized(world) {
	    this.world = world;
	}
	notifyWorldChange();
    }

    public long getDelay() {
	return this.delay;
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

    public void addListener(WorldConsumer aListener) {
	this.listeners = new Pair(aListener, listeners);
    }

    public void removeListener(WorldConsumer listener) {
	this.listeners = Kernel.remove(listener, listeners);
    }

    public void removeAllListeners() {
	this.listeners = Empty.EMPTY;
    }


    public Object bigBang() {

	final BlockingQueue lastWorldQueue = new LinkedBlockingQueue();

	stopped = false;
	startTimerEventThread();
	// Starts up the eventLoop and waits for events.
	Thread eventLoop = new Thread
	    (new Runnable() {
		    public void run() {
			if (stopWhen != null &&
			    stopWhen.judge(world)) {
			    stopped = true;
			}
			while(! stopped) {
			    try {
				WorldTransformer e = 
				    (WorldTransformer)
				    eventQueue.take();
				synchronized(world) {
				    world = e.transform(world);
				    // NOTE: e.transform
				    // may also change the stopped
				    // variable!
				    if (stopWhen != null &&
					stopWhen.judge(world)) {
					stopped = true;
				    }
				}
				notifyWorldChange();
			    } catch (InterruptedException e) {
				e.printStackTrace();
				stopped = true;
			    }
			}
			lastWorldQueue.offer(world);
		    }
		});
	eventLoop.start();
	while (true) {
	    try {
		Object lastWorld = lastWorldQueue.take();
		return lastWorld;
	    } catch (InterruptedException e) {
		e.printStackTrace();
		stopped = true;
		return world;
	    }
	}
    }

    // Starts up a thread that generates a TickEvent every
    // delay milliseconds.
    private void startTimerEventThread() {
	if (delay > 0) {
	    Thread timerThread = new Thread
		(new Runnable() {
			public void run() {
			    while (! stopped) {
				queueTransformer(new TickEvent());
				try {
				    Thread.sleep(delay);
				} catch (InterruptedException e) {
				    e.printStackTrace();
				    // do nothing.
				}
			    }
			}
		    });
	    timerThread.start();
	}
    }


    // Notify all listeners that the world has changed.
    private void notifyWorldChange() {
	List l = this.listeners;
	while (!l.isEmpty()) {
	    ((WorldConsumer) l.first()).consume(this.world);
	    l = l.rest();
	}
    }

    // Add a new WorldTransformer to the queue.
    public void queueTransformer(WorldTransformer t) {
	this.eventQueue.offer(t);
    }


    class TickEvent implements WorldTransformer {
	public Object transform(Object world) {
	    return onTick.transform(world);
	}

	public String toString() {
	    return "tick";
	}
    }
    
    // A proxy event that just makes sure the event loop shuts down
    // cleanly.
    class StopEvent implements WorldTransformer {
	public Object transform(Object world) {
	    stopped = true;
	    return world;
	}
	public String toString() {
	    return "stop";
	}
    }
}
