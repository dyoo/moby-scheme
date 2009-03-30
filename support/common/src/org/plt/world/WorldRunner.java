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

    private BlockingQueue eventQueue;

    public World(Object initialWorld) {
	this.world = initialWorld;
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

    public void setStopped(boolean stopped) {
	this.stopped = stopped;
    }

    public Object getWorld() {
	return this.world;
    }

    public void setWorld(Object world) {
	this.world = world;
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
	stopped = false;
	startTimerEventThread();
	// Starts up the eventLoop and waits for events.
	Thread eventLoop = new Thread
	    (new Runnable() {
		    public void run() {
			while(! stopped) {
			    if (stopWhen != null &&
				stopWhen.judge(world)) {
				stopped = true;
				break;
			    }
			    try {
				Event e = 
				    (Event) eventQueue.take();
				synchronized(world) {
				    world = e.transform(world);
				}
				notifyWorldChange();
			    } catch (InterruptedException e) {
				stopped = true;
			    }
			}
		    }
		});
	eventLoop.start();
	try {
	    eventLoop.join();
	} catch (InterruptedException e) {
	    stopped = true;
	}
	return world;
    }


    private void startTimerEventThread() {
	if (delay > 0) {
	    Thread timerThread = new Thread
		(new Runnable() {
			public void run() {
			    while (! stopped) {
				eventQueue.offer(new TickEvent());
				try {
				    Thread.sleep(delay);
				} catch (InterruptedException e) {
				    // do nothing.
				}
			    }
			}
		    });
	    timerThread.start();
	}
    }


    private void notifyWorldChange() {
	List l = this.listeners;
	while (!l.isEmpty()) {
	    ((WorldConsumer) l.first()).consume(this.world);
	    l = l.rest();
	}
    }


    public void asyncTransform(WorldTransformer t) {
	synchronized(world) {
	    this.world = t.transform(world);
	}
	notifyWorldChange();
    }

    static private interface Event {
	Object transform(Object world);
    }


    class TickEvent implements Event {
	public Object transform(Object world) {
	    return onTick.transform(world);
	}
    }
    
//     class KeyEvent implements Event {
// 	public KeyEvent
// 	public Object transform(World e) {
	    
// 	}
//     }

}
