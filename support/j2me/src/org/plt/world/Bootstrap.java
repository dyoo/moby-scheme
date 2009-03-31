package org.plt.world;

import org.plt.WorldKernel;
import org.plt.gui.Picture;
import org.plt.types.Posn;
import org.plt.types.Callable;
import org.plt.types.Logic;
import org.plt.types.Rational;

public class Bootstrap {

    private static class State {
	Posn target;
	int player;
	Posn object;
	int score;
	int timer;

	public State(Posn target, int player, Posn object, int score, int timer) {
	    this.target = target;
	    this.player = player;
	    this.object = object;
	    this.score = score;
	    this.timer = timer;
	}
    }

    private static Posn target1 = new Posn(5, 235);
    private static Posn object1 = new Posn(380, -400);
    private static int player1 = 320;
    private static State world1 = new State(target1, player1, object1, 0, 1);
    


    String titleString;
    Picture backgroundImage;
    Callable updateTargetX;
    Callable updateTargetY;
    Callable updatePlayer;
    Picture targetImage;
    Picture playerImage;
    Picture objectImage;
    Callable isOffscreen;

    public Bootstrap(Object titleString,
		     Object backgroundImage,
		     Object updateTargetXCallable,
		     Object updateTargetYCallable,
		     Object updatePlayerCallable,
		     Object updateObjectCallable,
		     Object targetImage,
		     Object playerImage,
		     Object objectImage,
		     Object offscreenCallable) {
	this.titleString = (String) titleString;
	this.backgroundImage = (Picture) backgroundImage;
	this.updateTargetX = (Callable) updateTargetXCallable;
	this.updateTargetY = (Callable) updateTargetYCallable;
	this.updatePlayer = (Callable) updatePlayerCallable;
	this.targetImage = (Picture) targetImage;
	this.playerImage = (Picture) playerImage;
	this.objectImage = (Picture) objectImage;
	this.isOffscreen = (Callable) offscreenCallable;
    }


    public Object start() {
	return WorldKernel.bigBang
	    (((Picture)backgroundImage).getWidth(),
	     ((Picture)backgroundImage).getHeight(),
	     getDelay(),
	     world1,
	     new Object[] {
		 WorldKernel.onTick(getUpdateWorld()),
		 WorldKernel.onRedraw(getDrawWorld()),
		 WorldKernel.onKey(getKeypress())
	     });
    }



    private Rational getDelay() {
	return new Rational(1, 10);
    }


    private Callable getUpdateWorld() {
	return new Callable() {
		public Object call(Object[] args) {
		    State world = (State) args[0];
		    if (((Logic) isOffscreen.call
			 (new Object[] { world.target.getX() }))
			.isTrue()) {
			return new State(target1,
					 world.player,
					 updateMovement(world.object),
					 world.score,
					 0);
		    } else if (isCollide(world)) {
			return new State(target1,
					 world.player,
					 updateMovement(world.object),
					 world.score + 100,
					 151);
		    } else if (world.timer > 1) {
			return new State(world.target,
					 world.player,
					 updateMovement(world.object),
					 world.score,
					 world.timer - 15);
		    } else {
			return new State(updateTarget(world),
					 world.player,
					 updateMovement(world.object),
					 world.score,
					 world.timer);
		    }
		}
	    };
    }


    private Callable getDrawWorld() {
	return null;
    }

    private Callable getKeypress() {
	return null;
    }


    private boolean isCollide(State world) {
	return false;
    }

    private Posn updateTarget(State world) {
	return null;
    }

    private Posn updateMovement(Posn object) {
	return null;
    }
}
