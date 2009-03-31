package org.plt.world;

import org.plt.WorldKernel;
import org.plt.Kernel;
import org.plt.gui.Picture;
import org.plt.types.Posn;
import org.plt.types.Number;
import org.plt.types.NumberTower;
import org.plt.types.Callable;
import org.plt.types.Logic;
import org.plt.types.Rational;

public class Bootstrap {

    private static class State {
	Posn target;
	Number player;
	Posn object;
	Number score;
	Number timer;

	public State(Posn target, Number player, Posn object, Number score, Number timer) {
	    this.target = target;
	    this.player = player;
	    this.object = object;
	    this.score = score;
	    this.timer = timer;
	}
    }

    private static Posn target1 = new Posn(new Rational(5), new Rational(235));
    private static Posn object1 = new Posn(new Rational(380), new Rational(-400));
    private static Number player1 = new Rational(320);
    private static State world1 = new State(target1, player1, object1, new Rational(0), new Rational(1));
    

    String titleString;
    Picture backgroundImage;
    Callable updateTargetX;
    Callable updateTargetY;
    Callable updatePlayer;
    Callable updateObject;
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
	this.backgroundImage = ((Picture) backgroundImage);
	this.backgroundImage.setPinhole(0, 0);
	this.updateTargetX = (Callable) updateTargetXCallable;
	this.updateTargetY = (Callable) updateTargetYCallable;
	this.updatePlayer = (Callable) updatePlayerCallable;
	this.updateObject = (Callable) updateObjectCallable;
	this.targetImage = (Picture) targetImage;
	this.playerImage = (Picture) playerImage;
	this.objectImage = (Picture) objectImage;
	this.isOffscreen = (Callable) offscreenCallable;
    }


    public Object start() {
	return WorldKernel.bigBang
	    (new Rational(320), 
	     new Rational(480),
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
					 new Rational(0));
		    } else if (isCollide(world)) {
			return new State(target1,
					 world.player,
					 updateMovement(world.object),
					 NumberTower.plus(world.score, new Rational(100)),
					 new Rational(151));
		    } else if (NumberTower.greaterThan(world.timer, new Rational(1))) {
			return new State(world.target,
					 world.player,
					 updateMovement(world.object),
					 world.score,
					 NumberTower.minus(world.timer, new Rational(15)));
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
	return new Callable() {
		public Object call(Object[] args) {
		    State world = (State) args[0];
		    Picture explosion = WorldKernel.circle(Kernel.add1(world.timer),
							   "solid",
							   (Kernel.even_question_(world.timer).isTrue() 
							    ? "red" : "orange"));
		    String scoreText = titleString + "     score: " + world.score;
		    Picture addTarget = NumberTower.greaterThan(world.timer, Rational.ONE) ? 
			WorldKernel.placeImage(explosion, 
					       world.object.getX(),
					       world.object.getY(),
					       backgroundImage)
			: WorldKernel.placeImage(targetImage,
						 world.target.getX(),
						 world.target.getY(),
						 backgroundImage);
		    Picture addObject = WorldKernel.placeImage(objectImage,
							       world.target.getX(),
							       world.target.getY(),
							       addTarget);
		    Picture addPlayer = WorldKernel.placeImage(playerImage,
							       world.player,
							       new Rational(410),
							       addObject);
		    return WorldKernel.placeImage(WorldKernel.text(scoreText, new Rational(20), "black"),
						  new Rational(10),
						  Rational.ZERO,
						  addPlayer);
		}
	    };
    }

    private Callable getKeypress() {
	return new Callable() {
		public Object call(Object[] args) {
		    State world = (State) args[0];
		    Object key = args[1];
		    if (Kernel.char_question_(key).isTrue())
			return world;
		    if (WorldKernel.isKeyEqual(key, "left").isTrue() ||
			WorldKernel.isKeyEqual(key, "right").isTrue())
			return new State(world.target,
					 (Number) updatePlayer.call(new Object[] {world.player, key}),
					 world.object,
					 world.score,
					 world.timer);
		    if (WorldKernel.isKeyEqual(key, "up").isTrue())
			return new State(world.target,
					 world.player,
					 fireObject(world),
					 world.score,
					 world.timer);
		    return world;
		}
	    };

    }

    private Posn fireObject(State world) {
	if (NumberTower.lessThan((Number) world.object.getY(), new Rational(100)))
	    return new Posn(world.player,
			    new Rational(400));
	return world.object;
    }


    private Number distance(Number x1, Number y1, Number x2, Number y2) {
	return Kernel.sqrt(NumberTower.plus(Kernel.sqr(NumberTower.minus(x1, x2)),
					    Kernel.sqr(NumberTower.minus(y1, y2))));
    }

    private boolean isCollide(State world) {
	return NumberTower.lessThan(distance((Number)world.target.getX(),
					     (Number)world.target.getY(),
					     (Number)world.object.getX(),
					     (Number)world.object.getY()),
				    new Rational(30));
    }

    private Posn updateTarget(State world) {
	return new Posn(updateTargetX.call(new Object[] { world.target.getX()}),
			updateTargetY.call(new Object[] { world.target.getY()}));
    }


    private Posn updateMovement(Posn object) {
	if (NumberTower.lessThan((Number)object.getY(),
				 new Rational(-1000)))
	    return object1;
	else
	    return new Posn(object.getX(),
			    updateObject.call(new Object[] {object.getY()}));
    }
}
