package org.plt.Sketch;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

import org.plt.platform.PlatformI;
import org.plt.platform.Platform;

import org.plt.world.WorldRunner;
import org.plt.world.WorldConsumer;
import org.plt.world.WorldTransformer;
import org.plt.WorldKernel;

import org.plt.world.MessageListener;
import org.plt.world.LocationChangeListener;
import org.plt.world.OrientationChangeListener;
import org.plt.world.AccelerationChangeListener;

import org.plt.types.Callable;



// This is stub code.
// The following things must be plugged in:
// 
// PROGRAM-NAME
// PROGRAM-DEFINITIONS
// PROGRAM-TOPLEVEL-EXPRESSIONS
// ON-START
// ON-PAUSE
// ON-DESTROY


public class Sketch extends MIDlet 
    implements WorldConsumer,
	       MessageListener, 
	       LocationChangeListener, 
	       OrientationChangeListener,
	       AccelerationChangeListener  {
    static class UserProgram {
        // Important: definitions must come first, because they'll
        // include static blocks that initialize values that
        // we depend on later!
        
// Module require erased

Object WIDTH; 
Object HEIGHT; 
Object BLANK_dash_COLOR; 
Object DRAW_dash_COLOR; 
Object DOT_dash_RADIUS; 
public class world implements org.plt.types.Struct { public Object posn;
public Object dots;
public Object direction; 
 public world(Object posn,Object dots,Object direction) { this.posn = posn;
this.dots = dots;
this.direction = direction; }
 public boolean equals(Object other) {
                     if (other instanceof world) {
                       return (((((org.plt.types.Logic)(org.plt.Kernel.equal_question_((world_dash_direction(this)),(world_dash_direction(other))))).isTrue())&&(((org.plt.types.Logic)(((((org.plt.types.Logic)(org.plt.Kernel.equal_question_((world_dash_dots(this)),(world_dash_dots(other))))).isTrue())&&(((org.plt.types.Logic)(((((org.plt.types.Logic)(org.plt.Kernel.equal_question_((world_dash_posn(this)),(world_dash_posn(other))))).isTrue())&&(((org.plt.types.Logic)org.plt.types.Logic.TRUE).isTrue())) ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE)).isTrue())) ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE)).isTrue())) ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE).isTrue();
                     } else {
                       return false;
                     }
                   } 
}
public Object make_dash_world(Object id0,Object id1,Object id2) { return new world(id0,id1,id2); } 
 public Object world_dash_posn(Object obj) { return ((world)obj).posn; }
public Object world_dash_dots(Object obj) { return ((world)obj).dots; }
public Object world_dash_direction(Object obj) { return ((world)obj).direction; }
 public org.plt.types.Logic world_question_(Object obj) { return obj instanceof world ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE; }
public Object update_dash_world_dash_posn(Object a_dash_world, Object posn) { return (make_dash_world(posn,(world_dash_dots(a_dash_world)),(world_dash_direction(a_dash_world)))); }
public Object update_dash_world_dash_dots(Object a_dash_world, Object dots) { return (make_dash_world((world_dash_posn(a_dash_world)),dots,(world_dash_direction(a_dash_world)))); }
public Object update_dash_world_dash_direction(Object a_dash_world, Object a_dash_direction) { return (make_dash_world((world_dash_posn(a_dash_world)),(world_dash_dots(a_dash_world)),a_dash_direction)); }
public Object update_dash_posn_dash_x(Object a_dash_posn, Object x) { return (org.plt.Kernel.make_dash_posn(x,(org.plt.Kernel.posn_dash_y(a_dash_posn)))); }
public Object update_dash_posn_dash_y(Object a_dash_posn, Object y) { return (org.plt.Kernel.make_dash_posn((org.plt.Kernel.posn_dash_x(a_dash_posn)),y)); }
Object initial_dash_world; 
public Object draw_dash_world_dash_posn(Object a_dash_posn, Object a_dash_scene) { return (org.plt.WorldKernel.placeImage((org.plt.WorldKernel.nwRectangle((new org.plt.types.Rational(1, 1)),(new org.plt.types.Rational(3, 1)),(new String("solid")),(new String("black")))),(org.plt.Kernel.posn_dash_x(a_dash_posn)),(org.plt.Kernel.posn_dash_y(a_dash_posn)),a_dash_scene)); }
public Object add_dash_posn_dash_to_dash_dots(Object a_dash_world) { return (update_dash_world_dash_dots(a_dash_world,(org.plt.Kernel.cons((world_dash_posn(a_dash_world)),(world_dash_dots(a_dash_world)))))); }
public Object move_dash_left(Object a_dash_world) { return (add_dash_posn_dash_to_dash_dots((update_dash_world_dash_posn(a_dash_world,(update_dash_posn_dash_x((world_dash_posn(a_dash_world)),org.plt.Kernel.max((new org.plt.types.Rational(0, 1)), new Object[] {org.plt.Kernel._dash_((org.plt.Kernel.posn_dash_x((world_dash_posn(a_dash_world)))), new Object[] {DOT_dash_RADIUS})}))))))); }
public Object move_dash_right(Object a_dash_world) { return (add_dash_posn_dash_to_dash_dots((update_dash_world_dash_posn(a_dash_world,(update_dash_posn_dash_x((world_dash_posn(a_dash_world)),org.plt.Kernel.min((org.plt.Kernel.sub1(WIDTH)), new Object[] {org.plt.Kernel._plus_(new Object[] {(org.plt.Kernel.posn_dash_x((world_dash_posn(a_dash_world)))),DOT_dash_RADIUS})}))))))); }
public Object move_dash_up(Object a_dash_world) { return (add_dash_posn_dash_to_dash_dots((update_dash_world_dash_posn(a_dash_world,(update_dash_posn_dash_y((world_dash_posn(a_dash_world)),org.plt.Kernel.max((new org.plt.types.Rational(0, 1)), new Object[] {org.plt.Kernel._dash_((org.plt.Kernel.posn_dash_y((world_dash_posn(a_dash_world)))), new Object[] {DOT_dash_RADIUS})}))))))); }
public Object move_dash_down(Object a_dash_world) { return (add_dash_posn_dash_to_dash_dots((update_dash_world_dash_posn(a_dash_world,(update_dash_posn_dash_y((world_dash_posn(a_dash_world)),org.plt.Kernel.min((org.plt.Kernel.sub1(HEIGHT)), new Object[] {org.plt.Kernel._plus_(new Object[] {(org.plt.Kernel.posn_dash_y((world_dash_posn(a_dash_world)))),DOT_dash_RADIUS})}))))))); }
public Object move_dash_by_dash_drifting(Object a_dash_world) { return (((org.plt.types.Logic)(org.plt.Kernel.string_equal__question_((world_dash_direction(a_dash_world)),(new String("stable")), new Object[] {}))).isTrue() ? (a_dash_world) : ((((org.plt.types.Logic)(org.plt.Kernel.string_equal__question_((world_dash_direction(a_dash_world)),(new String("left")), new Object[] {}))).isTrue() ? ((move_dash_left(a_dash_world))) : ((((org.plt.types.Logic)(org.plt.Kernel.string_equal__question_((world_dash_direction(a_dash_world)),(new String("right")), new Object[] {}))).isTrue() ? ((move_dash_right(a_dash_world))) : ((((org.plt.types.Logic)(org.plt.Kernel.string_equal__question_((world_dash_direction(a_dash_world)),(new String("up")), new Object[] {}))).isTrue() ? ((move_dash_up(a_dash_world))) : ((((org.plt.types.Logic)(org.plt.Kernel.string_equal__question_((world_dash_direction(a_dash_world)),(new String("down")), new Object[] {}))).isTrue() ? ((move_dash_down(a_dash_world))) : 
                      org.plt.Kernel.error(org.plt.types.Symbol.makeInstance("cond"), "Fell out of cond")))))))))); }




	public Object render_dash_etch_dash_a_dash_sketch(final Object a_dash_world) { 
	    final Object backgroundScene = 
		(org.plt.WorldKernel.placeImage((org.plt.WorldKernel.nwRectangle(WIDTH,HEIGHT,(new String("solid")),
										 BLANK_dash_COLOR)),
						(new org.plt.types.Rational(0, 1)),
						(new org.plt.types.Rational(0, 1)),
						(org.plt.WorldKernel.emptyScene(WIDTH,HEIGHT))));
	    Object dotsOnBackground;
	    dotsOnBackground = trampoline(new Callable() { 
		    public Object call(Object[] args) {
			return draw_dash_dots((world_dash_dots(a_dash_world)),
					      backgroundScene);
		    }
		});
					  
					  
	    return (draw_dash_world_dash_posn((world_dash_posn(a_dash_world)),
					      dotsOnBackground));
	    
	}
	int stackDepth = 0;

	public Object trampoline(Callable initialC) {
	    Callable c = initialC;
	    while (true) {
		try {
		    return c.call(new Object[]{});
		} catch (Bounce e) {
		    c = e.c;
		    stackDepth = 0;
		}
	    }
	}

	public class Bounce extends RuntimeException {
	    Callable c;
	    public Bounce(Callable c) {
		super();
		this.c = c;
	    }
	}


	public Object draw_dash_dots(Object dots, Object a_dash_scene) {
	    if(org.plt.Kernel.empty_question_(dots).isTrue()) {
		return a_dash_scene;
	    } else {
		return 
		    ((draw_dash_dots((org.plt.Kernel.rest(dots)),
				     (org.plt.WorldKernel.placeImage
				      ((org.plt.WorldKernel.circle(DOT_dash_RADIUS,
								   (new String("solid")),DRAW_dash_COLOR)),
				       (org.plt.Kernel.posn_dash_x((org.plt.Kernel.first(dots)))),
				       (org.plt.Kernel.posn_dash_y((org.plt.Kernel.first(dots)))),
				       a_dash_scene)))));
	    }
	}





public Object change_dash_direction(Object w, Object k) { return (((org.plt.types.Logic)((org.plt.WorldKernel.isKeyEqual(k,(org.plt.types.Symbol.makeInstance("left")))))).isTrue() ? ((update_dash_world_dash_direction(w,(new String("left"))))) : ((((org.plt.types.Logic)((org.plt.WorldKernel.isKeyEqual(k,(org.plt.types.Symbol.makeInstance("right")))))).isTrue() ? ((update_dash_world_dash_direction(w,(new String("right"))))) : ((((org.plt.types.Logic)((org.plt.WorldKernel.isKeyEqual(k,(org.plt.types.Symbol.makeInstance("up")))))).isTrue() ? ((update_dash_world_dash_direction(w,(new String("up"))))) : ((((org.plt.types.Logic)((org.plt.WorldKernel.isKeyEqual(k,(org.plt.types.Symbol.makeInstance("down")))))).isTrue() ? ((update_dash_world_dash_direction(w,(new String("down"))))) : (w)))))))); }
        public void runToplevel() {
	    
WIDTH = (new org.plt.types.Rational(320, 1));
HEIGHT = (new org.plt.types.Rational(480, 1));
BLANK_dash_COLOR = (new String("lightgray"));
DRAW_dash_COLOR = (new String("darkgray"));
DOT_dash_RADIUS = (new org.plt.types.Rational(3, 1));






initial_dash_world = (make_dash_world((org.plt.Kernel.make_dash_posn(org.plt.Kernel._slash_(WIDTH, new Object[] {(new org.plt.types.Rational(2, 1))}),org.plt.Kernel._slash_(HEIGHT, new Object[] {(new org.plt.types.Rational(2, 1))}))),org.plt.types.Empty.EMPTY,(new String("stable"))));










org.plt.Kernel.identity(org.plt.WorldKernel.bigBang(WIDTH,HEIGHT,initial_dash_world, new Object[] {(org.plt.world.config.Kernel.onRedraw((new org.plt.types.Callable() {
                   public Object call(Object[] args) {
                       return render_dash_etch_dash_a_dash_sketch(args[0]);
                   }
                 }))),(org.plt.world.config.Kernel.onTick((new org.plt.types.Rational(1, 20)),(new org.plt.types.Callable() {
                   public Object call(Object[] args) {
                       return move_dash_by_dash_drifting(args[0]);
                   }
                 }))),(org.plt.world.config.Kernel.onKey((new org.plt.types.Callable() {
                   public Object call(Object[] args) {
                       return change_dash_direction(args[0], args[1]);
                   }
                 })))}));;
	}
    }

    MyCanvas canvas;
    WorldRunner runner;


    // When a program is paused, we'll restart the world.
    public void pauseApp() {
	this.runner.stop();
	// IMPORTANT: The injected code here assumes the
	// presense of a 'listener' variable that
	// can be used to register callbacks.
	Sketch listener = this;
	
    }


    public void destroyApp(boolean unconditional) {
	this.runner.stop();
	// IMPORTANT: The injected code here assumes the
	// presense of a 'listener' variable that
	// can be used to register callbacks.
	Sketch listener = this;
	
    }	


    public void startApp() {
	this.runner = new WorldRunner();
	this.runner.addListener(this);
	WorldKernel.setRunner(this.runner);
	this.canvas = new MyCanvas();
	// IMPORTANT: The injected code here assumes the
	// presence of a 'listener' variable that
	// can be used to register callbacks.
	Sketch listener = this;
	
	Display.getDisplay(this).setCurrent(canvas);


	new Thread() {
	    public void run() {
		(new UserProgram()).runToplevel();
	    }
	}.start();
    }


    // WorldConsumer: we get the new world and show it!
    public void consume(Object newWorld) {
	if (this.canvas != null)
	    this.canvas.repaint();
    }


    public void onMessage(final Object aMessage) {
	this.runner.queueTransformer(new WorldTransformer() {
	    public Object transform(Object world) {
		return WorldKernel.getOnMessageEventHandler().call
		    (new Object[] { world, aMessage});
	    }
	    });
    }

    public void onLocationChange(final Object latitude,
				 final Object longitude) {
	this.runner.queueTransformer(new WorldTransformer() {
	    public Object transform(Object world) {
		return WorldKernel.getOnLocationChangeEventHandler().call
		    (new Object[] { world, latitude, longitude });
	    }
	    });
    }

    public void onOrientationChange(final Object azimuth, 
				    final Object pitch,
				    final Object roll) {
	this.runner.queueTransformer(new WorldTransformer() {
	    public Object transform(Object world) {
		return WorldKernel.getOnOrientationChangeEventHandler().call
		    (new Object[] { world, azimuth, pitch, roll });
	    }
	    });
    }


    public void onAccelerationChange(final Object x,
				     final Object y,
				     final Object z) {
	this.runner.queueTransformer(new WorldTransformer() {
	    public Object transform(Object world) {
		return WorldKernel.getOnAccelerationChangeEventHandler()
		    .call(new Object[] { world, x, y, z });
	    }
	    });
    }





    class MyCanvas extends Canvas {

	public void paint(Graphics g) {
	    if (runner.getWorld() == null)
		return;

            org.plt.gui.GraphicsAdapter ag = 
		new org.plt.gui.GraphicsAdapter(g);
	    // #4,5,6 on-redraw expression
	    org.plt.gui.DrawPicture pVisitor = 
		new org.plt.gui.DrawPicture(ag);
	    org.plt.gui.Scene aScene =
		(org.plt.gui.Scene) WorldKernel.getOnRedrawHandler().call
		(new Object[] {runner.getWorld()});
	    aScene.accept(pVisitor, 0, 0);
	}


	protected void keyPressed(int keyCode) {
	    final Object aKey = translateKeyCode(keyCode);
	    runner.queueTransformer(new WorldTransformer() {
		    public Object transform(Object world) {
			return WorldKernel.getOnKeyEventHandler().call
			    (new Object[] {world, aKey});
		    }
		});
	}

	private Object translateKeyCode(int keyCode) {
	    switch(getGameAction(keyCode)) {
	    case Canvas.LEFT: return "left";
	    case Canvas.RIGHT: return "right"; 
	    case Canvas.UP: return "up";
	    case Canvas.DOWN: return "down";
	    default:
		if (keyCode > 0) {
		    return new Character((char)keyCode);
		} else {
		    return "";
		}
	    }
	}
    }
}
