package org.plt;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

// This is stub code.
// The following things must be plugged in:
// 
// PROGRAM-NAME
// PROGRAM-DEFINITIONS
// INITIAL-WORLD-EXPRESSION
// DELAY-EXPRESSION
// STOP-WHEN-EXPRESSION
// ON-TICK-EXPPRESSION
// ON-REDRAW-EXPRESSION
// ON-KEY-EVENT-EXPRESSION


public class Cowabunga extends MIDlet {
    static Object WIDTH; static { WIDTH = (new plt.types.Rational(100, 1)); }
static Object HEIGHT; static { HEIGHT = (new plt.types.Rational(200, 1)); }
static Object UFO; static { UFO = (plt.Kernel._dash_kernel_dash_create_dash_image(("/image-1.png"))); }
static Object RED_dash_UFO; static { RED_dash_UFO = (plt.Kernel._dash_kernel_dash_create_dash_image(("/image-2.png"))); }
static Object COW_dash_LEFT; static { COW_dash_LEFT = (plt.Kernel._dash_kernel_dash_create_dash_image(("/image-3.png"))); }
static Object COW_dash_RIGHT; static { COW_dash_RIGHT = (plt.Kernel._dash_kernel_dash_create_dash_image(("/image-4.png"))); }
static Object GROUND_dash_HEIGHT; static { GROUND_dash_HEIGHT = (new plt.types.Rational(15, 1)); }
static Object COW_dash_Y; static { COW_dash_Y = (plt.Kernel._dash_((plt.Kernel._dash_(HEIGHT,(plt.Kernel._slash_((plt.Kernel.image_dash_height(COW_dash_LEFT)),(new plt.types.Rational(2, 1)))))),GROUND_dash_HEIGHT)); }
static Object EMPTY_dash_SCENE; static { EMPTY_dash_SCENE = (plt.Kernel.place_dash_image((plt.Kernel.nw_colon_rectangle(WIDTH,GROUND_dash_HEIGHT,("solid"),("brown"))),(new plt.types.Rational(0, 1)),(plt.Kernel._dash_(HEIGHT,GROUND_dash_HEIGHT)),(plt.Kernel.place_dash_image((plt.Kernel.nw_colon_rectangle(WIDTH,HEIGHT,("solid"),("white"))),(new plt.types.Rational(0, 1)),(new plt.types.Rational(0, 1)),(plt.Kernel.empty_dash_scene(WIDTH,HEIGHT)))))); }
static public class cow implements plt.types.Struct { public Object x;
public Object direction; 
 public cow(Object x,Object direction) { this.x = x;
this.direction = direction; }
 public boolean equals(Object other) {
                     if (other instanceof cow) {
                       return (((((plt.types.Logic)(plt.Kernel.equal_question_((cow_dash_direction(this)),(cow_dash_direction(other))))).isTrue())&&(((plt.types.Logic)(((((plt.types.Logic)(plt.Kernel.equal_question_((cow_dash_x(this)),(cow_dash_x(other))))).isTrue())&&(((plt.types.Logic)(plt.types.Logic.TRUE)).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE).isTrue();
                     } else {
                       return false;
                     }
                   } 
}
static public Object make_dash_cow(Object id0,Object id1) { return new cow(id0,id1); } 
 static public Object cow_dash_x(Object obj) { return ((cow)obj).x; }
static public Object cow_dash_direction(Object obj) { return ((cow)obj).direction; }
static public class world implements plt.types.Struct { public Object ufo;
public Object cows; 
 public world(Object ufo,Object cows) { this.ufo = ufo;
this.cows = cows; }
 public boolean equals(Object other) {
                     if (other instanceof world) {
                       return (((((plt.types.Logic)(plt.Kernel.equal_question_((world_dash_cows(this)),(world_dash_cows(other))))).isTrue())&&(((plt.types.Logic)(((((plt.types.Logic)(plt.Kernel.equal_question_((world_dash_ufo(this)),(world_dash_ufo(other))))).isTrue())&&(((plt.types.Logic)(plt.types.Logic.TRUE)).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE).isTrue();
                     } else {
                       return false;
                     }
                   } 
}
static public Object make_dash_world(Object id0,Object id1) { return new world(id0,id1); } 
 static public Object world_dash_ufo(Object obj) { return ((world)obj).ufo; }
static public Object world_dash_cows(Object obj) { return ((world)obj).cows; }
static public Object move_dash_cow(Object a_dash_cow) { return (((plt.types.Logic)((plt.Kernel.string_equal__question_((cow_dash_direction(a_dash_cow)),("left"))))).isTrue() ? ((make_dash_cow((plt.Kernel._dash_((cow_dash_x(a_dash_cow)),(new plt.types.Rational(1, 1)))),("left")))) : ((((plt.types.Logic)((plt.Kernel.string_equal__question_((cow_dash_direction(a_dash_cow)),("right"))))).isTrue() ? ((make_dash_cow((plt.Kernel._plus_((cow_dash_x(a_dash_cow)),(new plt.types.Rational(1, 1)))),("right")))) : plt.Kernel.error(plt.types.Symbol.makeInstance("cond"), "Fell out of cond")))); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object cow_dash_distance(Object cow_dash_1, Object cow_dash_2) { return (plt.Kernel.abs((plt.Kernel._dash_((cow_dash_x(cow_dash_1)),(cow_dash_x(cow_dash_2)))))); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object cow_dash_collides_dash_cow_question_(Object cow_dash_1, Object cow_dash_2) { return (((((plt.types.Logic)(plt.Kernel._lessthan_((cow_dash_distance((move_dash_cow(cow_dash_1)),(move_dash_cow(cow_dash_2)))),(plt.Kernel.image_dash_width(COW_dash_LEFT))))).isTrue())&&(((plt.types.Logic)(plt.Kernel._lessthan_((cow_dash_distance((move_dash_cow(cow_dash_1)),(move_dash_cow(cow_dash_2)))),(cow_dash_distance(cow_dash_1,cow_dash_2))))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object cow_dash_collides_dash_any_dash_cow_question_(Object a_dash_cow, Object cows) { return (((plt.types.Logic)((plt.Kernel.empty_question_(cows)))).isTrue() ? ((plt.types.Logic.FALSE)) : ((((((plt.types.Logic)(cow_dash_collides_dash_cow_question_(a_dash_cow,(plt.Kernel.first(cows))))).isTrue())||(((plt.types.Logic)(cow_dash_collides_dash_any_dash_cow_question_(a_dash_cow,(plt.Kernel.rest(cows))))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE))); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object cow_dash_collides_dash_with_dash_edge_question_(Object a_dash_cow) { return (((((plt.types.Logic)(((((plt.types.Logic)(plt.Kernel._lessthan__equal_((cow_dash_x(a_dash_cow)),(new plt.types.Rational(0, 1))))).isTrue())&&(((plt.types.Logic)(plt.Kernel.string_equal__question_((cow_dash_direction(a_dash_cow)),("left")))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)).isTrue())||(((plt.types.Logic)(((((plt.types.Logic)(plt.Kernel._greaterthan__equal_((cow_dash_x(a_dash_cow)),WIDTH))).isTrue())&&(((plt.types.Logic)(plt.Kernel.string_equal__question_((cow_dash_direction(a_dash_cow)),("right")))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE)).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object move_dash_cow_dash_with_dash_collision(Object a_dash_cow, Object all_dash_cows) { return (((plt.types.Logic)((((((plt.types.Logic)(cow_dash_collides_dash_with_dash_edge_question_(a_dash_cow))).isTrue())||(((plt.types.Logic)(cow_dash_collides_dash_any_dash_cow_question_(a_dash_cow,all_dash_cows))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE))).isTrue() ? ((make_dash_cow((cow_dash_x(a_dash_cow)),(((plt.types.Logic)((plt.Kernel.string_equal__question_((cow_dash_direction(a_dash_cow)),("left"))))).isTrue() ? (("right")) : (("left")))))) : ((move_dash_cow(a_dash_cow)))); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object move_dash_cows_dash_with_dash_collision(Object cows, Object all_dash_cows) { return (((plt.types.Logic)((plt.Kernel.empty_question_(cows)))).isTrue() ? (plt.types.Empty.EMPTY) : ((plt.Kernel.cons((move_dash_cow_dash_with_dash_collision((plt.Kernel.first(cows)),all_dash_cows)),(move_dash_cows_dash_with_dash_collision((plt.Kernel.rest(cows)),all_dash_cows)))))); }
// Test case erased

// Test case erased

// Test case erased

static public Object drop_dash_ufo_dash_and_dash_move_dash_cows(Object a_dash_world) { return (make_dash_world((plt.Kernel.make_dash_posn((plt.Kernel.posn_dash_x((world_dash_ufo(a_dash_world)))),(plt.Kernel._plus_((plt.Kernel.posn_dash_y((world_dash_ufo(a_dash_world)))),(new plt.types.Rational(5, 1)))))),(move_dash_cows_dash_with_dash_collision((world_dash_cows(a_dash_world)),(world_dash_cows(a_dash_world)))))); }
// Test case erased

// Test case erased

static public Object ufo_dash_hitting_dash_ground_question_(Object a_dash_world) { return (plt.Kernel._greaterthan__equal_((plt.Kernel.posn_dash_y((world_dash_ufo(a_dash_world)))),HEIGHT)); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object square(Object x) { return (plt.Kernel._star_(x,x)); }
// Test case erased

// Test case erased

static public Object distance(Object pos_dash_1, Object pos_dash_2) { return (plt.Kernel.sqrt((plt.Kernel._plus_((square((plt.Kernel._dash_((plt.Kernel.posn_dash_x(pos_dash_1)),(plt.Kernel.posn_dash_x(pos_dash_2)))))),(square((plt.Kernel._dash_((plt.Kernel.posn_dash_y(pos_dash_1)),(plt.Kernel.posn_dash_y(pos_dash_2)))))))))); }
// Test case erased

// Test case erased

// Test case erased

static public Object ufo_dash_hitting_dash_cow_question_(Object a_dash_ufo, Object a_dash_cow) { return (plt.Kernel._lessthan_((distance(a_dash_ufo,(plt.Kernel.make_dash_posn((cow_dash_x(a_dash_cow)),COW_dash_Y)))),(plt.Kernel.image_dash_height(UFO)))); }
// Test case erased

// Test case erased

static public Object ufo_dash_hitting_dash_any_dash_cow_question_(Object a_dash_ufo, Object cows) { return (((plt.types.Logic)((plt.Kernel.empty_question_(cows)))).isTrue() ? ((plt.types.Logic.FALSE)) : ((((((plt.types.Logic)(ufo_dash_hitting_dash_cow_question_(a_dash_ufo,(plt.Kernel.first(cows))))).isTrue())||(((plt.types.Logic)(ufo_dash_hitting_dash_any_dash_cow_question_(a_dash_ufo,(plt.Kernel.rest(cows))))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE))); }
// Test case erased

// Test case erased

// Test case erased

static public Object ufo_dash_hitting_dash_something_question_(Object a_dash_world) { return (((((plt.types.Logic)(ufo_dash_hitting_dash_ground_question_(a_dash_world))).isTrue())||(((plt.types.Logic)(ufo_dash_hitting_dash_any_dash_cow_question_((world_dash_ufo(a_dash_world)),(world_dash_cows(a_dash_world))))).isTrue())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE); }
// Test case erased

// Test case erased

// Test case erased

// Test case erased

static public Object move_dash_ufo(Object a_dash_world, Object a_dash_key_dash_event) { return (((plt.types.Logic)((plt.Kernel.key_equal__question_(a_dash_key_dash_event,(plt.types.Symbol.makeInstance("left")))))).isTrue() ? ((make_dash_world((plt.Kernel.make_dash_posn((plt.Kernel._dash_((plt.Kernel.posn_dash_x((world_dash_ufo(a_dash_world)))),(new plt.types.Rational(10, 1)))),(plt.Kernel.posn_dash_y((world_dash_ufo(a_dash_world)))))),(world_dash_cows(a_dash_world))))) : ((((plt.types.Logic)((plt.Kernel.key_equal__question_(a_dash_key_dash_event,(plt.types.Symbol.makeInstance("right")))))).isTrue() ? ((make_dash_world((plt.Kernel.make_dash_posn((plt.Kernel._plus_((plt.Kernel.posn_dash_x((world_dash_ufo(a_dash_world)))),(new plt.types.Rational(10, 1)))),(plt.Kernel.posn_dash_y((world_dash_ufo(a_dash_world)))))),(world_dash_cows(a_dash_world))))) : (a_dash_world)))); }
// Test case erased

// Test case erased

// Test case erased

static public Object draw_dash_cows(Object cows) { return (((plt.types.Logic)((plt.Kernel.empty_question_(cows)))).isTrue() ? (EMPTY_dash_SCENE) : ((plt.Kernel.place_dash_image((((plt.types.Logic)((plt.Kernel.string_equal__question_((cow_dash_direction((plt.Kernel.first(cows)))),("left"))))).isTrue() ? (COW_dash_LEFT) : (COW_dash_RIGHT)),(cow_dash_x((plt.Kernel.first(cows)))),COW_dash_Y,(draw_dash_cows((plt.Kernel.rest(cows)))))))); }
static public Object draw_dash_world(Object a_dash_world) { return (plt.Kernel.place_dash_image((((plt.types.Logic)((ufo_dash_hitting_dash_any_dash_cow_question_((world_dash_ufo(a_dash_world)),(world_dash_cows(a_dash_world)))))).isTrue() ? (RED_dash_UFO) : (UFO)),(plt.Kernel.posn_dash_x((world_dash_ufo(a_dash_world)))),(plt.Kernel.posn_dash_y((world_dash_ufo(a_dash_world)))),(draw_dash_cows((world_dash_cows(a_dash_world)))))); }


    // #0 big-bang's world0
    Object world = (make_dash_world((plt.Kernel.make_dash_posn((plt.Kernel._slash_(WIDTH,(new plt.types.Rational(2, 1)))),(new plt.types.Rational(0, 1)))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("left"))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("right"))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("left"))),plt.types.Empty.EMPTY))))))));

    // When a program is paused, we'll restart the world.
    public void pauseApp() {
	world = (make_dash_world((plt.Kernel.make_dash_posn((plt.Kernel._slash_(WIDTH,(new plt.types.Rational(2, 1)))),(new plt.types.Rational(0, 1)))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("left"))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("right"))),(plt.Kernel.cons((make_dash_cow((plt.Kernel.random(WIDTH)),("left"))),plt.types.Empty.EMPTY))))))));
    }

    public void destroyApp(boolean unconditional) {}

    public void startApp() {
	MyCanvas canvas = new MyCanvas();
	Display.getDisplay(this).setCurrent(canvas);
	new Thread(canvas).start();
    }

    class MyCanvas extends Canvas implements Runnable {
	boolean stopped = false;
	public void run() {
	    // #1 delay expression
	    long delay = (long) (((plt.Kernel._star_((new plt.types.Rational(1, 10)),(new plt.types.Rational(1000, 1))))).toInt());
	    while(! stopped) {
		repaint();
		if(((plt.types.Logic) (ufo_dash_hitting_dash_something_question_(world))).isTrue()) {
		    //  #2 stop-when expression
		    stopped = true;
		} else {
		    try {
			Thread.sleep(delay);
		    } catch (InterruptedException e) {}
		    synchronized(world) {
			//  #3 on-tick expression
			world = (drop_dash_ufo_dash_and_dash_move_dash_cows(world));
		    }
		}
	    }
	}

	public void paint(Graphics g) {
	    // #4,5,6 on-redraw expression
	    ((plt.gui.Scene) (draw_dash_world(world))).draw(g, 0, 0);
	}

	protected void keyPressed(int keyCode) {
	    Object aKey;
	    if (stopped) { return; }
	    if (keyCode > 0) {
		aKey = new Character((char)keyCode);
	    } else {
		String action;
		switch(getGameAction(keyCode)) {
		case Canvas.LEFT: aKey = "left"; break;
		case Canvas.RIGHT: aKey = "right"; break;
		case Canvas.UP: aKey = "up"; break;
		case Canvas.DOWN: aKey = "down"; break;
		default: aKey= "";
		}
	    }
	    synchronized(world) {
		world = (move_dash_ufo(world,aKey));
	    }
	    repaint();
	}
    }
}
