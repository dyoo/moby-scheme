package org.plt.guiworld;

import org.plt.types.*;
import org.plt.world.WorldTransformer;

public class GuiWorld {
    private static Object initialWorld;
    private static Object view;

    public static Object bigBang(Object initialWorld,
				 Object view) {
	GuiWorld.initialWorld = initialWorld;
	GuiWorld.view = view;
	return VoidObject.VOID;
    }

    public static Object getInitialWorld() {
	return GuiWorld.initialWorld;
    }


    public static Object getView() {
	return GuiWorld.view;
    }


    public static Object row(Object[] args) {
	Gui[] elts = new Gui[args.length];
	for(int i = 0; i < args.length; i++)
	    elts[i] = (Gui) args[i];
	return new Row(elts);
    }


    public static Object col(Object[] args) {
	Gui[] elts = new Gui[args.length];
	for(int i = 0; i < args.length; i++)
	    elts[i] = (Gui) args[i];
	return new Col(elts);
    }


    public static Object message(Object[] args) {
	final Callable c = coerseToCallable(args[0]);
	return new Message(new WorldTransformer() {
		public Object transform(Object world) {
		    return c.call(new Object[] { world });
		}
	    });
    }


    private static Callable coerseToCallable(final Object obj) {
	if (obj instanceof Callable) {
	    return (Callable) obj;
	} else {
	    return new Callable() {
		    public Object call(Object[] args) {
			return obj;
		    }
		};
	}
    }
}
