package org.plt.guiworld;

import org.plt.types.*;

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


}
