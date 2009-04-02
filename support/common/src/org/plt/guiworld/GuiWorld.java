package org.plt.guiworld;

import org.plt.types.*;
import org.plt.checker.SchemeException;

import org.plt.world.WorldTransformer;
import org.plt.world.WorldAndObjectTransformer;
import org.plt.world.WorldRunner;
import org.plt.world.WorldJudge;
import org.plt.world.config.ConfigReader;


public class GuiWorld {
    private static Object initialWorld;
    private static Gui view;
    private static WorldRunner runner;
    private static ConfigReader configReader;


    public static void setRunner(WorldRunner runner) {
	GuiWorld.runner = runner;
    }


    public static ConfigReader getConfigReader() {
	return GuiWorld.configReader;
    }


    public static Object bigBang(Object initialWorld,
				 Object view,
				 Object[] configs) {
	GuiWorld.initialWorld = initialWorld;
	GuiWorld.view = asGui(view);
	configReader = new ConfigReader();
	configReader.load(configs);


	runner.setWorld(initialWorld);
	runner.setDelay((long) (configReader.delay.toInt()));

	runner.setOnTick(new WorldTransformer() {
		public Object transform(Object world) {
		    return configReader.onTickHandler.call
			(new Object[] { world });
		}
	    });
	runner.setStopWhen(new WorldJudge() {
		public boolean judge(Object world) {
		    return ((org.plt.types.Logic) 
			    configReader.stopWhenHandler.call
			    (new Object[] { world }))
			.isTrue();
		}
	    });

	return runner.bigBang();
    }


    public static Object getInitialWorld() {
	return GuiWorld.initialWorld;
    }


    public static Gui getView() {
	return GuiWorld.view;
    }





    //////////////////////////////////////////////////////////////////////


    private static Gui asGui(Object obj) {
	if (obj instanceof Gui) {
	    return (Gui) obj;
	} else if (obj instanceof String) {
	    return message(obj);
	} else if (obj instanceof org.plt.types.Number) {
	    return message(obj.toString());
	} else {
	    throw new SchemeException("Don't know how to treat " + obj + " as a Gui");
	}
    }


    public static Gui row(Object[] args) {
	Gui[] elts = new Gui[args.length];
	for(int i = 0; i < args.length; i++)
	    elts[i] = asGui(args[i]);
	return new Row(elts);
    }


    public static Gui col(Object[] args) {
	Gui[] elts = new Gui[args.length];
	for(int i = 0; i < args.length; i++)
	    elts[i] = asGui(args[i]);
	return new Col(elts);
    }

    public static Gui boxGroup(Object _valF,
			       Object gui) {
	final Callable valF = coerseToCallable(_valF);
	return new BoxGroup(new WorldTransformer () {
		public Object transform(Object world) {
		    return valF.call(new Object[] { world });
		}
	    }, asGui(gui));
    }

    public static Gui message(Object msg) {
	final Callable c = coerseToCallable(msg);
	return new Message(new WorldTransformer() {
		public Object transform(Object world) {
		    return c.call(new Object[] { world });
		}
	    });
    }



    public static Gui dropDown(Object _valF,
			       Object _choicesF,
			       Object _callbackF) {
	final Callable valF = coerseToCallable(_valF);
	final Callable choicesF = coerseToCallable(_choicesF);
	final Callable callbackF = coerseToCallable(_callbackF);
	return new DropDown(new WorldTransformer() {
		public Object transform(Object world) {
		    return valF.call(new Object[] { world }); 
		}
	    },
	    new WorldTransformer() {
		public Object transform(Object world) {
		    return choicesF.call(new Object[] { world });
		}
	    },
	    new WorldAndObjectTransformer() {
		public Object transform(Object world, Object obj) {
		    return callbackF.call(new Object[] { world, obj });
		}
	    });
    }


    public static Gui button(Object _valF,
			     Object _callbackF) {
	final Callable valF = coerseToCallable(_valF);
	final Callable callbackF = coerseToCallable(_callbackF);
	return new Button(new WorldTransformer() {
		public Object transform(Object world) {
		    return valF.call(new Object[] { world });
		}
	    },
	    new WorldTransformer() {
		public Object transform(Object world) {
		    return callbackF.call(new Object[] { world });
		}
	    });
    }

    public static Gui textField(Object _valF,
				Object _callbackF) {
	final Callable valF = coerseToCallable(_valF);
	final Callable callbackF = coerseToCallable(_callbackF);
	return new TextField(new WorldTransformer() {
		public Object transform(Object world) {
		    return valF.call(new Object[] { world }); 
		}
	    },
	    new WorldAndObjectTransformer() {
		public Object transform(Object world, Object obj) {
		    return callbackF.call(new Object[] { world, obj });
		}
	    });
    }

    public static Gui checkBox(Object _labelF,
			       Object _valF,
			       Object _callbackF) {
	final Callable labelF = coerseToCallable(_labelF);
	final Callable valF = coerseToCallable(_valF);
	final Callable callbackF = coerseToCallable(_callbackF);

	return new CheckBox(new WorldTransformer() {
		public Object transform(Object world) {
		    return labelF.call(new Object[] { world }); 
		}
	    },
	    new WorldTransformer() {
		public Object transform(Object world) {
		    return valF.call(new Object[] { world }); 
		}
	    },
	    new WorldAndObjectTransformer() {
		public Object transform(Object world, Object obj) {
		    return callbackF.call(new Object[] { world, obj });
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
