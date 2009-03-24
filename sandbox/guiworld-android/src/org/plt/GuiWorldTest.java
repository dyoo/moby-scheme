package org.plt;

import android.app.Activity;
import android.os.Bundle;
import android.widget.*;
import org.plt.guiworld.*;
import org.plt.world.WorldTransformer;
import org.plt.world.WorldAndObjectTransformer;

public class GuiWorldTest extends Activity {
	private Object world;

	public GuiWorldTest() {
		world = new Integer(1);
	}

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		Gui gui = new Col(new Gui[] {

		new Row(new Gui[] { new Message(new WorldTransformer() {
			public Object transform(Object world) {
				return "Hello world";
			}
		}),

		new Message(new WorldTransformer() {
			// label
			public Object transform(Object world) {
				return "The World says: " + world.toString();
			}
		}),

		new org.plt.guiworld.Button(new WorldTransformer() {
			// label
			public Object transform(Object world) {
				return "says: " + world.toString();
			}
		}, new WorldTransformer() {
			public Object transform(Object world) {
				return new Integer(((Integer) world).intValue() + 1);
			}
		}) }),

		new org.plt.guiworld.TextField(new WorldTransformer() {
			public Object transform(Object world) {
				return "init text in text field: " + world.toString();
			}
		}, new WorldAndObjectTransformer() {
			public Object transform(Object world, Object obj) {
				try {
					int val = Integer.parseInt(obj.toString());
					Integer newWorld = new Integer(((Integer) world).intValue()
							+ val);
					return newWorld;
				} catch (NumberFormatException e) {
					return new Integer(0);
				}
			}
		}),

		new DropDown(new WorldTransformer() {
			public Object transform(Object world) {
				Integer count = (Integer) world;
				return Integer.toString(count);
			}
		}, new WorldTransformer() {
			public Object transform(Object world) {
				Integer count = (Integer) world;
				org.plt.types.List items = org.plt.types.Empty.EMPTY;
				for(int i = Math.max(count.intValue(), 20);
				    i >= 1; i--) {
				    items = new org.plt.types.Pair(String.valueOf(i),
								   items);
				}
				return items;
			}
		}, new WorldAndObjectTransformer() {
			public Object transform(Object world, Object obj) {
			    return new Integer(Integer.parseInt((String)obj));
				// return obj;
			    //  return world;
			}
		}),

		new Slider(new Integer(20), new Integer(50), new WorldTransformer() {
			public Object transform(Object world) {
				return Integer.parseInt(world.toString());
			}
		}, new WorldAndObjectTransformer() {
			public Object transform(Object world, Object obj) {
				return Integer.parseInt(obj.toString());
			}
		}),

		new org.plt.guiworld.CheckBox(new WorldTransformer() {
			public Object transform(Object world) {
				return "check: " + world.toString();
			}
		}, new WorldTransformer() {
			public Object transform(Object world) {
			    return Integer.parseInt(world.toString()) % 2 == 0 ?
				org.plt.types.Logic.TRUE : 
				org.plt.types.Logic.FALSE;
			}
		}, new WorldAndObjectTransformer() {
			public Object transform(Object world, Object obj) {
			    if (((org.plt.types.Logic) obj).isTrue()) {
				if (Integer.parseInt(world.toString()) % 2 == 0)
				    return world;
				else
				    return new Integer(Integer.parseInt(world.toString()) - 1);
			    } else {
				if (Integer.parseInt(world.toString()) % 2 == 0)
				    return new Integer(Integer.parseInt(world.toString()) - 1);
				else
				    return world;
			    }
			}
		}),

		new BoxGroup(new WorldTransformer() {
			public Object transform(Object world) {
				return "BoxGroup: " + world.toString();
			}
		}, new Message(new WorldTransformer() {
			// label
			public Object transform(Object world) {
				return "Message in BoxGroup: " + world.toString();
			}
		})) });

		// Uncomment this when we have a GuiRenderer.
		LinearLayout view = new LinearLayout(this);
		GuiRenderer guiRender = new GuiRenderer(world, view, gui);
		setContentView(guiRender.getView());
	}
}
