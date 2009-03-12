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

		new Message(new WorldTransformer() {
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
		},

		// callback
				new WorldTransformer() {
					public Object transform(Object world) {
						return new Integer(((Integer) world).intValue() + 1);
					}
				}),

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
				Object[] items = new Object[] { "0", "1", "2", "3" };
				return items;
			}
		}, new WorldAndObjectTransformer() {
			public Object transform(Object world, Object obj) {
				return Integer.parseInt(obj.toString());
			}
		})

		});

		// Uncomment this when we have a GuiRenderer.
		LinearLayout view = new LinearLayout(this);
		GuiRenderer guiRender = new GuiRenderer(world, view, gui);
		setContentView(guiRender.getView());
	}
}