package org.plt;

import android.app.Activity;
import android.os.Bundle;

import org.plt.guiworld.*;
import org.plt.world.WorldTransformer;
import org.plt.world.WorldAndObjectTransformer;

public class GuiWorldTest extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

	Gui view = new Row(new Gui[] {

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

		// Button has not yet been implemented.
		new Button(new WorldTransformer() {
			// label
			public Object transform(Object world) {
			    return "says: " + world.toString();
			}
		    },

		    // callback
		    new WorldTransformer() {
			public Object transform(Object world) {
			    return new Integer(
					       ((Integer) world).intValue() + 1);
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
				    Integer newWorld = new Integer(((Integer) world)
								   .intValue()
								   + val);
				    return newWorld;
				} catch (NumberFormatException e) {
				    return new Integer(0);
				}
			    }
			}),

		new DropDown(new WorldTransformer() {
			public Object transform(Object world) {
			    Object[] items = new Object[] { "item1", "item2",
							    "item3" };
			    return items;
			}
		    })

	    });


	// Uncomment this when we have a GuiRenderer.
// 	GuiRenderer guiRender = new GuiRenderer("title", world, view);
// 	this.setContentView(R.guiRender.getView());

    }
}
