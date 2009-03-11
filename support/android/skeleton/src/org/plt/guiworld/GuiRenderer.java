package org.plt.guiworld;

import android.view.*;
import android.widget.*;
import org.plt.world.*;

// GuiRenderer: creates the initial gui, given the world and the view.
// The visitor also keeps things up to date.

public class GuiRenderer {

	private String title;
	LinearLayout view;
	Object world;
	private Gui gui;

	public GuiRenderer(Object world, LinearLayout view, Gui gui) {
		this.world = world;
		this.view = view;
		this.gui = gui;
		initializeForm();
	}

	// Returns the toplevel view.
	public LinearLayout getView() {
		return view;
	}

	// Initialize the form contents.
	public void initializeForm() {
		this.gui.accept(new InitialGuiConstructor(gui));
	}

	// Changes the world, and updates the view accordingly.
	public void changeWorld(Object world) {
		this.world = world;
		this.gui.accept(new GuiRefresher(0));
	}

	// ////////////////////////////////////////////////////////////////////

	private class InitialGuiConstructor implements GuiVisitor {
		private Gui gui;

		public InitialGuiConstructor(Gui gui) {
			this.gui = gui;
		}

		public void visit(Row r) {
			Gui[] items = r.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(this);
			}
		}

		public void visit(Message m) {
			String msg = (m.getValF().transform(world)).toString();
			TextView txt = new TextView(view.getContext());
			txt.setText(msg);
			view.addView(txt);
		}

		// FIXME: implement these!

		public void visit(final TextField t) {
			// String msg = (t.getValF().transform(world)).toString();
			// final javax.microedition.lcdui.TextField tf = new
			// javax.microedition.lcdui.TextField(
			// "", msg, 100, javax.microedition.lcdui.TextField.ANY);
			// topForm.append(tf);
			// topForm.setItemStateListener(new ItemStateListener() {
			// public void itemStateChanged(Item item) {
			// javax.microedition.lcdui.TextField tf =
			// (javax.microedition.lcdui.TextField) item;
			// String str = tf.getString();
			// Object newWorld = t.getCallback().transform(world, str);
			// changeWorld(newWorld);
			// }
			// });
		}

		public void visit(Col c) {
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(final Button b) {
			// /*
			// * topForm.append("") is a must!! Otherwise, mismatch will happen
			// in
			// * visit methods of GuiRefresher!!
			// */
			// topForm.append("");
			// String label = (b.getValF().transform(world)).toString();
			// Command update = new Command(label, Command.OK, 1);
			// topForm.addCommand(update);
			// topForm.setCommandListener(new CommandListener() {
			// public void commandAction(Command c, Displayable d) {
			// Object newWorld = b.getCallback().transform(world);
			// changeWorld(newWorld);
			// topForm.removeCommand(c);
			// visit(b);
			// }
			// });
		}

		public void visit(Slider s) {
		}

		public void visit(DropDown d) {
			// Object[] items = (Object[]) (d.getValF().transform(world));
			// ChoiceGroup cg = new ChoiceGroup("", Choice.EXCLUSIVE);
			// for (int i = 0; i < items.length; i++) {
			// cg.append(items[i].toString(), null);
			// }
			// topForm.append(cg);
		}

		public void visit(CheckBox c) {
		}
	}

	private class GuiRefresher implements GuiVisitor {
		private int formIndex;

		public GuiRefresher(int formIndex) {
			this.formIndex = formIndex;
		}

		public void visit(Row r) {
			Gui[] items = r.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(new GuiRefresher(this.formIndex + i));
			}
		}

		public void visit(Message m) {
			TextView txt = (TextView) view.getChildAt(this.formIndex);
			txt.setText(m.getValF().transform(world).toString());
		}

		// FIXME: implement these!
		public void visit(TextField t) {
			// javax.microedition.lcdui.TextField tf =
			// (javax.microedition.lcdui.TextField) topForm
			// .get(this.formIndex);
			// tf.setString(t.getValF().transform(world).toString());
		}

		public void visit(Col c) {
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(Button b) {

		}

		public void visit(Slider s) {
		}

		public void visit(DropDown d) {
		}

		public void visit(CheckBox c) {
		}
	}

}
