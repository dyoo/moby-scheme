package org.plt.guiworld;

import org.plt.world.*;

import android.view.*;
import android.view.View.OnClickListener;
import android.widget.*;
import android.text.*;

// GuiRenderer: creates the initial gui, given the world and the view.
// The visitor also keeps things up to date.

public class GuiRenderer {

	// private String title;
	private LinearLayout view;
	private Object world;
	private Gui gui;

	public GuiRenderer(Object world, LinearLayout view, Gui gui) {
		this.world = world;
		this.view = view;
		view.setOrientation(LinearLayout.VERTICAL);
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
			String txt = (t.getValF().transform(world)).toString();
			EditText edit = new EditText(view.getContext());
			edit.setText(txt);
			edit.addTextChangedListener(new TextWatcher() {
				public void afterTextChanged(Editable s) {
					String str = s.toString();
					Object newWorld = t.getCallback().transform(world, str);
					changeWorld(newWorld);
				}

				public void beforeTextChanged(CharSequence s, int start,
						int count, int after) {

				}

				public void onTextChanged(CharSequence s, int start, int count,
						int after) {
				}
			});

			view.addView(edit);
		}

		public void visit(Col c) {
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(final org.plt.guiworld.Button b) {
			String label = (b.getValF().transform(world)).toString();
			android.widget.Button update = new android.widget.Button(view
					.getContext());
			update.setText(label);
			update.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					Object newWorld = b.getCallback().transform(world);
					changeWorld(newWorld);
				}
			});
			view.addView(update);
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
		private int viewIndex;

		public GuiRefresher(int viewIndex) {
			this.viewIndex = viewIndex;
		}

		public void visit(Row r) {
			Gui[] items = r.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(new GuiRefresher(this.viewIndex + i));
			}
		}

		public void visit(Message m) {
			TextView txt = (TextView) view.getChildAt(this.viewIndex);
			txt.setText(m.getValF().transform(world).toString());
		}

		// FIXME: implement these!
		public void visit(TextField t) {
			String label = (t.getValF().transform(world)).toString();
			EditText edit = (EditText) view.getChildAt(this.viewIndex);
			edit.setText(label);
		}

		public void visit(Col c) {
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(org.plt.guiworld.Button b) {
			String label = (b.getValF().transform(world)).toString();
			android.widget.Button update = (android.widget.Button) view
					.getChildAt(this.viewIndex);
			update.setText(label);
		}

		public void visit(Slider s) {
		}

		public void visit(DropDown d) {
		}

		public void visit(CheckBox c) {
		}
	}

}
