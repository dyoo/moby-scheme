package org.plt.guiworld;

import org.plt.world.*;
import org.plt.guiworldtest.*;

import android.view.*;
import android.widget.*;
import android.text.*;

import android.view.View.OnClickListener;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.CompoundButton.OnCheckedChangeListener;

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
		this.gui.accept(new InitialGuiConstructor(view));
	}

	// Changes the world, and updates the view accordingly.
	public void changeWorld(Object world) {
		this.world = world;
		this.gui.accept(new GuiRefresher(0, view));
	}

	// ////////////////////////////////////////////////////////////////////
	private class InitialGuiConstructor implements GuiVisitor {
		private LinearLayout topView;

		public InitialGuiConstructor(LinearLayout view) {
			this.topView = view;
		}

		public void visit(Row r) {
			LinearLayout parent = new LinearLayout(topView.getContext());
			parent.setOrientation(LinearLayout.HORIZONTAL);
			topView.addView(parent);
			InitialGuiConstructor visitor = new InitialGuiConstructor(parent);

			Gui[] items = r.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(visitor);
			}
		}

		public void visit(Col c) {
			LinearLayout parent = new LinearLayout(topView.getContext());
			parent.setOrientation(LinearLayout.VERTICAL);
			topView.addView(parent);
			InitialGuiConstructor visitor = new InitialGuiConstructor(parent);

			Gui[] items = c.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(visitor);
			}
		}

		public void visit(Message m) {
			String msg = (m.getValF().transform(world)).toString();
			TextView txt = new TextView(topView.getContext());
			txt.setText(msg);
			topView.addView(txt);
		}

		public void visit(final TextField t) {
			String txt = (t.getValF().transform(world)).toString();
			EditText edit = new EditText(topView.getContext());
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

			topView.addView(edit);
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(final org.plt.guiworld.Button b) {
			String label = (b.getValF().transform(world)).toString();
			android.widget.Button update = new android.widget.Button(topView
					.getContext());
			update.setText(label);
			update.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					Object newWorld = b.getCallback().transform(world);
					changeWorld(newWorld);
				}
			});
			topView.addView(update);
		}

		public void visit(final org.plt.guiworld.CheckBox c) {
			String msg = (c.getValF().transform(world)).toString();
			android.widget.CheckBox cb = new android.widget.CheckBox(topView
					.getContext());
			cb.setText(msg);
			cb.setOnCheckedChangeListener(new OnCheckedChangeListener() {
				public void onCheckedChanged(CompoundButton buttonView,
						boolean isChecked) {
					Object newWorld = c.getCallback().transform(world,
							new Boolean(isChecked));
					changeWorld(newWorld);
				}
			});

			topView.addView(cb);
		}

		public void visit(final Slider s) {
			Integer cur = (Integer) (s.getValF().transform(world));
			SeekBar bar = new SeekBar(topView.getContext());
			bar.setMax(100);
			bar.setProgress(cur.intValue() % 100);
			bar.setOnSeekBarChangeListener(new OnSeekBarChangeListener() {
				public void onProgressChanged(SeekBar seekBar, int progress,
						boolean fromTouch) {
					Object newWorld = s.getCallback().transform(world,
							new Integer(progress));
					changeWorld(newWorld);
				}

				public void onStartTrackingTouch(SeekBar seekBar) {

				}

				public void onStopTrackingTouch(SeekBar seekBar) {

				}
			});

			topView.addView(bar);
		}

		public void visit(final DropDown d) {
			ArrayAdapter adapter = new ArrayAdapter(topView.getContext(),
					android.R.layout.simple_list_item_1, d.getItems());
			adapter
					.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
			Spinner dropdown = new Spinner(topView.getContext());
			dropdown.setAdapter(adapter);
			dropdown.setOnItemSelectedListener(new OnItemSelectedListener() {
				public void onItemSelected(AdapterView parent, View view,
						int position, long id) {
					Object item = parent.getSelectedItem();
					Object newWorld = d.getCallback().transform(world, item);
					changeWorld(newWorld);
				}

				public void onNothingSelected(AdapterView parent) {

				}
			});
			topView.addView(dropdown);
		}
	}

	// /////////////////////GuiRefresher///////////////////////////////
	private class GuiRefresher implements GuiVisitor {
		private int viewIndex;
		private LinearLayout topView;

		public GuiRefresher(int viewIndex, LinearLayout view) {
			this.viewIndex = viewIndex;
			this.topView = view;
		}

		public void visit(Row r) {
			Gui[] items = r.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(new GuiRefresher(i, (LinearLayout) topView
						.getChildAt(this.viewIndex)));
			}
		}

		public void visit(Col c) {
			Gui[] items = c.getItems();
			for (int i = 0; i < items.length; i++) {
				items[i].accept(new GuiRefresher(i, (LinearLayout) topView
						.getChildAt(this.viewIndex)));
			}
		}

		public void visit(Message m) {
			TextView txt = (TextView) topView.getChildAt(this.viewIndex);
			txt.setText(m.getValF().transform(world).toString());
		}

		// FIXME: implement these!
		public void visit(TextField t) {
			String label = (t.getValF().transform(world)).toString();
			EditText edit = (EditText) topView.getChildAt(this.viewIndex);
			edit.setText(label);
		}

		public void visit(BoxGroup b) {
		}

		public void visit(Canvas c) {
		}

		public void visit(org.plt.guiworld.Button b) {
			String label = (b.getValF().transform(world)).toString();
			android.widget.Button update = (android.widget.Button) topView
					.getChildAt(this.viewIndex);
			update.setText(label);
		}

		public void visit(org.plt.guiworld.CheckBox c) {
			String label = (c.getValF().transform(world)).toString();
			android.widget.CheckBox cb = (android.widget.CheckBox) topView
					.getChildAt(this.viewIndex);
			cb.setText(label);
		}

		public void visit(Slider s) {
			SeekBar bar = (SeekBar) topView.getChildAt(this.viewIndex);
			try {
				Integer cur = (Integer) s.getValF().transform(world);
				bar.setProgress(cur.intValue() % 100);
			} catch (ClassCastException e) {

			}
		}

		public void visit(DropDown d) {
			Spinner dropdown = (Spinner) topView.getChildAt(this.viewIndex);
			try {
				Integer item = Integer.parseInt((d.getValF().transform(world))
						.toString());
				if (item.intValue() < dropdown.getCount())
					dropdown.setSelection(item.intValue());
			} catch (NumberFormatException e) {

			}
		}
	}

}
