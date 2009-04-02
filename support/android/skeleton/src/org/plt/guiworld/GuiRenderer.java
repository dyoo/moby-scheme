package org.plt.guiworld;

import org.plt.world.*;

import java.util.List;
import java.util.ArrayList;


import android.view.*;
import android.widget.*;
import android.text.*;
import android.graphics.Color;

import android.view.View.OnClickListener;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.CompoundButton.OnCheckedChangeListener;

// GuiRenderer: creates the initial gui, given the world and the view.
// The visitor also keeps things up to date.

/**
 * GuiRenderer's rendering a {Gui, World} is like Interpreter's interpreting an
 * Expression
 */
public class GuiRenderer {
    // view contains a subView corresponded to gui
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
    private void initializeForm() {
	this.gui.accept(new InitialGuiConstructor(view));
    }

    // Changes the world, and updates the view accordingly.
    public void changeWorld(Object world) {
	this.world = world;
	this.gui.accept(new GuiRefresher(0, view));
    }

    /**
     * It constructs a view corresponded to type of gui and displays the
     * constructed view in topView.
     */
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
	    String label = (b.getValF().transform(world)).toString();
	    LinearLayout group = new LinearLayout(view.getContext());
	    group.setBackgroundColor(Color.BLUE);
	    group.setOrientation(LinearLayout.VERTICAL);
	    TextView txtView = new TextView(group.getContext());
	    txtView.setText(label);
	    txtView.setBackgroundColor(Color.YELLOW);
	    txtView.setTextColor(Color.BLACK);
	    LinearLayout guiView = new LinearLayout(group.getContext());
	    InitialGuiConstructor visitor = new InitialGuiConstructor(guiView);
	    group.addView(txtView);
	    group.addView(guiView);
	    topView.addView(group);

	    b.getGui().accept(visitor);
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
	    String msg = (c.getLabelValF().transform(world)).toString();
	    org.plt.types.Logic checked = 
		(org.plt.types.Logic) c.getCheckValF().transform(world);
	    android.widget.CheckBox cb = new android.widget.CheckBox(topView
								     .getContext());
	    cb.setText(msg);
	    cb.setChecked(checked.isTrue());
	    cb.setOnCheckedChangeListener(new OnCheckedChangeListener() {
		    public void onCheckedChanged(CompoundButton buttonView,
						 boolean isChecked) {
			Object newWorld = 
			    c.getCallback().transform
			    (world, (isChecked ?
				     org.plt.types.Logic.TRUE : 
				     org.plt.types.Logic.FALSE));
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
	    List itemList = new ArrayList();
	    ArrayAdapter adapter =
		new ArrayAdapter(topView.getContext(),
				 android.R.layout.simple_spinner_item,
				 itemList);
	    adapter.setDropDownViewResource
		(android.R.layout.simple_spinner_dropdown_item);
	    Spinner dropdown = new Spinner(topView.getContext());
	    dropdown.setAdapter(adapter);


	    org.plt.types.List items = (org.plt.types.List)
		d.getChoicesF().transform(world);
	    String selected = (String) d.getValF().transform(world);
	    int i = 0;
	    while(!items.isEmpty()) {
		Object nextItem = items.first();
		adapter.add(nextItem);
		if (nextItem.equals(selected)) {
		    dropdown.setSelection(i);
		}
		items = items.rest();
		i++;
	    }

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

    /**
     * It refreshes an item according to its corresponded gui-type in topView
     */
    private class GuiRefresher implements GuiVisitor {
	// viewIndex indicates which Gui in topView should be refreshed
	private int indexToRefresh;
	private LinearLayout topView;

	public GuiRefresher(int indexToRefresh, LinearLayout view) {
	    this.indexToRefresh = indexToRefresh;
	    this.topView = view;
	}

	public void visit(Row r) {
	    Gui[] items = r.getItems();
	    for (int i = 0; i < items.length; i++) {
		items[i].accept(new GuiRefresher(i, (LinearLayout) topView
						 .getChildAt(this.indexToRefresh)));
	    }
	}

	public void visit(Col c) {
	    Gui[] items = c.getItems();
	    for (int i = 0; i < items.length; i++) {
		items[i].accept(new GuiRefresher(i, (LinearLayout) topView
						 .getChildAt(this.indexToRefresh)));
	    }
	}

	public void visit(Message m) {
	    TextView txt = (TextView) topView.getChildAt(this.indexToRefresh);
	    txt.setText(m.getValF().transform(world).toString());
	}

	public void visit(TextField t) {
	    String label = (t.getValF().transform(world)).toString();
	    EditText edit = (EditText) topView.getChildAt(this.indexToRefresh);
	    // Try to preserve selection.
	    int selectionStart = edit.getSelectionStart();
	    int selectionEnd = edit.getSelectionEnd();
	    edit.setText(label);
			
	    edit.setSelection(clamp(selectionStart, 0, edit.getText().length()),
			      clamp(selectionEnd, 0, edit.getText().length()));
	}
	        
	private int clamp(int x, int a, int b) {
	    if (x < a) 
		return a;
	    if (x > b)
		return b;
	    return x;
	}

	public void visit(BoxGroup b) {
	    String label = (b.getValF().transform(world)).toString();
	    LinearLayout group = (LinearLayout) topView
		.getChildAt(this.indexToRefresh);
	    TextView txtView = (TextView) group.getChildAt(0);
	    LinearLayout guiView = (LinearLayout) group.getChildAt(1);

	    txtView.setText(label);
	    b.getGui().accept(new GuiRefresher(0, guiView));
	}

	public void visit(Canvas c) {
	}

	public void visit(org.plt.guiworld.Button b) {
	    String label = (b.getValF().transform(world)).toString();
	    android.widget.Button update = (android.widget.Button) topView
		.getChildAt(this.indexToRefresh);
	    update.setText(label);
	}

	public void visit(org.plt.guiworld.CheckBox c) {
	    String label = (c.getLabelValF().transform(world)).toString();
	    org.plt.types.Logic checked = (org.plt.types.Logic)
		c.getCheckValF().transform(world);
	    android.widget.CheckBox cb = (android.widget.CheckBox) topView
		.getChildAt(this.indexToRefresh);
	    cb.setText(label);
	    cb.setChecked(checked.isTrue());
	}

	public void visit(Slider s) {
	    SeekBar bar = (SeekBar) topView.getChildAt(this.indexToRefresh);
	    try {
		Integer cur = (Integer) s.getValF().transform(world);
		bar.setProgress(cur.intValue() % 100);
	    } catch (ClassCastException e) {

	    }
	}

	public void visit(DropDown d) {
	    org.plt.types.List items = (org.plt.types.List)
		d.getChoicesF().transform(world);
	    String selected = (String) d.getValF().transform(world);
	    Spinner dropdown = (Spinner) topView
		.getChildAt(this.indexToRefresh);
	    ArrayAdapter adapter = (ArrayAdapter) dropdown.getAdapter();
	    adapter.clear();
	    int i = 0;
	    while(!items.isEmpty()) {
		Object nextItem = items.first();
		adapter.add(nextItem);
		if(nextItem.equals(selected)) {
		    dropdown.setSelection(i);
		}
		items = items.rest();
		i++;
	    }
	}
    }
}
