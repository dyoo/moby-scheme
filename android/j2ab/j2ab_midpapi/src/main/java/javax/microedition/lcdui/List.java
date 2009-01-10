package javax.microedition.lcdui;

import j2ab.android.lcdui.Toolkit;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.microedition.midlet.MIDlet;

import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnKeyListener;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;

public class List extends Screen implements OnKeyListener {

	public static final int EXCLUSIVE = 0;
	public static final int IMPLICIT = 1;
	public static final int MULTIPLE = 2;
	public static final int POPUP = 3;
	
	private static final String KEY_IMAGE = "image";
	private static final String KEY_TEXT = "text";
	
	private String title;
	private int listType;
	private int selectedIndex;
	
	private ListView view;
	private ArrayList<Map<String,?>> data;
	
	private MIDlet midlet;
	
	private Command selectCommand;
	
	public List( String title, int listType ) {
		this.title = title;
		this.listType = listType;
		this.data = new ArrayList<Map<String,?>>();
	}
	
	@Override
	public void disposeDisplayable() {
		this.view = null;
		this.data = null;
	}

	@Override
	public View getView() {
		return this.view;
	}

	@Override
	public void initDisplayable(MIDlet midlet) {
        this.midlet = midlet;
    	
        // TODO : use appearance mode to choose item
        Toolkit toolkit = midlet.getToolkit();
        this.view = (ListView)toolkit.inflate( 
        		toolkit.getResourceId( "layout.midplist" )
        );	
        this.view.setOnKeyListener( this );
        
        // set up the model
        ListAdapter model = new SimpleAdapter( 
        		midlet.getActivity(), 
        		this.data, 
        		toolkit.getResourceId( "layout.midplistitem" ),
        		new String[] { KEY_IMAGE, KEY_TEXT }, 
        		new int[]{ 
        			toolkit.getResourceId( "id.midplistitem_image" ), 
        			toolkit.getResourceId( "id.midplistitem_text" ) 
        		} 
        );
        this.view.setAdapter( model );
        this.view.setSelection( this.selectedIndex );
	}

	public int append( String stringPart, Image imagePart ) {
		HashMap<String, Object> row = new HashMap<String, Object>( 2 );
		// TODO : this isn't quite right, would be better to translate the Bitmap using 
		// some kind of renderer
		if( imagePart != null ) {
			row.put( KEY_IMAGE, imagePart.getBitmap() );
		}
		row.put( KEY_TEXT, stringPart );
		this.data.add( row );
		return this.data.size();
	}

	public Command getSelectCommand() {
		return selectCommand;
	}

	public void setSelectCommand(Command selectCommand) {
		this.selectCommand = selectCommand;
	}

	public int getSelectedIndex() {
		if( this.view != null ) {
			return this.view.getSelectedItemPosition();			
		} else {
			return this.selectedIndex;
		}
	}
	
	public String getString( int index ) {
		Map<String, ?> row = this.data.get( index );
		return (String)row.get( KEY_TEXT );
	}
	
	public void setSelectedIndex( int index, boolean value ) {
		this.selectedIndex = index;
		if( this.view != null ) {
			this.view.setItemChecked( index, value );
		}
	}

	@Override
	public boolean onKey( View source, int key, KeyEvent event ) {
		boolean handled;
		if( event.getKeyCode() == KeyEvent.KEYCODE_ENTER && this.commandListener != null && this.selectCommand != null ) {
			handled = true;
			this.commandListener.commandAction( this.selectCommand, this );
		} else{
			handled = false;
		}
		return handled;
	}
	
}
