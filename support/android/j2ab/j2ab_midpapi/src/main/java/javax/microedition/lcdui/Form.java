package javax.microedition.lcdui;

import j2ab.android.lcdui.Toolkit;

import java.util.ArrayList;
import java.util.List;

import javax.microedition.midlet.MIDlet;

import android.view.View;
import android.widget.LinearLayout;

public class Form extends Screen
{
    private String title;
    private MIDlet midlet;
    
    private LinearLayout view;
    private List<Item> items;
    
    public Form( String title )
    {
        this.items = new ArrayList<Item>();
        this.title = title;
    }
    
    public Form( String title, Item[] items )
    {
    	this( title );
    	for( int i=0; i<items.length; i++ )
    	{
    		append( items[i] );
    	}
    }
    
    public String getTitle()
    {
        return this.title;
    }
    
    public void setTitle( String title )
    {
        this.title = title;
        
    }
    
    public int append( Item item )
    {
        if( this.midlet != null )
        {
            item.init( this.midlet, this.view );
            this.view.addView( item.getView() );
        }
        this.items.add( item );
        return this.items.size() - 1;
    }
    
    @Override
    public void disposeDisplayable()
    {
        for( int i=0; i<this.items.size(); i++ )
        {
            Item item = this.items.get( i );
            item.dispose();
        }
        this.midlet = null;
        this.view = null;
    }

    @Override
    public View getView()
    {
        return this.view;
    }

    @Override
    public void initDisplayable( MIDlet midlet )
    {
    	Toolkit toolkit = midlet.getToolkit();
        LinearLayout view = (LinearLayout)toolkit.inflate( 
        		toolkit.getResourceId("layout.midpform")
        );

        this.view = view;
        this.midlet = midlet;
        
        for( int i=0; i<this.items.size(); i++ )
        {
            Item item = this.items.get( i );
            item.init( midlet, view );
            View itemView = item.getView();
            // TODO : remove this
            itemView.setLayoutParams( 
            		new LinearLayout.LayoutParams( 
            				LinearLayout.LayoutParams.FILL_PARENT, 
            				LinearLayout.LayoutParams.WRAP_CONTENT
            		) 
            );
            view.addView( itemView );
        }
    }
    
    public Item get( int pos ) 
    {
    	return this.items.get( pos );
    }
}
