package javax.microedition.lcdui;

import j2ab.android.lcdui.Toolkit;

import javax.microedition.midlet.MIDlet;

import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

public class StringItem extends Item implements Runnable
{
    private String text;
    // TODO : we don't use appearance mode
    private int appearanceMode;
    private MIDlet midlet;
    private View view;
    private TextView labelView;
    private TextView textView;
    
    public StringItem( String label, String text )
    {
        this( label, text, PLAIN );
    }
    
    public StringItem( String label, String text, int appearanceMode )
    {
        setLabel( label );
        this.text = text;
        this.appearanceMode = appearanceMode;
    }
    
    public String getText()
    {
        return this.text;
    }
    
    public void setText( String text )
    {
        this.text = text;
        if( this.textView != null )
        {
        	this.midlet.getHandler().post( this );
        }
    }
    
    @Override
    public void setLabel( String label )
    {
        super.setLabel(label);
        if( this.labelView != null )
        {
            this.midlet.getHandler().post( this );
        }
    }

    @Override
    public void dispose()
    {
        this.view = null;
        this.textView = null;
        this.labelView = null;
    }

    @Override
    public View getView()
    {
        return this.view;
    }

    @Override
    public void init( MIDlet midlet, ViewGroup parent )
    {
        this.midlet = midlet;
    	
        // TODO : use appearance mode to choose item
        Toolkit toolkit = midlet.getToolkit();
        this.view = toolkit.inflate( 
        		toolkit.getResourceId( "layout.midpstringitem" )
        );

        this.textView = (TextView)this.view.findViewById( 
        		toolkit.getResourceId( "id.midpstringitem_value" ) 
        );
        this.labelView = (TextView)this.view.findViewById( 
        		toolkit.getResourceId( "id.midpstringitem_label" )
        );
        this.textView.setText( this.text );
        this.labelView.setText( this.getLabel() );
    }

//	@Override
	public void run() 
	{
		if( this.textView != null )
		{
			this.textView.setText( this.text );
		}
		if( this.labelView != null )
		{
	        this.labelView.setText( this.getLabel() );
		}
	}

    
}
