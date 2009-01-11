package javax.microedition.lcdui;

import javax.microedition.midlet.MIDlet;

import android.view.View;
import android.widget.TextView;

public class TextBox extends Screen implements Runnable
{
    // TODO : present title and maxSize sensibly
    private String title;
    private String text;
    private int maxSize;
    private int constraints;
    private MIDlet midlet;
    
    private TextView textView;
    
    public TextBox( String title, String text, int maxSize, int constraints )
    {
        this.title = title;
        this.text = text;
        this.maxSize = maxSize;
        this.constraints = constraints;
    }
    
    @Override
    public void disposeDisplayable()
    {
        this.textView = null;
        this.midlet = null;
    }

    @Override
    public View getView()
    {
        return this.textView;
    }

    @Override
    public void initDisplayable( MIDlet midlet )
    {
    	this.midlet = midlet;
        TextView textView = TextField.createTextView( this.constraints, midlet.getActivity() );
        textView.setText( this.text );
        this.textView = textView;
    }
    
    public int getMaxSize()
    {
        return this.maxSize;
    }
    
    public void setMaxSize( int maxSize )
    {
        this.maxSize = maxSize;
    }
    
    public String getString()
    {
        String result;
        if( this.textView != null )
        {
        	result = this.textView.getText().toString();
        } 
        else
        {
            result = this.text;
        }
        return result;
    }

    public void setString( String text )
    {
        this.text = text;
        if( this.textView != null )
        {
        	this.midlet.getHandler().post( this );
        }
    }
    
    public int getConstraints()
    {
        return this.constraints;
    }
    
    public void setConstraints( int constraints )
    {
        this.constraints = constraints;
        // TODO : adjust the view if it exists
    }
    
    public void run()
    {
    	if( this.textView != null )
    	{
    		this.textView.setText( this.text );
    	}
    }
}
