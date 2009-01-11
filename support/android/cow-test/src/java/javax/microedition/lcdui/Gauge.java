package javax.microedition.lcdui;

import j2ab.android.lcdui.Toolkit;

import javax.microedition.midlet.MIDlet;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;

public class Gauge extends Item implements Runnable
{
    // TODO : interactive is ignored
    private boolean interactive;
    private int maxValue;
    private int value;
    private View view;
    private ProgressBar progressBar;
    private TextView label;
    private MIDlet midlet;
    
    public Gauge( String label, boolean interactive, int maxValue, int initialValue )
    {
        super( label );
        this.interactive = interactive;
        this.maxValue = maxValue;
        this.value = initialValue;
    }

    public int getValue()
    {
        return this.value;
    }
    
    public void setValue( int value )
    {
        this.value = value;
    	if( this.midlet != null )
    	{
    		this.midlet.invokeAndWait( this );
    	}
    }
    
    public int getMaxValue()
    {
        return this.maxValue;
    }
    
    public void setMaxValue( int maxValue )
    {
        this.maxValue = maxValue;
    	if( this.midlet != null )
    	{
    		this.midlet.invokeAndWait( this );
    	}
    }

    @Override
    public void setLabel( String label )
    {
        super.setLabel(label);
        if( this.label != null )
        {
            this.midlet.invokeAndWait( this );
        }
    }

    @Override
    public void dispose()
    {
    	this.progressBar.setVisibility( ProgressBar.GONE );
        this.view = null;
        this.progressBar = null;
        this.label = null;
        this.midlet = null;
    }

    @Override
    public View getView()
    {
        return this.view;
    }

    @Override
    public void init( MIDlet midlet, ViewGroup parent )
    {
    	Context context = midlet.getActivity();
    	Toolkit toolkit = midlet.getToolkit();
    	View view = toolkit.inflate(
    			toolkit.getResourceId( "layout.midpgauge" )
    	);
    	
    	int id = view.getId();
//    	ViewGroup vg = (ViewGroup)view;
//    	for( int i=0; i<vg.getChildCount(); i++ )
//    	{
//    		View v = vg.getChildAt( i );
//    		int childId = v.getId();
//    		System.out.println( Integer.toString( childId, 16 ) );
//    	}
    	
        ProgressBar progressBar = (ProgressBar)view.findViewById( 
        		toolkit.getResourceId( "id.midpgauge_progressbar" ) 
        );
//        progressBar.setLayoutParams( 
//        		new LinearLayout.LayoutParams( 
//        				LinearLayout.LayoutParams.WRAP_CONTENT, 
//        				LinearLayout.LayoutParams.WRAP_CONTENT 
//        		) 
//        );
        progressBar.setMax( this.maxValue );
        progressBar.setProgress( this.value );
//        progressBar.setIndeterminate( false );
//        progressBar.setOrientation( ProgressBar.HORIZONTAL );
//        progressBar.setVisibility( ProgressBar.VISIBLE );
        
        TextView label = (TextView)view.findViewById( 
        		toolkit.getResourceId( "R.id.midpgauge_label" )
        );
//        label.setLayoutParams(         		
//        	new LinearLayout.LayoutParams( 
//				LinearLayout.LayoutParams.FILL_PARENT, 
//				LinearLayout.LayoutParams.WRAP_CONTENT 
//        	) 
//        );
        label.setText( this.getLabel() );
        
//        view.addView( label );
//        view.addView( progressBar );
        
        this.view = view;
        this.progressBar = progressBar;
        this.label = label;
        this.midlet = midlet;
    }

//	@Override
	public void run() 
	{
		this.progressBar.setMax( this.maxValue );
		this.progressBar.setProgress( this.value );
		this.label.setText( this.getLabel() );
		this.progressBar.refreshDrawableState();
	}

}
