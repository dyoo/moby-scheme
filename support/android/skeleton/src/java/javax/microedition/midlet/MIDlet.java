package javax.microedition.midlet;

import j2ab.android.lcdui.Toolkit;

import java.util.Properties;

import javax.microedition.io.ConnectionNotFoundException;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Displayable;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Handler;
import android.view.View;

public abstract class MIDlet
{
	public static final String PROTOCOL_HTTP 	= "http://";
	public static final String PROTOCOL_HTTPS 	= "https://";
	public static final String PROTOCOL_SMS		= "sms:";
	public static final String PROTOCOL_PHONE	= "tel:";
	public static final String PROTOCOL_EMAIL	= "email:";

	public static MIDlet DEFAULT_MIDLET;
	public static Toolkit DEFAULT_TOOLKIT;
	public static Activity DEFAULT_ACTIVITY;
	public static Properties DEFAULT_APPLICATION_PROPERTIES;

	private android.view.Menu menu;
	
	private Activity activity = DEFAULT_ACTIVITY;
	private Toolkit toolkit = DEFAULT_TOOLKIT;
	private Properties applicationProperties = DEFAULT_APPLICATION_PROPERTIES;
	
    protected MIDlet()
    {
    	DEFAULT_MIDLET = this;
    }
    
    public Properties getApplicationProperties() 
    {
		return applicationProperties;
	}


	public void setApplicationProperties(Properties applicationProperties) 
	{
		this.applicationProperties = applicationProperties;
	}

	public android.view.Menu getMenu() 
	{
		return menu;
	}

	public void setMenu( android.view.Menu menu ) 
	{
		this.menu = menu;
		Display display = Display.getDisplay( this );
		Displayable current = display.getCurrent(); 
		if( current != null )
		{
			// load the menu items
			current.addCommandsToDisplay( display );
		}
	}
	
	public Handler getHandler()
	{
		return this.toolkit.getHandler();
	}
	
	public void invokeAndWait( Runnable r )
	{
		this.toolkit.invokeAndWait(r);
	}
	
	public Activity getActivity() 
	{
		return activity;
	}

	public void setActivity(Activity activity) 
	{
		this.activity = activity;
	}
	
	public Toolkit getToolkit() {
		return toolkit;
	}

	public void setToolkit(Toolkit toolkit) {
		this.toolkit = toolkit;
	}

	protected abstract void destroyApp( boolean unconditional )
        throws MIDletStateChangeException;
    
    protected abstract void pauseApp()
        throws MIDletStateChangeException; 
    
    protected abstract void startApp()
        throws MIDletStateChangeException;
    
    public final void notifyDestroyed()
    {
        this.activity.finish();
    }    

    public final void doDestroyApp( boolean unconditional )
    	throws MIDletStateChangeException
    {
    	this.destroyApp( unconditional );
    }

    public final void doStartApp()
		throws MIDletStateChangeException
	{
		this.startApp();
	}

    public final void doPauseApp()
		throws MIDletStateChangeException
	{
		this.pauseApp();
	}

    public boolean platformRequest( String url )
        throws ConnectionNotFoundException
    {
    	Uri content = Uri.parse( url );
    	String action;
    	if( url.startsWith( PROTOCOL_PHONE ) )
    	{
    		action = Intent.ACTION_DIAL;
    	}
    	else
    	{
    		action = Intent.ACTION_DEFAULT;
    	}
    	Intent intent = new Intent(  action, content );
    	this.getActivity().startActivity( intent );
    	return false;
    }
    
    public String getAppProperty( String key )
    {
        return this.applicationProperties.getProperty( key );
    }
    
}
