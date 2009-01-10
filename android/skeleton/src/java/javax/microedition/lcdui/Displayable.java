package javax.microedition.lcdui;

import java.util.Vector;

import javax.microedition.midlet.MIDlet;

import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.MenuItem.OnMenuItemClickListener;

public abstract class Displayable 
{
    protected CommandListener commandListener;
    private Vector<Command> commands;
    private Display currentDisplay;
    
    private int menuItemIds;
    
    public Displayable()
    {
        this.commands = new Vector<Command>();
    }
    
    public void addCommand( Command command )
    {
    	boolean added = false;
    	for( int i=0; i<this.commands.size(); i++ )
    	{
    		Command found = this.commands.elementAt( i );
    		if( found.getPriority() > command.getPriority() )
    		{
    			this.commands.insertElementAt( command, i );
    			added = true;
    			break;
    		}
    	}
    	if( !added )
    	{
    		this.commands.addElement( command );
    	}
        
        if( this.currentDisplay != null )
        {
        	addCommandToDisplay( command, this.currentDisplay );
        }
        
    }
    
    public void removeCommand( Command command )
    {
        this.commands.removeElement( command );
        if( this.currentDisplay != null )
        {
        	removeCommandFromDisplay( command, this.currentDisplay );
        }
    }
    
    public Vector<Command> getCommands()
    {
    	return this.commands;
    }
    
    public CommandListener getCommandListener() 
    {
		return this.commandListener;
	}

	public void setCommandListener( CommandListener commandListener )
    {
        this.commandListener = commandListener;
    }
    
    public int getWidth()
    {
    	View view = this.getView();
    	view = null;
    	if( view == null )
    	{
    		return MIDlet.DEFAULT_MIDLET.getToolkit().getScreenWidth();
    	}
    	else
    	{
    		return view.getWidth();
    	}
    }
    
    public int getHeight()
    {
    	View view = this.getView();
    	view = null;
    	if( view == null )
    	{
    		return MIDlet.DEFAULT_MIDLET.getToolkit().getScreenHeight();
    	}
    	else
    	{
    		return view.getHeight();
    	}
    }
    
    public abstract void initDisplayable( MIDlet midlet );

    public abstract void disposeDisplayable();
    
    public abstract View getView();

	public Display getCurrentDisplay() 
	{
		return currentDisplay;
	}
	
	public void setCurrentDisplay(Display currentDisplay) 
	{
		if( this.currentDisplay != null )
		{
			removeCommandsFromDisplay( this.currentDisplay );
		}
		this.currentDisplay = currentDisplay;
		if( currentDisplay != null )
		{
			addCommandsToDisplay( currentDisplay );
		}
	}
	
	private void removeCommandsFromDisplay( Display display )
	{
		for( Command command : this.commands )
		{
			removeCommandFromDisplay( command, display );
		}
				
	}
	
	private void removeCommandFromDisplay( Command command, Display display )
	{
		Menu menu = display.getMIDlet().getMenu();
		if( menu != null )
		{
			android.view.MenuItem item;
			item = command.getItem();
			menu.removeItem( item.getItemId() );
//			for( int i=0; i<menu.size(); i++ )
//			{
//				android.view.MenuItem found = menu.get( i );
//				if( 
//						found != null &&
//						item != null &&
//						found.getId() == item.getId() && 
//						item.getTitle() != null && 
//						item.getTitle().equals( found.getTitle() ) 
//				)
//				{
//					menu.removeItemAt( i );
//					break;
//				}
//			}
		}		
	}
	
	
	public void addCommandsToDisplay( Display display )
	{
		for( Command command : this.commands )
		{
			addCommandToDisplay( command, display );
		}
	}
	
	private void addCommandToDisplay( final Command command, final Display display )
	{
		final Menu menu = display.getMIDlet().getMenu();
		if( menu != null )
		{
			display.getMIDlet().getHandler().post( new Runnable(){
				public void run()
				{
					android.view.MenuItem item;
					int menuItemId = Displayable.this.menuItemIds++;
					item = menu.add( 
							Menu.NONE,
							menuItemId, 
							command.getPriority(), 
							command.getLabel() 
					);
					item.setOnMenuItemClickListener(
							new CallbackOnMenuItemClickListener( command )
					);
					command.setItem( item );		
					
					if( command.getCommandType() == Command.BACK )
					{
						// TODO : do something here
						//item.setAlphabeticShortcut( 0, KeyEvent.KEYCODE_BACK );
					}
				}
			});
		}
	}
    
    private class CallbackOnMenuItemClickListener implements OnMenuItemClickListener
    {
    	private Command source;
    	public CallbackOnMenuItemClickListener( Command source )
    	{
    		this.source = source;
    	}
		public boolean onMenuItemClick(MenuItem item) {
    		boolean result;
    		if( Displayable.this.commandListener != null )
    		{
    			Displayable.this.commandListener.commandAction( 
    					this.source, Displayable.this 
    			);
    			result = true;
    		} else {
    			result = false;
    		}
    		return result;
		}

    }
}
