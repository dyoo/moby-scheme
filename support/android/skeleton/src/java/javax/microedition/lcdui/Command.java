package javax.microedition.lcdui;

import android.view.Menu;
import android.view.MenuItem;

public class Command
{  
    public static final int BACK    = 1;
    public static final int CANCEL  = 2;
    public static final int EXIT    = 3;
    public static final int HELP    = 4;
    public static final int ITEM    = 5;
    public static final int OK      = 6;
    public static final int SCREEN  = 7;
    public static final int STOP    = 8;
    
    private int priority;
    private int commandType;
    private String label;
    private String longLabel;
    private MenuItem item;
    
    public Command( String label, int commandType, int priority ) {
    	this( label, null, commandType, priority );
    }
    
    public Command( String label, String longLabel, int commandType, int priority )
    {
        this.label = label;
        this.longLabel = longLabel;
        this.commandType = commandType;
        this.priority = priority;
    }
    
    public int getCommandType()
    {
        return this.commandType;
    }
    
    public String getLabel()
    {
        return this.label;
    }
    
    public int getPriority()
    {
        return this.priority;
    }

	public MenuItem getItem() 
	{
		return item;
	}

	public void setItem(MenuItem item) 
	{
		this.item = item;
	}
    
    
}
