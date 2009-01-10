package javax.microedition.lcdui;

import javax.microedition.midlet.MIDlet;

import android.view.View;
import android.view.ViewGroup;

public abstract class Item
{
    public static final int PLAIN                   = 0;
    public static final int BUTTON                  = 1;
    public static final int HYPERLINK               = 2;
    
    public static final int LAYOUT_2                = 0x0001;
    public static final int LAYOUT_BOTTOM           = 0x0002;
    public static final int LAYOUT_CENTER           = 0x0004;
    public static final int LAYOUT_DEFAULT          = 0x0008;
    public static final int LAYOUT_EXPAND           = 0x0010;
    public static final int LAYOUT_LEFT             = 0x0020;
    public static final int LAYOUT_NEWLINE_AFTER    = 0x0040;
    public static final int LAYOUT_NEWLINE_BEFORE   = 0x0080;
    public static final int LAYOUT_RIGHT            = 0x0100;
    public static final int LAYOUT_SHRINK           = 0x0200;
    public static final int LAYOUT_TOP              = 0x0400;
    public static final int LAYOUT_VCENTER          = 0x0800;
    public static final int LAYOUT_VEXPAND          = 0x1000;
    public static final int LAYOUT_VSHRINK          = 0x2000;
    
    private int layout;
    private String label;
    
    protected Item()
    {
        
    }
    
    protected Item( String label )
    {
        this.label = label;
    }
    
    public String getLabel()
    {
        return this.label;
    }
    
    public void setLabel( String label )
    {
        this.label = label;
    }
    
    public int getLayout()
    {
        return this.layout;
    }

    public void setLayout( int layout )
    {
        this.layout = layout;
    }
    
    public abstract void init( MIDlet midlet, ViewGroup parent );
    
    public abstract void dispose();
    
    public abstract View getView();
}
