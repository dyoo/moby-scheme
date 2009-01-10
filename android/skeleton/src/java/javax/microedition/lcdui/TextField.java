package javax.microedition.lcdui;

import javax.microedition.midlet.MIDlet;

import android.content.Context;
import android.text.method.ArrowKeyMovementMethod;
//import android.text.method.DialerInputMethod;
//import android.text.method.DigitsInputMethod;
//import android.text.method.InputMethod;
import android.text.method.MovementMethod;
import android.text.method.PasswordTransformationMethod;
import android.text.method.SingleLineTransformationMethod;
//import android.text.method.TextInputMethod;
import android.text.method.TransformationMethod;
//import android.text.method.TextInputMethod.Capitalize;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.TwoLineListItem;

public class TextField extends Item implements Runnable
{
    public static final int ANY             = 0x000;
    public static final int DECIMAL         = 0x001;
    public static final int EMAILADDR       = 0x002;
    public static final int INITIAL_CAPS_SENTENCE = 0x004;
    public static final int INITIAL_CAPS_WORD = 0x008;
    public static final int NON_PREDICTIVE  = 0x010;
    public static final int NUMERIC         = 0x020;
    public static final int PASSWORD        = 0x040;
    public static final int PHONENUMBER     = 0x080;
    public static final int SENSITIVE       = 0x100;
    public static final int URL             = 0x200;
    public static final int UNEDITABLE      = 0x400;
    
    public static TextView createTextView( int constraints, Context context )
    {
        boolean isEditable = (constraints & TextField.UNEDITABLE ) != 0;
        TextView textView;
        if( isEditable )
        {
            MovementMethod movementMethod = new ArrowKeyMovementMethod();
            TransformationMethod transformationMethod;
// input method appears to have been scrapped?            
//            InputMethod inputMethod;
//            if( ( constraints & TextField.NUMERIC ) > 0 )
//            {
//                inputMethod = new DigitsInputMethod();
//            }
//            else if( ( constraints & TextField.PHONENUMBER ) > 0 )
//            {
//                inputMethod = new DialerInputMethod();
//            }
//            else
//            {
//                Capitalize capitalize;
//                // assume text
//                if( ( constraints & TextField.INITIAL_CAPS_SENTENCE ) > 0 )
//                {
//                    capitalize = Capitalize.SENTENCES;
//                }
//                else if( ( constraints & TextField.INITIAL_CAPS_WORD ) > 0 )
//                {
//                    capitalize = Capitalize.WORDS;
//                }
//                else
//                {
//                    capitalize = Capitalize.NONE;
//                }
//                boolean autotext = ( constraints & TextField.NON_PREDICTIVE ) == 0;
//                inputMethod = new TextInputMethod( capitalize, autotext );
//            }
            
            if( ( constraints & TextField.PASSWORD ) > 0 )
            {
                transformationMethod = new PasswordTransformationMethod();
            }
            else
            {
                transformationMethod = new SingleLineTransformationMethod();
            }
            
            EditText editText = new EditText( context );
            editText.setMovementMethod( movementMethod );
            //editText.setInputMethod( inputMethod );
            editText.setTransformationMethod( transformationMethod );
            
            textView = editText;
        }
        else
        {
            // ignore the other constraints
            textView = new TextView( context );
        }
        return textView;
    }
    
    private String label;
    private String text;
    private int constraints;
    private int maxSize;
    
    private TwoLineListItem view;
    private TextView labelView;
    private TextView textView;
    
    private MIDlet midlet;
    
    public TextField( String label, String text, int maxSize, int constraints )
    {
        this.label = label;
        this.text = text;
        this.maxSize = maxSize;
        this.constraints = constraints;
    }
    
    @Override
    public void dispose()
    {
        this.text = this.textView.getText().toString();
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
    	Context context = midlet.getActivity();
        TwoLineListItem view = new TwoLineListItem( context );
        TextView labelView = new TextView( context );
        TextView textView = createTextView( this.constraints, context );
        
        labelView.setText( this.label );
        textView.setText( this.text );
        
        view.addView( labelView );
        view.addView( textView );
        
        this.view = view;
        this.labelView = labelView;
        this.textView = textView;

    }

    public String getLabel()
    {
        return this.label;
    }
    
    public void setLabel( String label )
    {
        this.label = label;
        if( this.labelView != null )
        {
        	this.midlet.getHandler().post( this );
        }
    }
    
    public String getString()
    {
        String text;
        if( this.textView != null )
        {
            text = this.textView.getText().toString();
        }
        else
        {
            text = this.text;
        }
        return text;
    }
    
    public void setString( String text )
    {
        this.text = text;
        if( this.textView != null )
        {
            this.textView.setText( text );
        }
    }
    
    public int getMaxSize()
    {
        return this.maxSize;
    }
    
    public void setMaxSize( int maxSize )
    {
        this.maxSize = maxSize;
    }
    
    public int getConstraints()
    {
        return this.constraints;
    }
    
    public void setConstraints( int constraints )
    {
        this.constraints = constraints;
    }

//	@Override
	public void run() 
	{
        this.labelView.setText( label );		
	}
    
    
}
