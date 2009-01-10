package javax.microedition.lcdui;

import android.graphics.Paint;
import android.graphics.Typeface;
import android.graphics.Paint.FontMetricsInt;

public class Font 
{
    public static final int FACE_SYSTEM             = 0;
    public static final int FACE_MONOSPACE          = 32;
    public static final int FACE_PROPORTIONAL       = 64;
    
    public static final int FONT_STATIC_TEXT        = 0;
    public static final int FONT_INPUT_TEXT         = 1;
    
    public static final int SIZE_MEDIUM             = 0;
    public static final int SIZE_SMALL              = 8;
    public static final int SIZE_LARGE              = 16;
    
    public static final int STYLE_PLAIN             = 0;
    public static final int STYLE_BOLD              = 1;
    public static final int STYLE_ITALIC            = 2;
    public static final int STYLE_UNDERLINED        = 4;
    
    public static Font getFont( int fontSpecifier )
    {
        // TODO : this isn't actually tied to anything
        return new Font( Typeface.DEFAULT, SIZE_MEDIUM );
    }
    
    public static Font getDefaultFont()
    {
        return new Font( Typeface.DEFAULT, SIZE_MEDIUM );
    }
    
    public static Font getFont( int face, int style, int size )
    {
    	Font font = new Font( size );
    	return getFont( font, face, style, size );
    }
       
    protected static Font getFont( Font font, int face, int style, int size )
    {
        int paintFlags = 0;
        int typefaceStyle = Typeface.NORMAL;
        Typeface base;
        switch( face )
        {
        case FACE_MONOSPACE:
            base = Typeface.MONOSPACE;
            break;
        case FACE_SYSTEM:
            base = Typeface.DEFAULT;
            break;
        case FACE_PROPORTIONAL:
            base = Typeface.SANS_SERIF;
            break;
        default:
            throw new IllegalArgumentException( "unknown face "+face );
        }
        if( ( style & STYLE_BOLD ) != 0 )
        {
            typefaceStyle |= Typeface.BOLD;
        }
        if( ( style & STYLE_ITALIC ) != 0 )
        {
            typefaceStyle |= Typeface.ITALIC;
        }
        if( ( style & STYLE_UNDERLINED ) != 0 )
        {
            paintFlags |= Paint.UNDERLINE_TEXT_FLAG;
        }
        Typeface typeface = Typeface.create( base, typefaceStyle );
        Paint paint = new Paint( paintFlags );        
        
        paint.setTypeface( typeface );
        font.setTypefacePaint( paint );
        return font;
    }    
 
    private static Paint createPaint( Typeface typeface )
    {
        Paint paint = new Paint();
        paint.setTypeface( typeface );
        return paint;
    }
    
    private Paint typefacePaint;
    private FontMetricsInt fontMetrics;
    private int size;
    
    protected Font( int size )
    {
    	this.size = size;
    }
    
    protected Font( Typeface typeface, int size )
    {
        this( createPaint( typeface ), size ); 
    }
    
    protected Font( Paint typefacePaint, int size )
    {
        setTypefacePaint( typefacePaint );
        this.size = size;
    }

    public float getScale()
    {
    	int size = this.getSize();
    	float scale;
    	if( size == Font.SIZE_LARGE )
    	{
    		scale = 1.5F;
    	}
    	else if( size == Font.SIZE_SMALL )
    	{
    		scale = 0.8F;
    	}
    	else
    	{
    		scale = 1;
    	}
    	return scale;
    }
    

    public Paint getTypefacePaint()
    {
        return this.typefacePaint;
    }
    
    public void setTypefacePaint( Paint typefacePaint )
    {
        this.typefacePaint = typefacePaint;
        this.fontMetrics = typefacePaint.getFontMetricsInt();
    }
    
    public int charWidth( char[] ch, int offset, int length )
    {
        return Math.round( this.typefacePaint.measureText( ch, offset, length ) * this.getScale() );
    }
    
    public int charWidth( char ch )
    {
        return this.charWidth( new char[]{ ch }, 0, 1 );
    }
    
    public int getBaselinePosition()
    {
        return Math.round( -this.typefacePaint.ascent() * this.getScale() );
    }
    
    public int getFace()
    {
        // TODO: work out the face
        return FACE_SYSTEM;
    }
    
    public int getHeight()
    {
        return Math.round( this.typefacePaint.getTextSize() * this.getScale() );
    }
    
    public int getStyle()
    {
        int style = STYLE_PLAIN;
        Typeface typeface = this.typefacePaint.getTypeface();
        if( typeface.isBold() )
        {
            style |= STYLE_BOLD;
        }
        if( typeface.isItalic() )
        {
            style |= STYLE_ITALIC;
        }
        if( this.typefacePaint.isUnderlineText() )
        {
            style |= STYLE_UNDERLINED;
        }
        return style;
    }
    
    public boolean isBold()
    {
        return this.typefacePaint.getTypeface().isBold();
    }
    
    public boolean isItalic()
    {
        return this.typefacePaint.getTypeface().isItalic();
    }
    
    public boolean isPlain()
    {
        return this.getStyle() == STYLE_PLAIN;
    }
    
    public int getSize()
    {
        return this.size;
    }
    
    public boolean isUnderlined()
    {
        return this.typefacePaint.isUnderlineText();
    }
    
    public int stringWidth( String str )
    {
        return Math.round( this.typefacePaint.measureText( str ) * this.getScale() );
    }
    
    public int substringWidth( String str, int offset, int len )
    {
        return Math.round( this.typefacePaint.measureText( str, offset, len ) * this.getScale() );
    }

}
