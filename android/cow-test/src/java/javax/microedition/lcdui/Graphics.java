package javax.microedition.lcdui;

import android.graphics.Bitmap;
import android.graphics.Paint;
import android.graphics.RectF;
import android.graphics.Paint.Align;
import android.graphics.Paint.Style;

public class Graphics
{
    public static final int BASELINE        = 0x01;
    public static final int BOTTOM          = 0x02;
    public static final int LEFT            = 0x04;
    public static final int RIGHT           = 0x08;
    public static final int TOP             = 0x10;
    public static final int VCENTER         = 0x20;
    public static final int HCENTER         = 0x40;
    
    public static final int DOTTED          = 0x01;
    public static final int SOLID           = 0x02;
    
    private android.graphics.Canvas canvas;
    private Bitmap bitmap;
    private javax.microedition.lcdui.Font font;
    private Paint paint;
    
    private int tx, ty;
    
    public Graphics( Bitmap bitmap )
    {
    	this( new android.graphics.Canvas( bitmap ), bitmap );
    }
    
    public Graphics( android.graphics.Canvas canvas, Bitmap bitmap )
    {
        setFont( Font.getDefaultFont() );
        this.canvas = canvas;
        this.bitmap = bitmap;
        canvas.save();
    }
    
    public Bitmap getBitmap()
    {
    	return this.bitmap;
    }
    
    public android.graphics.Canvas getCanvas()
    {
    	return this.canvas;
    }
    
    public int getClipX() {
    	return this.canvas.getClipBounds().left;
    }
    
    public int getClipY() {
    	return this.canvas.getClipBounds().top;
    }
    
    public int getClipWidth() {
    	return this.canvas.getClipBounds().width();
    }
    
    public int getClipHeight() {
    	return this.canvas.getClipBounds().height();
    }
    
    public int getColor()
    {
        return this.paint.getColor() & 0x00FFFFFF;
    }
    
    public void setColor( int color )
    {
        this.paint.setColor( 0xFF000000 | color );
    }

    public void setColor(int r, int g, int b) {
	// dyoo: implemented
	this.paint.setARGB(255, r, g, b);
    }
    
    public void fillRect( int x, int y, int width, int height )
    {
        this.canvas.drawRect( x, y, x+width, y+height, this.paint );
    }
    
    public void fillRoundRect( int x, int y, int width, int height, int rx, int ry ) {
    	this.canvas.drawRoundRect( new RectF( x, y, x+width, y+height ), rx, ry, this.paint );
    }
    
    public void drawArc(int x, int y, int width, int height, 
			int startAngle, int endAngle) {
	// dyoo: implemented
	this.canvas.drawArc(new RectF(x, y, x+width, y+height),
			    startAngle, endAngle, true, this.paint);
    }

    public void drawImage( javax.microedition.lcdui.Image image, int x, int y, int anchor )
    {
        int ax;
        int ay;
        if( ( anchor & LEFT ) != 0 )
        {
            ax = x;
        }
        else if( ( anchor & HCENTER ) != 0 )
        {
            ax = x - image.getWidth()/2;
        }
        else
        {
            ax = x - image.getWidth();
        }
        if( ( anchor & TOP ) != 0 )
        {
            ay = y;
        }
        else if( ( anchor & VCENTER ) != 0 )
        {
            ay = y - image.getHeight()/2;
        }
        else
        {
            ay = y - image.getHeight();
        }
        this.canvas.drawBitmap( image.getBitmap(), ax, ay, null );
    }
    
    public void drawLine( int x1, int y1, int x2, int y2 )
    {
        this.canvas.drawLine( x1, y1, x2, y2, this.paint );
    }
    
    public void drawRect( int x, int y, int width, int height )
    {
    	Paint outlinePaint = new Paint( this.paint );
    	outlinePaint.setStyle( Style.STROKE );
        this.canvas.drawRect( x, y, x+width, y+height, outlinePaint );
    }
    
    public void drawRoundRect( int x, int y, int width, int height, int rx, int ry ) {
    	Paint outlinePaint = new Paint( this.paint );
    	outlinePaint.setStyle( Style.STROKE );
        this.canvas.drawRoundRect( new RectF( x, y, x+width, y+height), rx, ry, outlinePaint );
    }
    
    public javax.microedition.lcdui.Font getFont()
    {
        return this.font;
    }
    
    public void setFont( javax.microedition.lcdui.Font font )
    {
        Paint typefacePaint = font.getTypefacePaint();
        if( this.paint != null )
        {
            this.paint.setTypeface( typefacePaint.getTypeface() );
            this.paint.setUnderlineText( typefacePaint.isUnderlineText() );
        }
        else
        {
            this.paint = new Paint( typefacePaint );
        }
        this.font = font;
    }
    
    public void drawString( String str, int x, int y, int anchor )
    {
        Align align;
        if( ( anchor & HCENTER ) != 0 )
        {
            align = Align.CENTER;
        }
        else if( ( anchor & RIGHT ) != 0 )
        {
            align = Align.RIGHT;
        }
        else
        {
            align = Align.LEFT;
        }
        
        this.paint.setTextAlign( align );
        this.canvas.save();
        float scale = this.font.getScale();
        this.canvas.scale( scale, scale );
        this.canvas.drawText( str, x/scale, y/scale, this.paint );
        this.canvas.restore();
    }
    
    public void clipRect( int x, int y, int w, int h ){
    	this.canvas.clipRect( x, y, x+w, y+h );
    }
    
    public void setClip( int x, int y, int w, int h )
    {
    	this.canvas.restore();
    	this.canvas.save();
    	this.canvas.translate( this.tx, this.ty );
        this.canvas.clipRect( x, y, x+w, y+h );
        
    }
    
    public void fillArc( int x, int y, int width, int height, int startAngle, int arcAngle )
    {
        // TODO : do something to the paint to make it fill!!
        this.canvas.drawArc( new RectF( x, y, x+width, y+height ), startAngle, arcAngle, true, this.paint );
    }
    
    
    public void translate( int x, int y )
    {
    	this.tx += x;
    	this.ty += y;
    	this.canvas.translate( x, y );
    }

    public int getTranslateX() {
    	return this.tx;
    }
    
    public int getTranslateY() {
    	return this.ty;
    }
}
