package javax.microedition.lcdui;

import java.io.IOException;
import java.io.InputStream;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Bitmap.Config;


public class Image 
{
    public static Image createImage( InputStream stream )
	    throws IOException
	{
	    Bitmap bitmap = BitmapFactory.decodeStream( stream );
	    return new Image( bitmap );
	}
	
	public static Image createImage( int width, int height )
	{
	    Bitmap bitmap = Bitmap.createBitmap( width, height, Config.ARGB_8888 );
	    return new Image( bitmap );
	}
	
	public static Image createImage( String resource )
	    throws IOException
	{
	    return createImage( Image.class.getResourceAsStream( resource ) );
	}
	
	public static Image createImage( byte[] imageData, int imageOffset, int imageLength )
	{
	    Bitmap bitmap = BitmapFactory.decodeByteArray( imageData, imageOffset, imageLength );
	    return new Image( bitmap );
	}
	
	public static final Image createRGBImage( int[] rgb, int width, int height, boolean processAlpha )
	{
		Bitmap.Config config;
		if( processAlpha )
		{
			config = Bitmap.Config.ARGB_4444;
		}
		else
		{
			config = Bitmap.Config.RGB_565;
		}
	    Bitmap bitmap = Bitmap.createBitmap( rgb, width, height, config );
	    return new Image( bitmap );
	}
	
	private Bitmap bitmap;
	
	protected Image( Bitmap bitmap )
	{
	    this.bitmap = bitmap;
	}
	
	public Bitmap getBitmap()
	{
	    return this.bitmap;
	}
	
	public int getWidth()
	{
	    return this.bitmap.getWidth();
	}
	
	public int getHeight()
	{
	    return this.bitmap.getHeight();
	}
	
	public Graphics getGraphics()
	{
	    return new Graphics( this.bitmap );
	}
	
	public void getRGB( int[] rgb, int offset, int scanlength, int x, int y, int width, int height )
	{
	    this.bitmap.getPixels( rgb, offset, scanlength, x, y, width, height );
	}
}
