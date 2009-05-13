package javax.microedition.lcdui;

import javax.microedition.midlet.MIDlet;

import android.content.Context;
import android.graphics.Bitmap;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnKeyListener;

public abstract class Canvas extends Displayable implements OnKeyListener
{

    public static final int FIRE        = KeyEvent.KEYCODE_DPAD_CENTER;
    public static final int GAME_A      = KeyEvent.KEYCODE_POUND;
    public static final int GAME_B      = KeyEvent.KEYCODE_STAR;
    public static final int GAME_C      = KeyEvent.KEYCODE_1;
    public static final int GAME_D      = KeyEvent.KEYCODE_3;

    public static final int LEFT        = KeyEvent.KEYCODE_DPAD_LEFT;
    public static final int RIGHT       = KeyEvent.KEYCODE_DPAD_RIGHT;
    public static final int UP          = KeyEvent.KEYCODE_DPAD_UP;
    public static final int DOWN        = KeyEvent.KEYCODE_DPAD_DOWN;
    
    public static final int KEY_NUM0    = KeyEvent.KEYCODE_0;
    public static final int KEY_NUM1    = KeyEvent.KEYCODE_1;
    public static final int KEY_NUM2    = KeyEvent.KEYCODE_2;
    public static final int KEY_NUM3    = KeyEvent.KEYCODE_3;
    public static final int KEY_NUM4    = KeyEvent.KEYCODE_4;
    public static final int KEY_NUM5    = KeyEvent.KEYCODE_5;
    public static final int KEY_NUM6    = KeyEvent.KEYCODE_6;
    public static final int KEY_NUM7    = KeyEvent.KEYCODE_7;
    public static final int KEY_NUM8    = KeyEvent.KEYCODE_8;
    public static final int KEY_NUM9    = KeyEvent.KEYCODE_9;
    public static final int KEY_POUND   = KeyEvent.KEYCODE_POUND;
    public static final int KEY_STAR    = KeyEvent.KEYCODE_STAR;
 
    private CanvasView canvasView;
//    private Handler handler;
//    private MIDlet midlet;
    
//    private com.kannuu.android.lcdui.Graphics graphics;
    
    protected Canvas()
    {
    }
    
    public void setFullScreenMode( boolean fullScreen ) {
    	// do nothing, is this possible on android?
    }
    
    public int getGameAction( int keyCode )
    {
        return keyCode;
    }
    
    public int getKeyCode( int gameAction )
    {
        return gameAction;
    }
    
    public void repaint( int x, int y, int w, int h ) {
    	if( this.canvasView != null ) {
    		this.canvasView.postInvalidate( x, y, x+w, y + h );
    		//this.canvasView.postInvalidate();
    	}
    }
    
    public void repaint()
    {
    	if( this.canvasView != null )
    	{
    		this.canvasView.postInvalidate();
    	}
//        if( this.handler != null && this.canvasView != null )
//        {
//            this.handler.post( this );
//        }
    }
    
//    public void run()
//    {
//    	if( this.graphics == null )
//    	{
//            createGraphics();
//    	}
//        paint( this.graphics );
//        this.canvasView.invalidate();
//    }
    
    protected void keyPressed( int keyCode )
    {
        System.out.println( keyCode );
        this.repaint();
    }
    
    protected void keyReleased( int keyCode )
    {
        
    }
    
    protected void keyRepeated( int keyCode )
    {
        
    }
    
    protected abstract void paint( javax.microedition.lcdui.Graphics g );
    
    public boolean hasPointerEvents()
    {
    	return true;
    }
    
    
    
    protected void pointerPressed( int x, int y )
    {
    	
    }
    
    protected void pointerReleased( int x, int y )
    {
    	
    }

    @Override
    public View getView()
    {
        return this.canvasView;
    }
  
    @Override
    public void disposeDisplayable()
    {
        this.canvasView.setOnKeyListener( null );
        this.canvasView = null;
//        this.handler = null;
//        this.graphics = null;
    }

    @Override
    public void initDisplayable( MIDlet midlet )
    {
        this.canvasView = new CanvasView( midlet.getActivity() );
        this.canvasView.setOnKeyListener( this );
//        this.midlet = midlet;
    }
    
//    private void createGraphics()
//    {
//    	createGraphics( this.getWidth(), this.getHeight() );
//    }
//    
//    private void createGraphics( int w, int h )
//    {
//    	Bitmap bitmap;
//    	com.kannuu.android.lcdui.Graphics graphics;
//    	if( w > 0 && h > 0 )
//    	{
//	        bitmap = Bitmap.createBitmap( w, h, false );
//	        graphics = new com.kannuu.android.lcdui.Graphics(
//	                new android.graphics.Canvas( bitmap )
//	        );
//	        this.graphics = graphics;
//    	}
//    	else
//    	{
//    		graphics = null;
//    		bitmap = null;
//    	}
//        this.canvasView.setBitmap( bitmap );
//		this.graphics = graphics;
//    }

    
// TODO : this is causing Eclipse to show an error, but it's right!!    
//    @Override
    public boolean onKey( View view, int keyCode, KeyEvent event )
    {
    	if( view == this.canvasView )
        {
            if( event.getAction() == KeyEvent.ACTION_DOWN )
            {
                if( event.getRepeatCount() == 0 )
                {
                    this.keyPressed( keyCode );
                }
                else
                {
                    this.keyRepeated( keyCode );
                }
            }
            else if( event.getAction() == KeyEvent.ACTION_UP )
            {
                this.keyReleased( keyCode );
            }
        }
        return false;
    }
    
    private class CanvasView extends View
    {
    	private javax.microedition.lcdui.Graphics graphics;
//    	private Bitmap bitmap;
    	
    	public CanvasView( Context context )
    	{
    		super( context );
    		this.setFocusable( true );
    	}
    	
//		public Bitmap getBitmap() 
//		{
//			return bitmap;
//		}
//
//		public void setBitmap(Bitmap bitmap) 
//		{
//			this.bitmap = bitmap;
//		}

		@Override
		protected void onDraw(android.graphics.Canvas canvas) 
		{
//			super.onDraw(canvas);
//			if( this.bitmap != null )
//			{
//				canvas.drawBitmap( this.bitmap, 0, 0, null );
//			}
			javax.microedition.lcdui.Graphics graphics;
			if( this.graphics == null )
			{
				Bitmap bitmap = Bitmap.createBitmap(
									canvas.getWidth(), 
									canvas.getHeight(), 
									Bitmap.Config.ARGB_8888
								);
				graphics = new javax.microedition.lcdui.Graphics( bitmap );
				this.graphics = graphics;
			}
			else
			{
				graphics = this.graphics;
			}
//			graphics.setColor( 0xFFFFFF );
//			graphics.fillRect( 0, 0, getWidth(), getHeight() );
//			graphics.setColor( 0x000000 );
			Canvas.this.paint( graphics );
			canvas.drawBitmap( graphics.getBitmap(), 0, 0, null );
			
		}
    	
        @Override
        protected void onSizeChanged(int w, int h, int oldw, int oldh) 
        {
        	invalidate();
        }

		@Override
		public boolean onKeyDown(int keyCode, KeyEvent event) 
		{
			return super.onKeyDown(keyCode, event);
		}

		@Override
		public boolean onKeyUp( int keyCode, KeyEvent event ) 
		{
			return super.onKeyUp(keyCode, event);
		}

		@Override
		public boolean onTouchEvent( MotionEvent event )
		{
			//System.out.println( "("+(int)event.getX()+","+(int)event.getY()+","+event.getAction()+")" );
			switch( event.getAction() )
			{
			case MotionEvent.ACTION_DOWN:
				Canvas.this.pointerPressed( 
						Math.round( event.getX() ), 
						Math.round( event.getY() ) 
				);
				break;
			case MotionEvent.ACTION_UP:
				Canvas.this.pointerReleased( 
						Math.round( event.getX() ), 
						Math.round( event.getY() ) 
				);
				break;
			}
			System.out.println( this.isFocused() );
			return true;
		}        
		
    }

    protected void hideNotify()
	{
		
	}
}
