package j2ab.android.lcdui;

import android.os.Handler;
import android.view.View;

public interface Toolkit {
	Handler getHandler();
	
	void invokeAndWait( Runnable r );
	
	int getResourceId( String resourceName );
	
	View inflate( int resourceId );
	
	int getScreenWidth();
	
	int getScreenHeight();
}	
