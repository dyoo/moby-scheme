package javax.microedition.media;

import java.io.IOException;

public class Manager 
{
	public static final String TONE_DEVICE_LOCATOR = "tone_device";
	
	public static Player createPlayer( java.io.InputStream ins, String type ) throws IOException, MediaException
	{
		return null;
	}
	
	public static Player createPlayer( String locator ) throws IOException, MediaException
	{
		Player player;
		if( TONE_DEVICE_LOCATOR.equals( locator ) )
		{
			player = new TonePlayer();
		}
		else
		{
			player = null;
		}
		return player;
	}
	
	public static void playTone( int note, int duration, int volume ) throws MediaException
	{
		// do nothing
	}
	
	public static String[] getSupportedContentTypes( String protocol ) 
	{
		return new String[ 0 ];
	}
	
	public String[] getSupportedProtocols( String content_type )
	{
		return new String[ 0 ];
	}
}
