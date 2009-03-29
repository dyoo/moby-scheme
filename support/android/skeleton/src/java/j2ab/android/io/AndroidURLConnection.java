package j2ab.android.io;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import javax.microedition.io.Connection;
import javax.microedition.io.InputConnection;
import javax.microedition.io.OutputConnection;
import javax.microedition.io.StreamConnection;

public class AndroidURLConnection implements StreamConnection,
		OutputConnection, InputConnection, Connection {

	protected URLConnection connection;
	
	public AndroidURLConnection( String url ) 
		throws MalformedURLException, IOException{
		this( new URL( url ) );
	}
	
	public AndroidURLConnection( URL url ) 
		throws IOException {
		this( url.openConnection() );
	}
	
	public AndroidURLConnection( URLConnection connection ) {
		
	}
	
	
	public DataInputStream openDataInputStream() throws IOException {
		return new DataInputStream( this.openInputStream() );
	}

	
	public InputStream openInputStream() throws IOException {
		return this.connection.getInputStream();
	}

	
	public void close() throws IOException {
		this.connection = null;
	}

	
	public DataOutputStream openDataOutputStream() throws IOException {
		return new DataOutputStream( openOutputStream() );
	}

	
	public OutputStream openOutputStream() throws IOException {
		return this.connection.getOutputStream();
	}

}
