package javax.microedition.io;

import j2ab.android.io.AndroidURLConnection;
import j2ab.android.io.file.AndroidFileConnection;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Connector
{
    public static final int READ        = 0x01;
    public static final int WRITE       = 0x02;
    public static final int READ_WRITE  = READ | WRITE;
    
    private static final String PROTOCOL_FILE = "file:";
    private static final String PROTOCOL_HTTP = "http:";
    
    public static final Connection open( String name )
		throws IOException
    {
        return open( name, READ_WRITE );
    }
    
    public static final Connection open( String name, int mode )
    	throws IOException
    {
    	Connection connection;
    	if( name.startsWith( PROTOCOL_FILE ) ) {
    		connection = new AndroidFileConnection( name );
    		// TODO : http should have a separate connection type
    	} else {
    		connection = new AndroidURLConnection( name );
    	}
    	return connection;
    }
    
    public static final DataInputStream openDataInputStream( String name ) 
    	throws IOException {
    	return new DataInputStream( openInputStream( name ) );
    }
    
    public static final DataOutputStream openDataOutputStream( String name ) 
    	throws IOException {
    	return new DataOutputStream( openOutputStream( name ) );
    }
    
    public static final InputStream openInputStream( String name ) 
    	throws IOException {
    	Connection connection = open( name, READ );
    	return ((InputConnection)connection).openInputStream();
    }
    
    public static final OutputStream openOutputStream( String name ) 
    	throws IOException {
    	Connection connection = open( name, WRITE );
    	return ((OutputConnection)connection).openOutputStream();
    }
}
