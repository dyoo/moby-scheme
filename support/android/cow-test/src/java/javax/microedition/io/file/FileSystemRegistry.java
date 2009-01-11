package javax.microedition.io.file;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

public class FileSystemRegistry {
	
	public static boolean addFileSystemListener( FileSystemListener listener ) {
		return false;
	}
	
	public static boolean removeFileSystemListener( FileSystemListener listener ) {
		return false;
	}
	
	public static Enumeration listRoots() {
		File[] roots = File.listRoots();
		Vector<String> rootsArray = new Vector<String>( roots.length );
		for( File root : roots ) {
			String path = root.getPath();
			if( path.startsWith( File.separator ) ) {
				path = path.substring( File.separator.length() );
			}
			// root paths appear to have some rubbish characters attached to them!
			StringBuffer cleaned = new StringBuffer( path.length() );
			for( int i=0; i<path.length(); i++ ) {
				char c = path.charAt( i );
				if( Character.isLetterOrDigit( c ) || Character.isWhitespace( c ) ) {
					cleaned.append( c );
				} else {
					// TODO log this as bogus!
					
				}
			}
			path = cleaned.toString();
			rootsArray.add( path );
		}
		return rootsArray.elements();
	}
}
