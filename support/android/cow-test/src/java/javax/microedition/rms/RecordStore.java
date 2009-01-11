package javax.microedition.rms;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import javax.microedition.midlet.MIDlet;

public class RecordStore 
{
	private static final int DEFAULT_MAX_RECORDSTORE_SIZE = 20 * 1024;
	
    // TODO : we need to postfix the application name/id to this somehow
    private static String DATA_DIR = "_default";
    private static final String DATA_EXT = ".dat";
    
    private static Set<String> OPEN_RECORD_STORES = new HashSet<String>();
    
    public static void setApplicationName( String name )
    {
        DATA_DIR = name;
    }
    
    public static void deleteRecordStore( final String recordStoreName )
        throws RecordStoreException
    {
        File file = getRecordStoreDirectory( recordStoreName );
        if( !file.exists() )
        {
        	throw new RecordStoreNotFoundException( recordStoreName );
        }
        else
        {
        	File directory = getRecordStoreDirectory( recordStoreName );
        	File[] files = directory.listFiles( new RecordStoreFilenameFilter( recordStoreName ) );
        	for( int i=0; i<files.length; i++ )
        	{
        		File found = files[ i ];
        		found.delete();
        	}
        }
    }
        
    public static RecordStore openRecordStore( String recordStoreName, boolean createIfNecessary )
        throws RecordStoreException
    {
        File file = getRecordStoreDirectory( recordStoreName );
        if( !file.exists() )
        {
            if( createIfNecessary )
            {
                if( !file.mkdirs() )
                {
                	throw new RecordStoreException( "couldn't create directory" );
                }
            }
            else
            {
                throw new RecordStoreNotFoundException( recordStoreName );
            }
        }
        else
        {
        	File recordStorePlaceHolder = new File( file, recordStoreName+DATA_EXT );
        	if( !recordStorePlaceHolder.exists() )
        	{
        		if( createIfNecessary )
        		{
        			try
        			{
	        			if( !recordStorePlaceHolder.createNewFile() )
	        			{
	        				throw new RecordStoreException( "couldn't create record store file "+recordStorePlaceHolder.getPath() );
	        			}
        			}
        			catch( Exception ex )
        			{
        				throw new RecordStoreException( "couldn't create record store file "+recordStorePlaceHolder, ex );
        			}
        		}
        		else
        		{
        			throw new RecordStoreNotFoundException( "no record store "+ recordStoreName );
        		}
        	}
        }
        RecordStore result = new RecordStore( file, recordStoreName );
        OPEN_RECORD_STORES.add( recordStoreName );
        return result;
    }
    
    private static final File getRecordStoreDirectory( String recordStoreName )
    	throws RecordStoreException
    {
    	try
    	{
    		File parent = MIDlet.DEFAULT_MIDLET.getActivity().getFileStreamPath( DATA_DIR );
    		File result = parent;
    		return result;
    	}
    	catch( Exception ex )
    	{
    		throw new RecordStoreException( "couldn't create dir " + DATA_DIR, ex );
    	}
        //return new File( DATA_DIR + recordStoreName );
    }
    
    private static final String getRecordStoreName( String fileName ) {
    	// remove the dat extension
    	if( fileName.endsWith( DATA_EXT ) ) {
    		fileName = fileName.substring( 0, fileName.length() - DATA_EXT.length() );
    	}
    	// remove any digits
    	while( fileName.length() > 0 && Character.isDigit( fileName.charAt( fileName.length() - 1 ) ) ) {
    		fileName = fileName.substring( 0, fileName.length() - 1 );
    	}
    	return fileName;
    }
    
    public static final String[] listRecordStores() {
    	try {
    		File recordStoreDirectory = getRecordStoreDirectory( null );
    		// ugh, this isn't going to be pretty
    		HashSet<String> names = new HashSet<String>();
    		String[] records = recordStoreDirectory.list();
    		if( records != null ) {
	    		for( int i=0; i<records.length; i++ ) {
	        		String recordName = getRecordStoreName( records[i] );
	        		names.add( recordName );
	    		}
    		}
    		String[] result = new String[ names.size() ];
    		names.toArray( result );
    		return result;
    	} catch( Exception ex ) {
    		throw new RuntimeException( "unable to get record stores", ex );
    	}
    	
    }
    
    private File dataDirectory;
    private String name;
    
    private RecordStore( File dataDirectory, String name )
    {
        this.dataDirectory = dataDirectory;
        this.name = name;
    }
    
    public int addRecord( byte[] data, int offset, int numBytes )
        throws RecordStoreException
    {
    	if( !this.dataDirectory.exists() )
    	{
    		throw new RecordStoreException( "no data directory "+this.dataDirectory.getPath() );
    	}
        int nextRecordId = this.getNextRecordID();
        File file = getRecordFile( nextRecordId );
        try
        {
            if( !file.createNewFile() )
            {
            	throw new RecordStoreException( "duplicate record "+file.getName() );
            }
            FileOutputStream fos = new FileOutputStream( file, false );
            fos.write( data, offset, numBytes );
            fos.close();
        }
        catch( IOException ex )
        {
            throw new RecordStoreException( "error writing " + file.getAbsolutePath(), ex );
        }
        
        return nextRecordId;
    }
    
    private File getRecordFile( int recordId )
    {
        String filename = this.name + recordId + DATA_EXT;
        return new File( this.dataDirectory, filename );
    }
    
    public int getSize() {
        String[] filenames = this.dataDirectory.list( new RecordStoreFilenameFilter( this.name ) );
        int size = 0;
        for( int i=0; i<filenames.length; i++ ) {
        	String filename = filenames[ i ];
        	File file = new File( this.dataDirectory, filename );
        	size += file.length();
        }
        return size;
    }
    
    public int getSizeAvailable() {
    	// guess what, this isn't available!!
    	return DEFAULT_MAX_RECORDSTORE_SIZE - this.getSize();
    }
    
    public void closeRecordStore()
        throws RecordStoreException
    {
        String path = this.name;
        if( OPEN_RECORD_STORES.contains( path ) )
        {
            OPEN_RECORD_STORES.remove( path );
        }
        else
        {
            throw new RecordStoreNotOpenException( path );
        }
    }
    
    public void deleteRecord( int recordId )
        throws RecordStoreException
    {
        File file = getRecordFile( recordId );
        file.delete();
    }
    
    public RecordEnumeration enumerateRecords( RecordFilter filter, RecordComparator comparator, boolean keepUpdated )
        throws RecordStoreException
    {
        String[] filenames = filter( filter );
        String[] orderedFilenames = order( comparator, filenames );
        return new RecordEnumerationImpl( orderedFilenames );
    }
    
    private class RecordStoreRecordFilter extends RecordStoreFilenameFilter
    {
    	private RecordFilter filter;
    	public RecordStoreRecordFilter( RecordFilter filter )
    	{
    		super( RecordStore.this.name );
    		this.filter = filter;
    	}
		@Override
		public boolean accept(File f, String name) 
		{
			boolean result;
			if( !name.equals( RecordStore.this.name + DATA_EXT ) )
			{
				result = super.accept(f, name);
				if( result )
				{
	                File file = new File( f, name );
	                try
	                {
	                    byte[] data = RecordStore.this.getRecord( file );
	                    result = ( filter != null ) ? ( filter.matches( data ) ) : ( true );
	                }
	                catch( RecordStoreException ex )
	                {
	                    throw new RuntimeException( ex );
	                }
					
				}
			}
			else
			{
				result = false;
			}
			return result;
		}
    	
    	
    }
    
    private String[] filter( final RecordFilter filter )
        throws RecordStoreException
    {
        try
        {
            return this.dataDirectory.list(
            		new RecordStoreRecordFilter( filter )
            );
        }
        catch( RuntimeException ex )
        {
            throw new RecordStoreException( "unable to filter records", ex );
        }
    }
    
    private String[] order( RecordComparator comparator, String[] filenames )
    {
        String[] result;
        if( comparator != null )
        {
            throw new UnsupportedOperationException( "ordering not supported" );
        }
        else
        {
            result = filenames;
        }
        return result;
    }

    public int getNextRecordID()
        throws RecordStoreException
    {
        String[] files = this.dataDirectory.list( 
        		new RecordStoreFilenameFilter( this.name ) 
        );
        int maxPrefix = 0;
        if( files != null )
        {
	        for( int i=0; i<files.length; i++ )
	        {
	            String file = files[ i ];
	            Integer fileNumber = parseFilenameToRecordId( file ); 
	            if( fileNumber != null && fileNumber > maxPrefix )
	            {
	                maxPrefix = fileNumber;
	            }
	        }
        }
        return maxPrefix + 1;
    }
    
    private Integer parseFilenameToRecordId( String file )
    {
        String fileNumberString = file.substring( 0, file.length() - DATA_EXT.length() );
        fileNumberString = fileNumberString.substring( this.name.length() );
        Integer result;
        if( fileNumberString.length() > 0 )
        {
            result = Integer.parseInt( fileNumberString );
        }
        else
        {
        	result = null;
        }
        return result;
    }
    
    public int getNumRecords()
        throws RecordStoreException
    {
        return this.dataDirectory.list( new RecordStoreFilenameFilter( this.name ) ).length;
    }
    
    public byte[] getRecord( int recordId )
        throws RecordStoreException
    {
        File file = getRecordFile( recordId );
        return getRecord( file );
    }
    
    public byte[] getRecord( File file )
        throws RecordStoreException
    {
        if( !file.exists() )
        {
            throw new InvalidRecordIDException( ""+file.getPath() );
        }
        
        FileInputStream fis = null;
        try
        {
            fis = new FileInputStream( file );
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            int b;
            while( ( b = fis.read() ) >= 0 )
            {
                bos.write( b );
            }
            return bos.toByteArray();
        }
        catch( IOException ex )
        {
            throw new RecordStoreException( "unable to read "+file.getPath(), ex );
        }
        finally
        {
            if( fis != null )
            {
                try
                {
                    fis.close();
                }
                catch( IOException ex )
                {
                    throw new RecordStoreException( "unable to close "+file.getPath(), ex );
                }
            }
        }
    
    }
    
    public void setRecord( int recordId, byte[] newData, int offset, int numBytes )
        throws RecordStoreException
    {
        File file = getRecordFile( recordId );
        
        if( !file.exists() )
        {
            throw new InvalidRecordIDException( ""+recordId );
        }
        
        FileOutputStream fos = null;
        try
        {
            fos = new FileOutputStream( file );
            fos.write( newData, offset, numBytes );
        }
        catch( IOException ex )
        {
            throw new RecordStoreException( "unable to write "+recordId, ex );
        }
        finally
        {
            if( fos != null )
            {
                try
                {
                    fos.close();
                }
                catch( IOException ex )
                {
                    throw new RecordStoreException( "unable to close "+recordId, ex );
                }
            }
        }
    }

    private static class RecordStoreFilenameFilter implements FilenameFilter
    {
    	private String name;
    	
    	public RecordStoreFilenameFilter( String name )
    	{
    		this.name = name;
    	}
    	
//		@Override
		public boolean accept( File f, String name ) 
		{
			boolean result;
			if( name.startsWith( this.name ) )
			{
				String post = name.substring( this.name.length() );
				if( post.endsWith( DATA_EXT ) )
				{
					post = post.substring( 0, post.length() - DATA_EXT.length() );
					if( post.length() == 0 )
					{
						result = true;
					}
					else
					{
						try
						{
							Integer.parseInt( post );
							result = true;
						}
						catch( NumberFormatException nfe )
						{
							result = false;
						}
					}
				}
				else
				{
					result = false;
				}
			}
			else
			{
				result = false;
			}
			return result;
		}
    }
    
    private class RecordEnumerationImpl implements RecordEnumeration
    {
        private String[] filenames;
        private int position;
        
        public RecordEnumerationImpl( String[] filenames )
        {
            this.filenames = filenames != null?filenames:new String[0];
        }
        
//        @Override
        public void destroy()
        {
            this.filenames = null;
        }

//        @Override
        public boolean hasNextElement()
        {
            return this.position < this.filenames.length;
        }

//        @Override
        public boolean hasPreviousElement()
        {   
            return this.position > 0;
        }

//        @Override
        public boolean isKeptUpdated()
        {
            return false;
        }

//        @Override
        public void keepUpdated( boolean keepUpdated )
        {
            throw new UnsupportedOperationException( "keepUpdated" );
        }

//        @Override
        public byte[] nextRecord()
            throws RecordStoreException
        {
            String filename = this.filenames[ this.position ];
            this.position ++;
            return RecordStore.this.getRecord( 
                    new File( 
                            RecordStore.this.dataDirectory, filename 
                    ) 
            );
        }

//        @Override
        public int nextRecordId()
        {
            String filenameNumberString = this.filenames[ this.position ];
            this.position++;
            return RecordStore.this.parseFilenameToRecordId( filenameNumberString );
        }

//        @Override
        public int numRecords()
        {
            return this.filenames.length;
        }

//        @Override
        public byte[] previousRecord()
            throws RecordStoreException
        {
            String filename = this.filenames[ this.position - 1 ];
            this.position --;
            return RecordStore.this.getRecord(
                    new File(
                            RecordStore.this.dataDirectory, filename
                    )
            );
        }

//        @Override
        public int previousRecordId()
        {
            String filename = this.filenames[ this.position - 1 ];
            this.position --;
            return RecordStore.this.parseFilenameToRecordId( filename );
        }

//        @Override
        public void rebuild()
        {
            throw new UnsupportedOperationException( "rebuild" );
        }

//        @Override
        public void reset()
        {
            this.position = 0;
        }
        
    }
}
