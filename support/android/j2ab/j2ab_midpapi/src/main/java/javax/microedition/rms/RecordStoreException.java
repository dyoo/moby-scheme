package javax.microedition.rms;

public class RecordStoreException extends Exception
{
    public RecordStoreException()
    {
        
    }
    
    public RecordStoreException( String message )
    {
        super( message );
    }
    
    public RecordStoreException( String message, Throwable cause )
    {
        super( message, cause );
    }
}
