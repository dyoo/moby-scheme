package javax.microedition.rms;

public class RecordStoreNotOpenException extends RecordStoreException
{
    public RecordStoreNotOpenException()
    {
        
    }
    
    public RecordStoreNotOpenException( String message )
    {
        super( message );
    }
    
    public RecordStoreNotOpenException( String message, Throwable cause )
    {
        super( message, cause );
    }
}
