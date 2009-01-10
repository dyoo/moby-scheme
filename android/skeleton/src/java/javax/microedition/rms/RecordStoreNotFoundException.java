package javax.microedition.rms;

public class RecordStoreNotFoundException extends RecordStoreException
{
    public RecordStoreNotFoundException()
    {
        
    }
    
    public RecordStoreNotFoundException( String message )
    {
        super( message );
    }
    
    public RecordStoreNotFoundException( String message, Throwable cause )
    {
        super( message, cause );
    }
}
