package javax.microedition.rms;

public class InvalidRecordIDException extends RecordStoreException
{
    public InvalidRecordIDException()
    {
        
    }
    
    public InvalidRecordIDException( String message )
    {
        super( message );
    }
    
    public InvalidRecordIDException( String message, Throwable cause )
    {
        super( message, cause );
    }
}
