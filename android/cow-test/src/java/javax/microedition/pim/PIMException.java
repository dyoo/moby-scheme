package javax.microedition.pim;

public class PIMException extends Exception
{
    
    public static final int FEATURE_NOT_SUPPORTED   = 1;
    public static final int GENERAL_ERROR           = 2;
    public static final int LIST_CLOSED             = 3;
    public static final int LIST_NOT_ACCESSIBLE     = 4;
    public static final int MAX_CATEGORIES_EXCEEDED = 5;
    public static final int UNSUPPORTED_VERSION     = 6;
    public static final int UPDATE_ERROR            = 7;
    
    private int reason;
    
    public PIMException( String message )
    {
        this( message, GENERAL_ERROR );
    }
    
    public PIMException( String message, int reason )
    {
        super( message );
        this.reason = reason;
    }
    
    public int getReason()
    {
        return this.reason;
    }
}
