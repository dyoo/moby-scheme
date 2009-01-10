package javax.microedition.rms;

public interface RecordComparator
{
    public static final int EQUIVALENT  = 0;
    public static final int PRECEDES    = -1;
    public static final int FOLLOWS     = 1;
    
    int compare( byte[] rec1, byte[] rec2 );
}