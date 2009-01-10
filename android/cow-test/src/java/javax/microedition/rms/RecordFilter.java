package javax.microedition.rms;

public interface RecordFilter
{   
    boolean matches( byte[] candidate );
}
