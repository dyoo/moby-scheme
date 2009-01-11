package javax.microedition.pim;

import java.util.Enumeration;

public interface PIMList
{
    Enumeration items()
        throws PIMException;
    
    void close()
        throws PIMException;
    
    boolean isSupportedField( int fieldName );
    
    String getName();
}
