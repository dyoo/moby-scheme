package javax.microedition.io;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public interface OutputConnection extends Connection
{
    DataOutputStream openDataOutputStream()
        throws IOException;
    
    OutputStream openOutputStream()
        throws IOException;
}
