package javax.microedition.io;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

public interface InputConnection extends Connection
{
    DataInputStream openDataInputStream()
        throws IOException;
    
    InputStream openInputStream()
        throws IOException;
}
