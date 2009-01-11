package javax.microedition.io.file;

import java.io.IOException;

import javax.microedition.io.InputConnection;
import javax.microedition.io.OutputConnection;
import javax.microedition.io.StreamConnection;

public interface FileConnection extends javax.microedition.io.Connection, InputConnection,
		OutputConnection, StreamConnection {
	
	 long availableSize(); 
	 boolean canRead(); 
	 boolean canWrite(); 
	 void create() throws IOException; 
	 void delete()  throws IOException;
	 long directorySize(boolean includeSubDirs) throws IOException; 
	 boolean exists(); 
	 long fileSize() throws IOException; 
	 java.lang.String getName(); 
	 java.lang.String getPath();
	 java.lang.String getURL();
	 boolean isDirectory();
	 boolean isHidden();
	 boolean isOpen();
	 long lastModified();
	 java.util.Enumeration list() throws IOException; 
	 java.util.Enumeration list(java.lang.String filter, boolean includeHidden) throws IOException; 
	 void mkdir() throws IOException; 
	 java.io.DataInputStream openDataInputStream() throws IOException; 
	 java.io.DataOutputStream openDataOutputStream() throws IOException; 
	 java.io.InputStream openInputStream() throws IOException;
	 java.io.OutputStream openOutputStream() throws IOException;  
	 java.io.OutputStream openOutputStream(long byteOffset) throws IOException; 
	 void rename(java.lang.String newName) throws IOException; 
	 void setFileConnection(java.lang.String fileName) throws IOException; 
	 void setHidden(boolean hidden) throws IOException; 
	 void setReadable(boolean readable)  throws IOException;
	 void setWritable(boolean writable)  throws IOException;
	 long totalSize();
	 void truncate(long byteOffset) throws IOException; 
	 long usedSize();
}
