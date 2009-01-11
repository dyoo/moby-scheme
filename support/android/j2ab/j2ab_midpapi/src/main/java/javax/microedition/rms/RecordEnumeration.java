package javax.microedition.rms;

public interface RecordEnumeration
{
    void destroy();
    
    boolean hasNextElement();
    
    boolean hasPreviousElement();
    
    boolean isKeptUpdated();
    
    void keepUpdated( boolean keepUpdated );
    
    byte[] nextRecord()
        throws RecordStoreException;
    
    int nextRecordId();
    
    int numRecords();
        
    byte[] previousRecord()
        throws RecordStoreException;
    
    int previousRecordId();
    
    void rebuild();
    
    void reset();
}
