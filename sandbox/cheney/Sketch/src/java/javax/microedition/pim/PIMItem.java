package javax.microedition.pim;

public interface PIMItem
{
    public static final int ATTR_NONE           = 0;
    
    public static final int BINARY              = -1;
    public static final int BOOLEAN             = -2;
    public static final int DATE                = -3;
    public static final int EXTENDED_ATTRIBUTE_MIN_VALUE = -4;
    public static final int EXTENDED_FIELD_MIN_VALUE = -5;
    public static final int INT                 = -6;
    public static final int STRING              = -7;
    public static final int STRING_ARRAY        = -8;
    
    int getAttributes( int field, int index );
    
    String[] getStringArray( int field, int index );
    
    String getString( int field, int index );
    
    long getDate( int field, int index );
    
    int countValues( int field );
}
