package javax.microedition.pim;


public interface Contact extends PIMItem
{
    public static final int ADDR            = 1;
    public static final int BIRTHDAY        = 2;
    public static final int CLASS           = 3;
    public static final int EMAIL           = 4;
    public static final int FORMATTED_ADDR  = 5;
    public static final int FORMATTED_NAME  = 6;
    public static final int NAME            = 7;
    public static final int NICKNAME        = 8;
    public static final int NOTE            = 9;
    public static final int ORG             = 10;
    public static final int PHOTO           = 11;
    public static final int PUBLIC_KEY      = 12;
    public static final int PUBLIC_KEY_STRING = 13;
    public static final int REVISION        = 14;
    public static final int TEL             = 15;
    public static final int TITLE           = 16;
    public static final int UID             = 17;
    public static final int URL             = 18;
    
    public static final int ATTR_ASST       = 0x0001;
    public static final int ATTR_AUTO       = 0x0002;
    public static final int ATTR_FAX        = 0x0004;
    public static final int ATTR_HOME       = 0x0008;
    public static final int ATTR_MOBILE     = 0x0010;
    public static final int ATTR_OTHER      = 0x0020;
    public static final int ATTR_PAGER      = 0x0040;
    public static final int ATTR_PREFERRED  = 0x0080;
    public static final int ATTR_SMS        = 0x0100;
    public static final int ATTR_WORK       = 0x0200;
    
    public static final int ADDR_POBOX      = 0;
    public static final int ADDR_EXTRA      = 1;
    public static final int ADDR_STREET     = 2;
    public static final int ADDR_LOCALITY   = 3;
    public static final int ADDR_REGION     = 4;
    public static final int ADDR_POSTALCODE = 5;
    public static final int ADDR_COUNTRY    = 6;
    
    public static final int NAME_FAMILY     = 0;
    public static final int NAME_GIVEN      = 1;
    public static final int NAME_OTHER      = 2;
    public static final int NAME_PREFIX     = 3;
    public static final int NAME_SUFFIX     = 4;
    
    public static final int CLASS_CONFIDENTIAL = 0;
    public static final int CLASS_PRIVATE   = 1;
    public static final int CLASS_PUBLIC    = 2;
 
    int getPreferredIndex( int field );
}
