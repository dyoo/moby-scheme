package javax.microedition.pim;

import j2ab.android.pim.AndroidContactList;

public class PIM
{
    public static final int READ_ONLY       = 0x01;
    public static final int WRITE_ONLY      = 0x02;
    public static final int READ_WRITE      = 0x03;
    
    public static final int CONTACT_LIST    = 1;
    
    private static PIM INSTANCE; 
    
    public static final PIM getInstance()
    {
        if( INSTANCE == null )
        {
            INSTANCE = new PIM();
        }
        return INSTANCE;
    }
    
    protected PIM()
    {
        
    }
    
    public String[] listPIMLists( int type )
    {
    	return new String[] { "default" };
    }
    
    public PIMList openPIMList( int pimListType, int mode )
    {
    	return openPIMList( pimListType, mode, null );
    }
    
    public PIMList openPIMList( int pimListType, int mode, String name )
    {
        return new AndroidContactList();
    }
}
