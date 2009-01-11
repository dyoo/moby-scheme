package j2ab.android.pim;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;

import javax.microedition.pim.PIMException;
import javax.microedition.pim.PIMItem;
import javax.microedition.pim.PIMList;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;

public abstract class AndroidPIMList implements PIMList 
{
	public static final String[] convert( Collection<String> values )
	{
		return convert( values, new String[ 0 ] );
	}
	
	public static final String[] convert( Collection<String> values, String[] additional )
	{
		String[] names = new String[ values.size() + additional.length ];
		System.arraycopy( additional, 0, names, 0, additional.length );
		int pos = additional.length;
		for( String value : values )
		{
			names[ pos ] = value;
			pos++;
		}
		return names;
	}
	
	private String name;
	private String[] columnNames;
	protected ContentResolver contentResolver;
	private Uri contentURI;
	private Collection<Cursor> cursors;
	
	protected AndroidPIMList(
			String name,
			ContentResolver contentResolver,
			Uri contentURI, 
			String[] columnNames )
	{
		this.name = name;
		this.contentResolver = contentResolver;
		this.contentURI = contentURI;
		this.columnNames = columnNames;
		this.cursors = new ArrayList<Cursor>();
		
	}
	
//	@Override
	public void close() throws PIMException 
	{
		for( Cursor cursor : this.cursors )
		{
			cursor.close();
		}
		this.cursors.clear();
	}
	
	public String getName() {
		return this.name;
	}

//	@Override
	public Enumeration items() throws PIMException 
	{
		
		Cursor cursor = this.contentResolver.query( 
				this.contentURI, 
				this.columnNames, 
				null, 
				null, 
				null
		);
		if( cursor == null )
		{
			StringBuilder columnNames = new StringBuilder();
			for( int i=0; i<this.columnNames.length; i++ )
			{
				String columnName = this.columnNames[ i ];
				columnNames.append( columnName );
				if( i < this.columnNames.length-1 )
				{
					columnNames.append( ", " );
				}
			}
			throw new PIMException( "invalid query "+this.contentURI.toString()+" "+columnNames );
		}
		this.cursors.add( cursor );
		return new AndroidPIMListEnumeration( cursor );
	}
	
	protected abstract PIMItem create( Cursor cursor );

	private class AndroidPIMListEnumeration implements Enumeration
	{
		private Cursor cursor;
		
		public AndroidPIMListEnumeration( Cursor cursor )
		{
			this.cursor = cursor;
			cursor.moveToFirst();
		}
		
//		@Override
		public boolean hasMoreElements() 
		{
			return !this.cursor.isAfterLast();
		}

//		@Override
		public Object nextElement() 
		{
			PIMItem item = AndroidPIMList.this.create( this.cursor );
			this.cursor.moveToNext();
			return item;
		}
		
	}
}
