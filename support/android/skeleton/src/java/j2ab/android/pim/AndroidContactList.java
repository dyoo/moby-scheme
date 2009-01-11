package j2ab.android.pim;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.microedition.midlet.MIDlet;
import javax.microedition.pim.Contact;
import javax.microedition.pim.ContactList;
import javax.microedition.pim.PIMItem;

import android.database.Cursor;
import android.net.Uri;
import android.provider.BaseColumns;
import android.provider.Contacts;

public class AndroidContactList extends AndroidPIMList implements ContactList 
{
	private static final Map<Integer, String> DEFAULT_COLUMN_MAPPINGS = new HashMap<Integer, String>();
	private static final Map<String, Class> DEFAULT_COLUMN_TYPES = new HashMap<String, Class>();

	static 
	{
		DEFAULT_COLUMN_MAPPINGS.put( Contact.UID, BaseColumns._ID ); 
		DEFAULT_COLUMN_TYPES.put( BaseColumns._ID, String.class );
		
		DEFAULT_COLUMN_MAPPINGS.put( Contact.FORMATTED_NAME, Contacts.PeopleColumns.NAME );
		DEFAULT_COLUMN_TYPES.put( Contacts.PeopleColumns.NAME, String.class );
		
		//DEFAULT_COLUMN_MAPPINGS.put( Contact.TITLE, Contacts.PeopleColumns.TITLE );
		
		DEFAULT_COLUMN_MAPPINGS.put( Contact.TEL, Contacts.People.NUMBER );
		DEFAULT_COLUMN_TYPES.put( Contacts.People.NUMBER, String.class );
		
		//DEFAULT_COLUMN_MAPPINGS.put( Contact.EMAIL, Contacts.ContactMethodsColumns.DATA );
	} 
		
	public AndroidContactList()
	{
		super( 
				"Contacts",
				MIDlet.DEFAULT_MIDLET.getActivity().getContentResolver(), 
				Contacts.People.CONTENT_URI,
				convert( 
						DEFAULT_COLUMN_MAPPINGS.values(), 
						new String[]{ 
							Contacts.People.PRIMARY_EMAIL_ID, 
							Contacts.People.PRIMARY_PHONE_ID
						} 
				)
		);
	}

	@Override
	protected PIMItem create( Cursor cursor ) 
	{
		AndroidContact contact = new AndroidContact( cursor );
		
		return contact;
	}
	
	private static class PhoneNumber
	{
		private String number;
		private int attributes;
		
		public PhoneNumber( String number, int attributes )
		{
			this.number = number;
			this.attributes = attributes;
		}

		public String getNumber() {
			return number;
		}

		public int getAttributes() {
			return attributes;
		}
		
		
	}
	
	private class AndroidContact extends PreloadingAndroidPIMItem implements Contact
	{
		private ArrayList<PhoneNumber> phoneNumbers;
		private String emailAddress;
		
		public AndroidContact( Cursor cursor )
		{
			super( cursor, DEFAULT_COLUMN_MAPPINGS, DEFAULT_COLUMN_TYPES );
			
			// look up the phone numbers for this person
			
			String id = this.getString( Contact.UID, 0 );
			String preferredPhoneId = this.getString( 
					Contacts.People.PRIMARY_PHONE_ID 
			);
			String preferredEmailId = this.getString(
					Contacts.People.PRIMARY_EMAIL_ID
			);
			
			Uri emailUri = Contacts.ContactMethods.CONTENT_URI;
			Cursor emailCursor = AndroidContactList.this.contentResolver.query( 
					emailUri, 
					new String[]{
							Contacts.ContactMethods._ID, 
							Contacts.ContactMethods.TYPE, 
							Contacts.ContactMethods.DATA
					}, 
					Contacts.ContactMethods.PERSON_ID+"=?", 
					new String[]{ id }, 
					null
			);
			String emailAddress = null;
			
			int dataColumn = emailCursor.getColumnIndex( Contacts.ContactMethods.DATA );
			int emailTypeColumn = emailCursor.getColumnIndex( Contacts.ContactMethods.TYPE );
			int emailIdColumn = emailCursor.getColumnIndex( Contacts.ContactMethods._ID );
			while( emailCursor.moveToNext() )
			{
				String data = emailCursor.getString( dataColumn );
				String type = emailCursor.getString( emailTypeColumn );
				String emailId = emailCursor.getString( emailIdColumn );
				if( emailAddress == null || Contacts.ContactMethods.CONTENT_EMAIL_TYPE.equals( type ) )
				{
					emailAddress = data;
				}
				if( emailId != null && emailId.equals( preferredEmailId ) )
				{
					emailAddress = data;
					break;
				}
			}
			emailCursor.close();
			this.emailAddress = emailAddress;
			
			Uri phoneUri = Contacts.Phones.CONTENT_URI;
			Cursor phoneCursor = AndroidContactList.this.contentResolver.query( 
					phoneUri, 
					new String[]{ 
							Contacts.Phones._ID,
							Contacts.Phones.NUMBER, 
							Contacts.Phones.TYPE, 
							Contacts.Phones.PERSON_ID
					}, 
					Contacts.Phones.PERSON_ID+"=?", 
					new String[]{ id }, 
					null
			);
			
			int numberColumn = phoneCursor.getColumnIndex( Contacts.Phones.NUMBER );
			int typeColumn = phoneCursor.getColumnIndex( Contacts.Phones.TYPE );
			int idColumn = phoneCursor.getColumnIndex( Contacts.Phones._ID );
			
			ArrayList<PhoneNumber> phoneNumbers = new ArrayList<PhoneNumber>();
			
			while( phoneCursor.moveToNext() )
			{
				String number = phoneCursor.getString( numberColumn );
				int type = phoneCursor.getInt( typeColumn );
				String phoneId = phoneCursor.getString( idColumn );
				int attributes = 0;
				if( type == Contacts.Phones.TYPE_MOBILE )
				{
					attributes = attributes | Contact.ATTR_MOBILE;
				}
				if( type == Contacts.Phones.TYPE_WORK ) 
				{
					attributes = attributes | Contact.ATTR_WORK;
				}
				if( type == Contacts.Phones.TYPE_HOME ) 
				{
					attributes = attributes | Contact.ATTR_HOME;
				}
				if( phoneId != null && phoneId.equals( preferredPhoneId ) )
				{
					attributes = attributes | Contact.ATTR_PREFERRED;
				}
				PhoneNumber phoneNumber = new PhoneNumber( number, attributes );
				phoneNumbers.add( phoneNumber );
			}
			phoneCursor.close();
			this.phoneNumbers = phoneNumbers;
			
		}
		
		public int getPreferredIndex( int field ) 
		{
			return 0;
		}
		
		@Override
		public int countValues( int field ) 
		{
			int result;
			if( field == Contact.TEL )
			{
				result = this.phoneNumbers.size();
			}
			else
			{
				result = super.countValues( field );
			}
			return result;
		}
		
		@Override
		public int getAttributes( int field, int index ) 
		{
			int result;
			if( field == Contact.TEL )
			{
				result = this.phoneNumbers.get( index ).getAttributes();
			}
			else if( field == Contact.EMAIL )
			{
				result = this.emailAddress != null ? 1 : 0;
			}
			else
			{
				result = super.getAttributes( field, index );
			}
			return result;
		}

		@Override
		public String getString(int field, int index) 
		{
			String result;
			if( field == Contact.TEL )
			{
				result = this.phoneNumbers.get( index ).getNumber();
			}
			else if( field == Contact.EMAIL ) 
			{
				result = this.emailAddress;
			}
			else
			{
				result = super.getString( field, index );
			}
			return result;
		}

		@Override
		public String[] getStringArray(int field, int index) 
		{
			String[] result;
			if( field == Contact.NAME )
			{
				String fullName = getString( Contact.FORMATTED_NAME, 0 );
				int spaceIndex = fullName.indexOf( ' ' );
				String firstName; 
				String lastName;
				String title = getString( Contact.TITLE, 0 );
				String prefix = null;
				String postfix = null;
				if( spaceIndex >= 0 )
				{
					firstName = fullName.substring( 0, spaceIndex ); 
					lastName = fullName.substring( spaceIndex + 1 );
				}
				else
				{
					firstName = fullName;
					lastName = null;
				}
				result = new String[]{ firstName, lastName, title, prefix, postfix };
			}
			else
			{
				result = super.getStringArray( field, index );
			}
			return result;
		}
		
		
	}

	@Override
	public boolean isSupportedField(int fieldName) {
		return DEFAULT_COLUMN_MAPPINGS.containsKey( fieldName );
	}
	
	
}
