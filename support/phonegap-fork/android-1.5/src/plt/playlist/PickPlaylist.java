package plt.playlist;

import android.app.Activity;
import android.app.ListActivity;
import android.content.Context;
import android.net.Uri;
import android.database.Cursor;
import android.view.View;
import android.widget.ListView;
import android.widget.ArrayAdapter;
import android.content.Intent;
import android.content.ContentResolver;
import android.os.Bundle;
import android.provider.MediaStore.Audio.Playlists;
import android.provider.MediaStore.Audio.PlaylistsColumns;
import android.provider.MediaStore.Audio.Media;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;


public class PickPlaylist extends ListActivity {


    ArrayAdapter<PlaylistRecord> items;


    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
	initializeItems();	
	setListAdapter(items);
	getListView().setTextFilterEnabled(true);
    }


    private void initializeItems() {
	Uri uri = Playlists.getContentUri("external");
	this.items = new ArrayAdapter<PlaylistRecord>
	    (this, android.R.layout.simple_list_item_1);
	
	try {
	    Cursor cursor = this.managedQuery
		(uri, null, null, null, PlaylistsColumns.NAME + " ASC");
	    if (cursor.moveToFirst()) {
		int idColumn = cursor.getColumnIndex(Playlists._ID); 
		int nameColumn = cursor.getColumnIndex(Playlists.NAME);
		do {
		    this.items.add(new PlaylistRecord(cursor.getLong(idColumn),
						      cursor.getString(nameColumn)));

		} while (cursor.moveToNext());
	    }
	} catch (Exception e) {
	    System.out.println("Exception: " + e);
	    this.setResult(RESULT_CANCELED);
	}
    }


    protected void onListItemClick(ListView l, View v, int position, long id) {
	PlaylistRecord record = this.items.getItem(position);
	System.out.println(record.getSongUris(this));

	Intent intent = new Intent();
	intent.putExtra("position", position);
	intent.putExtra("value", record);
	setResult(RESULT_OK, intent);




// 	android.media.MediaPlayer player = android.media.MediaPlayer.create
// 	    (this, record.getSongUris(this).get(0));	
// 	player.start();

	
	
	this.finish();
    }

}
