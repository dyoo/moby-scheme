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



public class PlaylistRecord implements Serializable {
    private long id;
    private String name;

    public PlaylistRecord(long id, String name) {

	this.id = id;
	this.name = name;
    }

    public long getId() {
	return this.id;
    }

    public String getName() { 
	return this.name;
    }


    public String toString() {
	return this.name;
    }


    public List<Uri> getSongUris(Activity activity) {
	int[] songIds = getSongListForPlaylist(activity);
	List<Uri> uris = new ArrayList<Uri>();
	for (int i = 0; i < songIds.length; i++) {
	    uris.add(getSongUri(songIds[i]));
	}
	return uris;
    }

    

    // getSongUri: long -> string
    private Uri getSongUri(int songId) {
	return Uri.parse(Media.EXTERNAL_CONTENT_URI + "/" + songId);
    }



    // Returns a list of song ids.
    private int[] getSongListForPlaylist(Activity activity) {
        final String[] ccols = new String[] { Playlists.Members.AUDIO_ID };
        Cursor cursor = activity.managedQuery(Playlists.Members.getContentUri("external", this.id),
                ccols, null, null, Playlists.Members.DEFAULT_SORT_ORDER);
        
        if (cursor != null) {
            int [] list = getSongListForCursor(cursor);
            cursor.close();
            return list;
        }
        return new int[]{};
    }



    private int[] getSongListForCursor(Cursor cursor) {
        if (cursor == null) {
            return new int[]{};
        }
        int len = cursor.getCount();
        int [] list = new int[len];
        cursor.moveToFirst();
        int colidx = -1;
        try {
            colidx = cursor.getColumnIndexOrThrow(Playlists.Members.AUDIO_ID);
        } catch (IllegalArgumentException ex) {
            colidx = cursor.getColumnIndexOrThrow(Media._ID);
        }
        for (int i = 0; i < len; i++) {
            list[i] = cursor.getInt(colidx);
            cursor.moveToNext();
        }
        return list;
    }



}

