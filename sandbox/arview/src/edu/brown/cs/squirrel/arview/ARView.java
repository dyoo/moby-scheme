package edu.brown.cs.squirrel.arview;

import android.app.Activity;
import android.os.Bundle;

public class ARView extends Activity {
	@Override
	public void onCreate(Bundle bundle) {
		super.onCreate(bundle);
		setContentView(new ARViewView(this));
	}
}
