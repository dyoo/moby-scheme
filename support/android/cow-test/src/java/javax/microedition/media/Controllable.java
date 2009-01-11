package javax.microedition.media;

public interface Controllable {
	Control getControl( String controlType );
	
	Control[] getControls();
}
