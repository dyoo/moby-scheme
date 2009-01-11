package javax.microedition.media;

import javax.microedition.media.control.ToneControl;

public class TonePlayer implements Player, ToneControl, Controllable {

	
	
//	@Override
	public Control getControl(String controlType) {
		if( controlType.equals( "ToneControl" ) )
		{
			return this;
		}
		else
		{
			return null;
		}
	}

//	@Override
	public Control[] getControls() {
		// TODO Auto-generated method stub
		return new Control[]{ this };
	}

//	@Override
	public void addPlayerListener(PlayerListener playerListener) {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void close() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void deallocate() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public String getContentType() {
		// TODO Auto-generated method stub
		return null;
	}

//	@Override
	public long getDuration() {
		// TODO Auto-generated method stub
		return 0;
	}

//	@Override
	public long getMediaTime() {
		// TODO Auto-generated method stub
		return 0;
	}

//	@Override
	public int getState() {
		// TODO Auto-generated method stub
		return 0;
	}

//	@Override
	public void prefetch() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void realize() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void removePlayerListener(PlayerListener playerListener) {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void setLoopCount(int count) {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public long setMediaTime(long now) {
		// TODO Auto-generated method stub
		return 0;
	}

//	@Override
	public void start() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void stop() {
		// TODO Auto-generated method stub
		
	}

//	@Override
	public void setSequence(byte[] sequence) {

	}

}
