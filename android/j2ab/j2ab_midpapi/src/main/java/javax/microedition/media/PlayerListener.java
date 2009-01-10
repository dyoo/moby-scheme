package javax.microedition.media;

public interface PlayerListener {
	static String	CLOSED  = "closed";
	static String	DEVICE_AVAILABLE = "device available";
	static String	DEVICE_UNAVAILABLE  = "device unavailable";
	static String	DURATION_UPDATED = "duration updated";
	static String	END_OF_MEDIA = "end of media";
	static String	ERROR = "error";
	static String	STARTED = "started";
	static String	STOPPED = "stopped";
	static String	VOLUME_CHANGED = "volume changed";

	void playerUpdate(Player player, String event, Object eventData);
}
