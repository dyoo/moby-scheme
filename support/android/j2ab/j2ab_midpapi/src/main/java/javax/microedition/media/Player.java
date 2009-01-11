package javax.microedition.media;

public interface Player extends Controllable {
	static int	CLOSED = 1; 
	static int	PREFETCHED = 2;
	static int	REALIZED = 3;
	static int	STARTED = 4;
	static long	TIME_UNKNOWN = 5; 
	static int	UNREALIZED = 6;
	
	void	addPlayerListener(PlayerListener playerListener); 
	void	close() ;
	void	deallocate() ;
	String	getContentType() ;
	long	getDuration() ;
	long	getMediaTime(); 
	int	getState() ;
	void	prefetch() throws MediaException; 
	void	realize() throws MediaException; 
	void	removePlayerListener(PlayerListener playerListener); 
	void	setLoopCount(int count); 
	long	setMediaTime(long now) throws MediaException; 
	void	start() throws MediaException; 
	void	stop() throws MediaException; 
}
