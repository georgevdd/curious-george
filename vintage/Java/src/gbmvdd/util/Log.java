package gbmvdd.util;

public abstract class Log
{
	public abstract void log( Object o );
	
	public static Log defaultLog = new DefaultLog();
}
