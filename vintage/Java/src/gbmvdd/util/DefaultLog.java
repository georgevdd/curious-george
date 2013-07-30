package gbmvdd.util;

public class DefaultLog extends Log
{
	public void log( Object o )
	{
		System.err.println( o );
	}
}
