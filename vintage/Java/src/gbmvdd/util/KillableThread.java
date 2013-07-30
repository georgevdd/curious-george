package gbmvdd.util;

public class KillableThread extends Thread
{
	public KillableThread()
	{
		super();
	}

	public KillableThread(String name)
	{
		super( name );
	}

	public KillableThread( ThreadGroup group, String name )
	{
		super( group, name );
	}

	protected boolean killed = false;

	public final void kill()
	{
		killed = true;
	}

	public final boolean killed()
	{
		return killed;
	}
}

