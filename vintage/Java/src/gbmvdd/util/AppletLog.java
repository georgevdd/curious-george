package gbmvdd.util;

import java.awt.*;
import java.applet.*;

public class AppletLog extends Applet
{
	TextArea ta;
	
	public Log log = new Log()
		{
			public void log( Object o )
			{
				ta.append( o.toString() + "\n" );
			}
		};
		

	public void init()
	{
		setLayout( new BorderLayout() );
		ta = new TextArea();
		ta.setEditable( false );
		this.add( ta );
	}
	
	public void log( Object o )
	{
		ta.append( o.toString() + "\n" );
	}
}
