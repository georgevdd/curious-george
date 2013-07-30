package gbmvdd.util;

import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.io.*;
import java.lang.reflect.*;

public class MenuLoader
{
	public static java.util.List load( Reader in, Object o ) throws IOException
	{
		java.util.List topLevelMenus = new LinkedList();
		Stack stack = new Stack();
		
		Class c = o.getClass();
		
		BufferedReader br = new BufferedReader( in );
		String line;
		while( (line = br.readLine()) != null )
		{
			if( (line.length() == 0) ||
				line.charAt( 0 ) == '#' ||
				line.charAt( 0 ) == '!' )
				continue;
			
			int depth = 0;
			while( depth < line.length() &&
				(line.charAt( depth ) == ' ' ||
				line.charAt( depth ) == '\t') )
				++depth;
			
			if( depth > stack.size() )
				continue;
			
			String itemCmd, itemLabel;
			int cmdEnd = line.indexOf( ' ', depth );
			if( cmdEnd == -1 )
				itemCmd = itemLabel = line.substring( depth );
			else
			{
				itemCmd = line.substring( depth, cmdEnd );
				itemLabel = line.substring( cmdEnd + 1 );
			}
			
			MenuItem item;
			
			try
			{
				try
				{
					gbmvdd.awt.Action action = (gbmvdd.awt.Action)(c.getField( itemCmd ).get( o ));
					item = action.new MenuItem();
				}
				catch( NoSuchFieldException e )
				{
					if( itemCmd.endsWith( "Menu" ) )
						item = new Menu( itemLabel );
					else
					{
						Log.defaultLog.log( "Creating default menu item for "
							+ itemCmd + "." );
						item = new MenuItem( itemLabel );
						item.setEnabled( false );
					}
					item.setActionCommand( itemCmd );
				}

				while( stack.size() > depth )
					stack.pop();

				if( depth == 0 )
					topLevelMenus.add( item );
				else
					((Menu)stack.peek()).add( item );

				stack.push( item );
			}
			catch( Exception e )
			{
				Log.defaultLog.log( "Couldn't create menu item for "
					+ itemCmd + ": " + e );
			}				
		}
		
		return topLevelMenus;
	}
}