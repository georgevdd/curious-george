package gbmvdd.bse;

import antlr.*;
import java.awt.Color;
import java.util.Properties;

public class SimpleTokenColourer
{
	Properties colours;
	
	public void setColours( Properties colours )
	{
		this.colours = colours;
	}
	
	public Color getTokenColour( BSEToken token, Object languageExtraData )
	{
		try
		{
			String groupName = colours.getProperty
				( String.valueOf( token.getType() ) + ".gid", "0" );
			String colourString = colours.getProperty
				( groupName + ".grp.col", "0" );
			return Color.decode( colourString );
		}
		catch( Exception e )
		{
			return Color.black;
		}
	}
	
	public Color getBackgroundColour( BSEToken token, Object languageExtraData )
	{
		try
		{	String groupName = colours.getProperty
				( String.valueOf( token.getType() ) + ".gid", "0" );

			if( token.getType() == -1 )
				System.err.println( groupName );
			String colourString = colours.getProperty
				( groupName + ".grp.bgcol", "0xffffff" );
			return Color.decode( colourString );
		}
		catch( Exception e )
		{
			return Color.white;
		}
	}
}