package gbmvdd.bse.tinyc;

import java.io.*;
import antlr.*;
import gbmvdd.bse.*;
import java.util.Properties;
import java.awt.Color;

public class TinyCLanguage implements gbmvdd.bse.Language
{
	static Properties colours;
	static SimpleTokenColourer colourer = new SimpleTokenColourer();
	
	static
	{
		colours = new Properties();
		try
		{
			colours.load( TinyCLanguage.class.getClassLoader().getResource
				( "gbmvdd/bse/tinyc/Colours.properties" ).openStream() );
			colourer.setColours( colours );
		}
		catch( Exception e )
		{
			System.err.println
				( "TinyCLanguage: couldn't find default colour properties." );
		}
	}
	
	public BSELexer createLexer( Reader in )
	{
		return new TinyCLexer( in );
	}

	public BSEParser createParser( TokenStream tStream )
	{
		return new TinyCParser( tStream );
	}
	
	public Object createLanguageExtraData()
	{
		return null;
	}
	
	public java.awt.Color getTokenColour( BSEToken token, Object languageExtraData )
	{
		return colourer.getTokenColour( token, languageExtraData );
	}
	
	public java.awt.Color getBackgroundColour( BSEToken token, Object languageExtraData )
	{
		return colourer.getBackgroundColour( token, languageExtraData );
	}
}