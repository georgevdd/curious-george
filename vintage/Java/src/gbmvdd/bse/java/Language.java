package gbmvdd.bse.java;

import java.io.Reader;
import antlr.TokenStream;
import gbmvdd.bse.*;
import java.awt.Color;
import java.util.Properties;

public class Language implements gbmvdd.bse.Language
{
	static Properties colours;
	static SimpleTokenColourer colourer = new SimpleTokenColourer();
	
	static
	{
		colours = new Properties();
		try
		{
			colours.load( gbmvdd.bse.java.Language.class.getResourceAsStream
				( "/gbmvdd/bse/java/Colours.properties" ) );
			colourer.setColours( colours );
		}
		catch( Exception e )
		{
			gbmvdd.util.Log.defaultLog.log
				( "TinyCLanguage: couldn't find default colour properties:" + e );
		}
	}
	
	public BSELexer createLexer( Reader in )
	{
		return new JavaLexer( in );
	}

	public BSEParser createParser( TokenStream tStream )
	{
		return new JavaParser( tStream );
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