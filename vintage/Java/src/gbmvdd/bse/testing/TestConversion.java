package gbmvdd.bse.testing;

import java.io.*;
import java.util.*;

import antlr.*;
import gbmvdd.bse.*;

public class TestConversion
{
	static Properties props;

	static String getStringSetting( String key, String defaultValue, PrintStream errPrintStream )
	{
		String str = props.getProperty( key );
		if( str == null )
		{
			if( errPrintStream != null )
				errPrintStream.println( "Using default value (" +
					defaultValue + ") for " + key + "." );
			return defaultValue;
		}
		return str;
	}

	static int getIntSetting( String key, int defaultValue, PrintStream errPrintStream )
	{
		String strNum = props.getProperty( key );
		if( strNum == null )
		{
			if( errPrintStream != null )
				errPrintStream.println( "Using default value (" +
					defaultValue + ") for " + key + "." );
			return defaultValue;
		}

		try
		{
			return Integer.parseInt( strNum );
		}
		catch( NumberFormatException e )
		{
			if( errPrintStream != null )
				errPrintStream.println( "Bad number (\"" +
					strNum + "\") for " + key +
					". Using default value (" + defaultValue + ")." );
			return defaultValue;
		}
	}

	static boolean getBooleanSetting( String key, boolean defaultValue, PrintStream errPrintStream )
	{
		String strBool = props.getProperty( key );

		if( strBool != null )
		{
			if( strBool.equals( "false" ) )
				return false;
			if( strBool.equals( "true" ) )
				return true;
		}

		if( errPrintStream != null )
			errPrintStream.println( "Using default value (" +
				defaultValue + ") for " + key +
				((strBool == null) ? "" : (" - configured value (" +
					strBool + ") is neither true nor false"))
				+ "." );
		return defaultValue;
	}

	public static void main( String[] args )
	{
		String configFilename;

		if( args.length < 1 )
			configFilename = null;
		else
			configFilename = args[0];

		props = new Properties();

		if( configFilename != null )
		{
			File configFile = new File( configFilename );
			if( !configFile.exists() )
			{
				System.err.println( "Configuration file \"" + configFilename + "\" does not exist." );
				System.exit( 1 );
			}

			try
			{
				InputStream configInputStream = new FileInputStream( configFile );
				props.load( configInputStream );
				configInputStream.close();
			}
			catch( IOException e )
			{
				System.err.println( "Unable to load settings from \"" + configFilename + "\"." );
				System.exit( 2 );
			}
		}
		else
			System.err.println( "Using default settings..." );

		String languageName = getStringSetting( "language", "gbmvdd.bse.tinyc.TinyC", System.err ) + "Language";
		Class languageClass = null;
		try
		{
			languageClass = Class.forName( languageName );
		}
		catch( ClassNotFoundException e )
		{
			try
			{
				languageClass = Class.forName( "gbmvdd.bse." + languageName );
			}
			catch( ClassNotFoundException e2 )
			{
				System.err.println( "Class file for \"" + languageName + "\" not found." );
				System.exit( 2 );
			}
		}
		System.out.println( "Language definition: " + languageClass );

		String sourceFilename = getStringSetting( "source", "TinyCParser.java", System.err );
		File sourceFile = new File( sourceFilename );
		if( !sourceFile.exists() )
		{
			System.err.println( "Source file \"" + sourceFilename + "\" does not exist." );
			System.exit( 3 );
		}

		int numRuns = getIntSetting( "runs", 500, System.err );
		System.out.println( "Trials: " + numRuns );

		Language language = null;
		try
		{
			language = (Language)languageClass.newInstance();
		}
		catch( Exception e )
		{
			System.err.println( "Failed to instantiate language class." );
			System.exit( 4 );
		}

		int forcedLocation = getIntSetting( "forcedLocation", -1, System.err );
		if( forcedLocation != -1 )
			numRuns = 1;

		boolean printInput = getBooleanSetting( "printInput", false, System.err );
		boolean printOutput = getBooleanSetting( "printOutput", false, System.err );




		ParseTree parseTree = null;

		try
		{
			InputStream inputStream = new FileInputStream( sourceFile );
			Reader inputReader = new InputStreamReader( inputStream );
			BufferedReader bufReader = new BufferedReader( new NewlineConvertingReader( inputReader ) );

			// Turn the input into a parse tree.
			CharScanner lexer = language.createLexer( bufReader );
			lexer.setTokenObjectClass( "gbmvdd.bse.BSEToken" );
			WhitespaceCollapsingTokenFilter wsFilter = new WhitespaceCollapsingTokenFilter( lexer );
			BSEParser parser = language.createParser( wsFilter );
			wsFilter.hide( parser.getIgnoredTokens() );
			parseTree = parser.parseEntireStream();

			bufReader.close();
			inputReader.close();
			inputStream.close();
		}
		catch( IOException e )
		{
			System.err.println( "Error reading from source file." );
			System.exit( 5 );
		}
		
		long maxLen = sourceFile.length();
		System.out.println( "maxLen = " + maxLen + "; parseTree has " +
			parseTree.getCharCount() + " characters in." );



		int run = 0;

		int offset = -2;
		int line = -2;
		int lineOffset = -2;
		int lineOffsetLine = -2;
		int lineOffsetLineOffset = -2;

		try
		{
			for( run = 0; run < numRuns; run++ )
			{
				offset = (forcedLocation == -1) ? (int)(Math.random() * maxLen) : forcedLocation;

				line = parseTree.getLineNumberForOffset( offset );
				lineOffset = parseTree.getOffsetForLineNumber( line );
				lineOffsetLine = parseTree.getLineNumberForOffset( lineOffset );
				lineOffsetLineOffset = parseTree.getOffsetForLineNumber( lineOffsetLine );

				boolean passed =
					(line != -1) &&
					(lineOffset != -1 ) &&
					(line == lineOffsetLine) &&
					(lineOffset == lineOffsetLineOffset) &&
					true;

				if( !passed )
				{
					System.err.println( "Run " + run + " failed:" );
					System.err.println( "offset = " + offset );
					System.err.println( "line = " + line );
					System.err.println( "lineOffset = " + lineOffset );
					System.err.println( "lineOffsetLine = " + lineOffsetLine );
					System.err.println( "lineOffsetLineOffset = " + lineOffsetLineOffset );
				}
				else
					System.out.print( "." );
			}
		}
		catch( RuntimeException e )
		{
			System.err.println( "Error on run " + run + ":" );
			System.err.println( "offset = " + offset );
			System.err.println( "line = " + line );
			System.err.println( "lineOffset = " + lineOffset );
			System.err.println( "lineOffsetLine = " + lineOffsetLine );
			System.err.println( "lineOffsetLineOffset = " + lineOffsetLineOffset );
			
			throw e;
		}
	}
}