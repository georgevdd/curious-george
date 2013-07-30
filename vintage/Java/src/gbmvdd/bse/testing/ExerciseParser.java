package gbmvdd.bse.testing;

import java.io.*;
import java.util.*;

import antlr.*;
import gbmvdd.bse.*;

public class ExerciseParser
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

		String languageName = getStringSetting( "language", "gbmvdd.bse.testing.TinyC", System.err ) + "Language";
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

		long forcedStart = (long)(getIntSetting( "forcedStart", -1, System.err ));
		long forcedEnd = (long)(getIntSetting( "forcedEnd", -1, System.err ));
		if( (forcedStart != -1) && (forcedEnd != -1) )
			numRuns = 1;

		boolean printInput = getBooleanSetting( "printInput", false, System.err );
		boolean printOutput = getBooleanSetting( "printOutput", false, System.err );

		for( int run = 0; run < numRuns; run++ )
		{
			long maxLen = sourceFile.length();
			long endOffset = (forcedEnd == -1) ? (long)(Math.random() * maxLen) : forcedEnd;
			long startOffset = (forcedStart == -1) ? (long)(Math.random() * endOffset) : forcedStart;

			char[] inputArray = new char[(int)(endOffset - startOffset)];

			try
			{
				InputStream inputStream = new FileInputStream( sourceFile );
				Reader inputReader = new InputStreamReader( inputStream );
				inputReader.read( inputArray, 0, inputArray.length );
				inputReader.close();
				inputStream.close();
			}
			catch( IOException e )
			{
				System.err.println( "Error reading from source file on trial " + run + "." );
				System.exit( 5 );
			}

			String input = new String( inputArray );

			// Turn the input into a parse tree.
			CharScanner lexer = language.createLexer( new StringReader( input ) );
			lexer.setTokenObjectClass( "gbmvdd.bse.BSEToken" );
			WhitespaceCollapsingTokenFilter wsFilter = new WhitespaceCollapsingTokenFilter( lexer );
			BSEParser parser = language.createParser( wsFilter );
			wsFilter.hide( parser.getIgnoredTokens() );
			ParseTree parseTree = parser.parseEntireStream();

			// Turn the parse tree back into characters.
//			TokenStream astOut = new WhitespaceUncollapsingTokenFilter( parseTree.getTokenStream() );
//			TokenStream astOut = new LogTokenStream( new WhitespaceUncollapsingTokenFilter( parseTree.getTokenStream() ) );
			TokenStream astOut = parseTree.getTokenStream( true );
			Reader astReader = new TokenStreamReader( astOut );

			StringBuffer s = new StringBuffer();
			int nextChar;

			try
			{
				while( (nextChar = astReader.read()) != -1 )
					s.append( (char)nextChar );
			}
			catch( IOException e )
			{
				System.err.println( "Error writing characters to StringBuffer." );
				System.exit( 6 );
			}

			String output = s.toString();

			if( output.compareTo( input ) != 0 )
			{
				System.out.println( "start = " + startOffset +
					"; end = " + endOffset + " (trial " + run + ")." );
				if( printInput )
					System.out.println( "Input:  -->" + input + "<--" );
				if( printOutput )
					System.out.println( "Output: -->" + output + "<--" );
			}
			else
				System.out.print( "." );

/*			Token tok;
				try
				{
					do
					{
						tok = lexer.nextToken();
						System.out.println( tok );
					} while( tok.getType() != Token.EOF_TYPE );
				}
				catch( TokenStreamException e )
				{ System.out.println( "Boo!" ); }
*/		}
	}
}