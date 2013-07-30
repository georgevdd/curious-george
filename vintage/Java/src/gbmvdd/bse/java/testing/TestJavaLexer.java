package gbmvdd.bse.java.testing;

import java.io.*;
import java.util.*;

import antlr.*;
import gbmvdd.bse.*;
import gbmvdd.bse.java.*;

public class TestJavaLexer
{
	public static void main( String[] args )
	{
		String inputFilename = args[0];
		File inputFile = new File( inputFilename );

		try
		{
			InputStream inputStream = new FileInputStream( inputFile );
			Reader inputReader = new InputStreamReader( inputStream );
			JavaLexer lexer = new JavaLexer( inputReader );
			LogTokenStream logStream = new LogTokenStream( lexer );

			Token tok;
			do
			{
				tok = logStream.nextToken();
			} while( tok.getType() != Token.EOF_TYPE );
		}
		catch( Exception e )
		{
			System.err.println( e );
		}
	}
}