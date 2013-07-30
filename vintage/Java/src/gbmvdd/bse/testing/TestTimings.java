package gbmvdd.bse.testing;

import java.util.*;
import java.io.*;
import gbmvdd.bse.*;

public class TestTimings
{
	int trialsPerSizePerDoc = 5;
	
	String[] sources =
	{
		"ParseTreeChild.java",
		"SimpleTokenColourer.java",
		"ParseTreeTokenStream.java",
		"ParseTreeTokenBuffer.java",
		"BSE.java",
		"BSEView.java",
		"java/JavaLexer.java",
		"java/JavaParser.java"		
	};
	
	String resultFilename = "timings.txt";
	
	BSEModel model;
	
	public void run() throws IOException
	{
		Random r = new Random();
		
		PrintWriter results = new PrintWriter( new OutputStreamWriter( new FileOutputStream( resultFilename ) ) );
		
		for( int src = 0; src < sources.length; src++ )
		{
			File srcFile = new File( sources[src] );
			int srcLen = (int)srcFile.length();
			
			System.out.println( sources[src] + ": " );
			
			for( int logSize = 0; (1 << logSize) < srcLen; logSize++ )
			{
				r.setSeed( 1475439954 );
				
				for( int trial = 0; trial < trialsPerSizePerDoc; trial++ )
				{
					Reader srcReader = new InputStreamReader( new FileInputStream( srcFile ) );
					model.load( srcReader );
					srcReader.close();
					
					int editSize = (1 << logSize);
					int editStart = r.nextInt( srcLen - editSize );
					
					long startTime = System.currentTimeMillis();
					model.spliceText( editStart, editStart + editSize, "" );
					long endTime = System.currentTimeMillis();
					
					long editTime = endTime - startTime;
					
					String resultString = "" + srcLen + ", " + editSize + "," + editTime;
					results.println( resultString );
					
					System.out.print( "." );
				}
				
				System.out.print( "|" );
				results.flush();
			}
		}
		
		results.flush();
		results.close();
	}
	
	public TestTimings() throws Exception
	{
		model = new BSEModel( Util.instantiateLanguage
			( "gbmvdd.bse.java.Language" ) );
	}
	
	public static void main( String[] args ) throws Exception
	{
		new TestTimings().run();
	}
}