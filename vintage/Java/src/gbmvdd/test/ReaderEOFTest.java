package gbmvdd.test;

import java.io.*;

public class ReaderEOFTest
{
	public static void main( String[] args )
	{
		char[] c = new char[5];

		try
		{
			FileInputStream inIS = new FileInputStream( args[0] );
			Reader in = new InputStreamReader( inIS );

			while( true )
			{
				int retVal = in.read( c, 0, 5 );
				if( retVal == -1 )
					break;
				System.out.println( "Read " + retVal + " chars: ->" +
									new String( c, 0, retVal ) + "<- " );
			}
		}
		catch( Exception e )
		{
			System.out.println( e );
		}
	}
}