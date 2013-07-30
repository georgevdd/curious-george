package gbmvdd.bse;

import antlr.*;

public class LogTokenStream implements TokenStream
{
	private TokenStream tStream;

	public LogTokenStream( TokenStream streamToBeLogged )
	{
		tStream = streamToBeLogged;
	}

	public Token nextToken() throws TokenStreamException
	{
		Token result = tStream.nextToken();
		if( result instanceof BSEToken )
			System.err.println( "Token: " + result + " [" +
				((BSEToken)result).getNewlineCount() + "/" +
				((BSEToken)result).getCharCount() + "]" );
		else
			System.err.println( "Token: " + result );
		return result;
	}
}