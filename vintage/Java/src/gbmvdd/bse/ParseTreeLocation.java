package gbmvdd.bse;

import antlr.*;
import java.util.*;

/**
* Represents a route down from the root of
* a parse tree to a particular token, and
* also a character offset into that token.
*
* Contains ParseTreeChildren each of which
* is the parent of the next one. The last
* item on the list should be a BSEToken.
*/
public class ParseTreeLocation extends Stack
{
	private int offsetInToken;

	public BSEToken getToken()
	{
		return (BSEToken)peek();
	}

	public int getOffsetInToken()
	{
		return offsetInToken;
	}

	public void setOffsetInToken( int offset )
	{
		offsetInToken = offset;
	}

	public int getOffset()
	{
		int result = offsetInToken;

		for( int i = 0; i < (size() - 1); i++ )
		{
			// TODO:
			ParseTree curLevel = (ParseTree)get( i );
			ParseTreeChild nextLevel = (ParseTreeChild)get( i + 1 );

			ParseTreeChild curChild = curLevel.getFirstParseTreeChild();
			while( curChild != nextLevel )
			{
				result += curChild.getCharCount();
				curChild = curChild.getNextSiblingParseTreeChild();
			}
		}

		return result;
	}

	public String toString()
	{
		if( empty() )
			return "[empty]";

		StringBuffer result = new StringBuffer( "[" );

		for( int i = 0; i < (size() - 1); i++ )
			result.append( ((ParseTree)get( i )).getText() + "->" );

		String tokText = getToken().getText();

		// This should only happen for the EOF token.
		if( tokText == null )
			tokText = "<null>";

		result.append( tokText.substring( 0, offsetInToken ) );
		result.append( "#" );
		result.append( tokText.substring( offsetInToken ) );

		return result.toString();
	}

	public TokenStream getTokenStream( boolean includeWhitespace )
	{
		return new ParseTreeTokenStream( (ParseTreeLocation)this.clone(), includeWhitespace );
	}

	public TokenStream getTokenStream() { return getTokenStream( false ); }

	public void discardLevels( int numLevels )
	{
		if( numLevels >= size() )
			throw new IllegalArgumentException( "can't remove last element!" );

		removeRange( 0, numLevels );
	}
}
