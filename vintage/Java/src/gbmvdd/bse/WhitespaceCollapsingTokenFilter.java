package gbmvdd.bse;

import antlr.*;

public class WhitespaceCollapsingTokenFilter extends TokenStreamBasicFilter
{
	public WhitespaceCollapsingTokenFilter( TokenStream input )
	{
		super( input );
	}
	
	public WhitespaceCollapsingTokenFilter
		( TokenStream input, antlr.collections.impl.BitSet mask )
	{
		super( input );
		discard( mask );
	}

	public void hide( int ttype )
	{
		discard( ttype );
	}

	public void hide( antlr.collections.impl.BitSet mask )
	{
		discard( mask );
	}

	public Token nextToken() throws TokenStreamException
	{
		BSEToken tok = (BSEToken)input.nextToken();
		BSEToken firstHidden = null;
		BSEToken lastHidden;

		while( (tok != null) && discardMask.member(tok.getType()) )
		{
			if( firstHidden == null )
			{
				firstHidden = tok;
			}

			lastHidden = tok;
	   		tok = (BSEToken)input.nextToken();
	   		lastHidden.setNextParseTreeChild( tok );
		}

		// Link the list into a loop.
		tok.setNextParseTreeChild( (firstHidden == null) ? tok : firstHidden );
		return tok;
    }
}
