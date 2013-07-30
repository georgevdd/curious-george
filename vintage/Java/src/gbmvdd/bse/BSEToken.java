package gbmvdd.bse;

import antlr.*;

public class BSEToken extends antlr.CommonToken implements ParseTreeChild
{
	// Note: These are not automatically kept consistent
	// because changing the text of the token does not
	// alter them. They rely on additional code in the Lexer
	// to set their values.
	protected int newlineCount;
	protected int charCount;

	/**
	* This reference is used to string hidden tokens along
	* with visible ones.
	*/
	protected BSEToken next;

	public BSEToken() { super(); }
	public BSEToken(String s) { super( s ); }
	public BSEToken(int t, String txt) {
		super( t, txt );
		if( t == Token.EOF_TYPE )
			charCount = 1;
	}

//	public void setNewlineCount( int count ) { newlineCount = count; }
	public int getNewlineCount() { return newlineCount; }

//	public void setCharCount( int count ) { charCount = count; }
	public int getCharCount() { return charCount; }

	public void setText( String s )
	{
		text = s;
		charCount = s.length();

		// Count all the newlines in the text.
		int i = 0;
		while( (i = text.indexOf( '\n', i ) + 1) != 0 )
			++newlineCount;
	}

	public void setType( int type )
	{
		super.setType( type );

		// This is a small hack so that when the cursor is at the very
		// end of the file, it is reported to be at the start of the EOF
		// token, rather than throwing a NullPointerException.
		if( type == Token.EOF_TYPE )
			charCount = 1;
	}

	public void setNextParseTreeChild( BSEToken next ) { this.next = next; }
	public ParseTreeChild getNextSiblingParseTreeChild() { return next; }

	public ParseTreeLocation getLocationForOffset( int offset )
	{
		ParseTreeLocation result = new ParseTreeLocation();
		getLocationForOffset( offset, result );
		return result;
	}
	
	public ParseTreeLocation getLocationForOffset( int offset, ParseTreeLocation partialLocation )
	{
		partialLocation.add( this );
		partialLocation.setOffsetInToken( offset );
		return partialLocation;
	}
	
	public int getOffsetForLineNumber( int lineNumber )
	{
		if( lineNumber == 0 )
			return 0;
		else
		{
			// This returns the index of the character
			// after the newline, as required.
			int j = 0;
			for( int i = 0; i < lineNumber; i++ )
				j = text.indexOf( '\n', j ) + 1;
			return j;
		}
	}
	
	public int getLineNumberForOffset( int offset )
	{
		int newlines = 0;
		int curNewline = -1;
		while( (curNewline = text.indexOf( '\n', curNewline + 1 )) != -1 )
		{
			if( curNewline < offset )
				++newlines;
			else
				return newlines;
		}
		return newlines;
	}
	
	public void paint( BSEView.RenderContext r, int offset )
	{
		r.drawString( this, getText(), offset );
	}
	
	public void paint( BSEView.RenderContext r )
	{
		paint( r, 0 );
	}
}
