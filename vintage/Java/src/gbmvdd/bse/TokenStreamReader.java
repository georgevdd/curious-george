package gbmvdd.bse;

import java.io.*;
import antlr.*;

public class TokenStreamReader extends Reader
{
	private TokenStream in;
	private Token curToken = null;
	private String curTokenText = "";
	private int textLen = 0;
	private int offset = 0;
	private boolean finished = false;

	public TokenStreamReader( TokenStream in )
	{
		this.in = in;

		if( in instanceof ParseTreeTokenStream )
		{
			offset = ((ParseTreeTokenStream)in).path.getOffsetInToken();

			try
			{
				curToken = in.nextToken();
				if( curToken.getType() == Token.EOF_TYPE )
					finished = true;
				else
				{
					curTokenText = curToken.getText();
					textLen = curTokenText.length();
				}
			}
			catch( TokenStreamException e )
			{
				// Reset things to how they were when
				// the stream was constructed, to give
				// slightly well-defined behaviour if this
				// method is called again.
				curToken = null;
				curTokenText = null;
				textLen = 0;
				offset = 0;
			}
		}
	}
	
	public int read( char[] cbuf, int off, int len ) throws IOException
	{
		if( finished )
			return -1;

		int charsLeft = len;

		while( charsLeft > 0 )
		{
			// This test must be strictly less than, because
			// otherwise wrong things will happen if a number
			// of chars is read that advances exactly to the
			// end of the token stream.
			if( charsLeft < (textLen - offset) )
			{
				// All remaining characters can be supplied
				// from the text of the current token.
				curTokenText.getChars( offset, offset + charsLeft, cbuf, off );
				offset += charsLeft;
				// No need to update off, because we're about to fall out
				// of the loop.
				charsLeft = 0;
			}
			else
			{
				// The whole of the remaining text of the current
				// token is needed.
				curTokenText.getChars( offset, textLen, cbuf, off );
				off += (textLen - offset);
				charsLeft -= (textLen - offset);
				// No need to update offset, because we're about to either
				// overwrite it or return.

				// Pull the next token in and loop
				try
				{
					curToken = in.nextToken();
					if( curToken.getType() == Token.EOF_TYPE )
					{
						// Any further calls to this method should return -1.
						// We shouldn't just return -1 here, as this might not
						// be the first time round the loop and to be
						// consistent with InputStreamReader's behaviour we
						// ought in that case to return the number of characters
						// read.
						finished = true;
						break;
					}

					curTokenText = curToken.getText();
					textLen = curTokenText.length();
					offset = 0;
				}
				catch( TokenStreamException e )
				{
					// Reset things to how they were when
					// the stream was constructed, to give
					// slightly well-defined behaviour if this
					// method is called again.
					curToken = null;
					curTokenText = null;
					textLen = 0;
					offset = 0;

					throw new IOException( e.toString() );
				}
			}
		}

		int charsRead = (len - charsLeft);
		return ((charsRead == 0) && (len != 0)) ? -1 : charsRead;
	}

	public void close()
	{
		// Do nothing.
	}

	public boolean isAtTokenStart()
	{
		return (offset == 0);
	}
}