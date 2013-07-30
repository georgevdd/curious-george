package gbmvdd.bse;

import java.io.*;

public class SplicingReader extends Reader
{
	private Reader input;
	private int spliceStart;
	private int spliceEnd;
	private String spliceText;
	private int curPos = 0;
	private boolean readingString = false;

	public SplicingReader( Reader inputReader, int spliceStartOffset, int spliceEndOffset, String textToSplice )
	{
		if( spliceStartOffset > spliceEndOffset )
			throw new IllegalArgumentException();

		input = inputReader;
		spliceStart = spliceStartOffset;
		spliceEnd = spliceEndOffset;
		spliceText = textToSplice;
	}


	public int read() throws IOException
	{
		if( curPos == spliceStart )
		{
			// Discard text that's spliced out of the stream.
			for( ; curPos < spliceEnd; ++curPos )
				input.read();

			spliceStart = -1;
			curPos = 0;

			readingString = true;
		}

		if( readingString )
		{
			if( curPos >= spliceText.length() )
				readingString = false;
			else
				return spliceText.charAt( curPos++ );
		}

		++curPos;
		return input.read();
	}


	// Obligatory methods

	public int read( char[] cbuf, int off, int len ) throws IOException
	{
		int i;
		for( i = 0; i < len; i++ )
		{
			int nextChar = read();
			if( nextChar == -1 )
				return (i == 0) ? -1 : i;
			cbuf[off + i] = (char)nextChar;
		}

		return i;
	}

	public void close() throws IOException
	{
		input.close();
	}
}
