package gbmvdd.bse;

import java.io.*;

public class NewlineConvertingReader extends FilterReader
{
	Reader in;
	private int mode;
	
	public static final int MODE_IDENTITY = 0;
	public static final int MODE_CRLF_TO_LF = 1;

	public NewlineConvertingReader( Reader in, int conversionMode )
	{
		super( in );
		this.in = in;
		mode = conversionMode;
	}
	
	public NewlineConvertingReader( Reader in )
	{
		this( in, MODE_IDENTITY );
	}
	
	private boolean skipChar( int ch )
	{
		switch( mode )
		{
		case MODE_IDENTITY:
			return false;
		case MODE_CRLF_TO_LF:
			if( ch == '\r' )
				return true;
			return false;
		default:
			return false;
		}
	}
	
	public int read() throws IOException
	{
		int ch;
		
		do
		{
			ch = in.read();
		} while( skipChar( ch ) );
		
		return ch;
	}
	
	public int read( char[] cbuf, int off, int len ) throws IOException
	{
		int i;
		for( i = off; i < off + len; i++ )
		{
			int ch = read();
			if( ch == -1 )
				break;
			
			cbuf[i] = (char)ch;
		}
		
		int charsRead = i - off;
		return ( (charsRead == 0) && (len != 0) ) ? -1 : charsRead;
	}
}