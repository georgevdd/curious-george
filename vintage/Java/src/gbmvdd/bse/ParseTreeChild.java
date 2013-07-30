package gbmvdd.bse;

public interface ParseTreeChild
{
	public int getNewlineCount();
	public int getCharCount();

	public ParseTreeChild getNextSiblingParseTreeChild();

	public ParseTreeLocation getLocationForOffset( int offset );
	public int getOffsetForLineNumber( int lineNumber );
	public int getLineNumberForOffset( int offset );
	
	public ParseTreeLocation getLocationForOffset( int offset, ParseTreeLocation partialLocation );
}
