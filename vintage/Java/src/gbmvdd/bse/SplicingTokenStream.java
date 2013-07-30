package gbmvdd.bse;

import antlr.*;

public class SplicingTokenStream implements TokenStream
{
	private TokenStream input;
	private BSEToken spliceStartToken;
	private int spliceStartOffset;
	private BSEToken spliceEndToken;
	private int spliceEndOffset;

	private String spliceText;

//	SplicingTokenStreamReader inputReader = null;

	private static final int
		STATE_BEFORESPLICE = 0,
		STATE_DURINGSPLICE = 1,
		STATE_AFTERSPLICE = 2;

	private int state = 0;

	public SplicingTokenStream( TokenStream inStream,
		ParseTreeLocation spliceStartLocation,
		ParseTreeLocation spliceEndLocation, String textToSplice )
	{
		input = inStream;
		spliceStartToken = spliceStartLocation.getToken();
		spliceStartOffset = spliceStartLocation.getOffsetInToken();
		spliceEndToken = spliceEndLocation.getToken();
		spliceEndOffset = spliceEndLocation.getOffsetInToken();

		spliceText = textToSplice;
	}

	public Token nextToken() throws TokenStreamException
	{
		// TODO: implement this.
		return null;
	}
}