package gbmvdd.bse;

import gbmvdd.util.Log;

import antlr.*;

public abstract class BSELexer extends CharScanner
{
	protected int currentTokenNewlineCount;

	protected Log errLog = null;

	{
		setTokenObjectClass( "gbmvdd.bse.BSEToken" );
	}

// Constructors are the same as for CharScanner.
	public BSELexer() { super(); }
	public BSELexer( InputBuffer cb ) { super( cb ); }
	public BSELexer( LexerSharedInputState sharedState ) { super( sharedState ); }

// Error reporting functions overridden from CharScanner.
	public void reportError(RecognitionException e)
	{
		if( errLog != null ) errLog.log( e );
	}

    public void reportError(String s)
    {
		if( errLog != null ) errLog.log( s );
	}

	public void setLog( Log errLog )
	{
		this.errLog = errLog;
	}

	public abstract int k();
}