package gbmvdd.bse;

import gbmvdd.util.Log;
import antlr.collections.impl.BitSet;
import antlr.*;

/** The superclass of all parsers for BSE languages. This class
 * adds diverse bits of functionality that would otherwise have
 * to be duplicated in every supported language:
 * <ul>
 * <li>The AST node class is set to gbmvdd.bse.ParseTree by default.</li>
 * <li>Support is added for parsing an existing parse tree, using a
 *     ParseTreeTokenBuffer.</li>
 * <li>Three methods are provided for sending error reports to
 *     arbitrary destinations.</li>
 * <li>It is possible to dynamically discover the value of k for a
 *     particular parser, as well as which classes of token are
 *     regarded as whitespace in its language.</li>
 * <li>An interface is defined for parsing an entire compilation unit
 *     without having to know the name of the top-level rule.</li>
 * </ul>
 */
public abstract class BSEParser extends LLkParser
{
/** Error reporting functions send output to this log. */
	protected Log errLog = null;
	
/** This BitSet is returned by getIgnoredTokens, to
 * indicate that no whitespace tokens exist in a language,
 * unless that method is overridden by the language's parser.
 */
	private static final BitSet emptyBitSet = new BitSet();
	
/** If a parser is parsing an existing parse tree, this
 * implementation of the token buffer is used instead of
 * the (final) default implementation.
 */
	protected ParseTreeTokenBuffer parseTreeTokenBuffer = null;

// By default, the parser is expected to return ParseTrees, not ASTs.
	{
		setASTNodeClass( "gbmvdd.bse.ParseTree" );
	}

/** @see antlr.LLkParser#LLkParser( int ) and other constructors.
 * The following constructors only exist so as to pass the ability to
 * invoke LLkParser.(constructor) down to subclasses.
 */
	public BSEParser( int k_ ) { super( k_ ); }
	public BSEParser( ParserSharedInputState state, int k_ ) { super( state, k_ ); }
	public BSEParser( TokenBuffer tokenBuf, int k_ ) { super( tokenBuf, k_ ); }
	public BSEParser( TokenStream lexer, int k_ ) { super( lexer, k_ ); }

/** Constructs a BSEParser that reads input from a ParseTreeTokenBuffer.
 * When matching a rule, if the parser discovers that the TokenBuffer
 * already begins with an instance of that rule, it can take the entire
 * instance out of the TokenBuffer in a single operation.
 */ 
 	public BSEParser( ParseTreeTokenBuffer pttb, int k_ )
	{
		super( (TokenBuffer)null, k_ );
		parseTreeTokenBuffer = pttb;
	}
	
/** The following five methods are overridden so that if the parser
 * was constructed using a ParseTreeTokenBuffer, then the correct
 * source of tokens will be used. This is used because TokenBuffer
 * is a final class.
 */
	public int LA( int k ) throws TokenStreamException
	{
		if( parseTreeTokenBuffer != null )
			return parseTreeTokenBuffer.LA( k );
		else
			return super.LA( k );
	}
	
	public Token LT( int k ) throws TokenStreamException
	{
		if( parseTreeTokenBuffer != null )
			return parseTreeTokenBuffer.LT( k );
		else
			return super.LT( k );		
	}
	
	public void consume()
	{
		if( parseTreeTokenBuffer != null )
			parseTreeTokenBuffer.consume();
		else
			super.consume();		
	}

	public int mark()
	{
		if( parseTreeTokenBuffer != null )
			return parseTreeTokenBuffer.mark();
		else
			return super.mark();		
	}
	
	public void rewind( int pos )
	{
		if( parseTreeTokenBuffer != null )
			parseTreeTokenBuffer.rewind( pos );
		else
			super.rewind( pos );		
	}

	
/** Reports an exception to any listening Log. */
	public void reportError(RecognitionException e)
	{
		if( errLog != null ) errLog.log( e );
	}

/** Sends a String to the Log, if any. */
    public void reportError(String s)
    {
		if( errLog != null ) errLog.log( s );
	}

/** Sets the Log for this parser. Any exceptions raised or
 * diagnostics produced during parsing will be sent to this log.
 */
	public void setLog( Log errLog )
	{
		this.errLog = errLog;
	}

/** Returns the set of tokens which are whitespace, comments or
 * otherwise transparent to the parser of a language.
 */
	public BitSet getIgnoredTokens() { return emptyBitSet; }
	
/** Invokes the method of a parser corresponding to the start
 * symbol of its language.
 *
 * @return	the ParseTree resulting from parsing every token in
 *			the input stream.
 */
	public abstract ParseTree parseEntireStream();
	
/** Indicates how many tokens of lookahead a parser needs in
 * order to parse any input stream deterministically. (This does
 * not include the arbitrary lookahead required by semantic
 * predicates).
 */
	public abstract int k();



///////////////////
// Optimisations //
///////////////////

	public antlr.collections.AST extractExistingASTFromInput( int astType )
	{
		if( parseTreeTokenBuffer == null )
			return null;
		return null;
	}
}