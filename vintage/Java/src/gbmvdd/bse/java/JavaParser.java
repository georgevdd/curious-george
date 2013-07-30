// $ANTLR 2.7.1: "java.g" -> "JavaParser.java"$
// GBMvdD's BSE code generator made this.

package gbmvdd.bse.java;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;
import antlr.collections.AST;
import antlr.ASTPair;
import antlr.collections.impl.ASTArray;

import gbmvdd.bse.*;

/** Java 1.2 Recognizer
 *
 * Run 'java Main <directory full of java files>'
 *
 * Contributing authors:
 *		John Mitchell		johnm@non.net
 *		Terence Parr		parrt@magelang.com
 *		John Lilley			jlilley@empathy.com
 *		Scott Stanchfield	thetick@magelang.com
 *		Markus Mohnen       mohnen@informatik.rwth-aachen.de
 *		Peter Williams		pwilliams@netdynamics.com
 *
 * Version 1.00 December 9, 1997 -- initial release
 * Version 1.01 December 10, 1997
 *		fixed bug in octal def (0..7 not 0..8)
 * Version 1.10 August 1998 (parrt)
 *		added tree construction
 *		fixed definition of WS,comments for mac,pc,unix newlines
 *		added unary plus
 * Version 1.11 (Nov 20, 1998)
 *		Added "shutup" option to turn off last ambig warning.
 *		Fixed inner class def to allow named class defs as statements
 *		synchronized requires compound not simple statement
 *		add [] after builtInType DOT class in primaryExpression
 *		"const" is reserved but not valid..removed from modifiers
 * Version 1.12 (Feb 2, 1999)
 *		Changed LITERAL_xxx to xxx in tree grammar.
 *		Updated java.g to use tokens {...} now for 2.6.0 (new feature).
 *
 * Version 1.13 (Apr 23, 1999)
 *		Didn't have (stat)? for else clause in tree parser.
 *		Didn't gen ASTs for interface extends.  Updated tree parser too.
 *		Updated to 2.6.0.
 * Version 1.14 (Jun 20, 1999)
 *		Allowed final/abstract on local classes.
 *		Removed local interfaces from methods
 *		Put instanceof precedence where it belongs...in relationalExpr
 *			It also had expr not type as arg; fixed it.
 *		Missing ! on SEMI in classBlock
 *		fixed: (expr) + "string" was parsed incorrectly (+ as unary plus).
 *		fixed: didn't like Object[].class in parser or tree parser
 * Version 1.15 (Jun 26, 1999)
 
 *		Screwed up rule with instanceof in it. :(  Fixed.
 *		Tree parser didn't like (expr).something; fixed.
 *		Allowed multiple inheritance in tree grammar. oops.
 * Version 1.16 (August 22, 1999)
 *		Extending an interface built a wacky tree: had extra EXTENDS.
 *		Tree grammar didn't allow multiple superinterfaces.
 *		Tree grammar didn't allow empty var initializer: {}
 * Version 1.17 (October 12, 1999)
 *		ESC lexer rule allowed 399 max not 377 max.
 *		java.tree.g didn't handle the expression of synchronized
 *			statements.
 *
 * Version tracking now done with following ID:
 *
 * $Id: java.g,v 1.4 2002/02/20 17:27:20 gbmvdd Exp $
 *
 * BUG:
 * 		Doesn't like boolean.class!
 *
 * class Test {
 *   public static void main( String args[] ) {
 *     if (boolean.class.equals(boolean.class)) {
 *       System.out.println("works");
 *     }
 *   }
 * }
 *
 * This grammar is in the PUBLIC DOMAIN
 *
 *
 * GBMvdD 14/01/2002 removed all !'s and ^'s from the grammar. Also
 * made things refer to declaratorBrackets where they had previously
 * duplicated its definition.
 */
public class JavaParser extends gbmvdd.bse.BSEParser
       implements JavaTokenTypes
 {



	public gbmvdd.bse.ParseTree parseEntireStream()
	{
		try
		{
			compilationUnit();
			return (gbmvdd.bse.ParseTree)getAST();
		}
		catch( Exception e )
		{
			return null;
		}
	}


	public JavaParser( ParseTreeTokenBuffer pttb, int k_ )
	{
		super( pttb, k_ );
	}

	public int k() { return 2; }


protected JavaParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public JavaParser(TokenBuffer tokenBuf) {
  this(tokenBuf,2);
}

protected JavaParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public JavaParser(TokenStream lexer) {
  this(lexer,2);
}

public JavaParser(ParserSharedInputState state) {
  super(state,2);
  tokenNames = _tokenNames;
}

	public BitSet getIgnoredTokens()
	{
		return _tokenSet_0;
	}
	

	public final void compilationUnit() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST compilationUnit_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10001 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case PACKAGE_DEF:
			{
				packageDefinition();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case EOF:
			case IMPORT:
			case FINAL:
			case ABSTRACT:
			case SEMI:
			case LITERAL_private:
			case LITERAL_public:
			case LITERAL_protected:
			case LITERAL_static:
			case LITERAL_transient:
			case LITERAL_native:
			case LITERAL_threadsafe:
			case LITERAL_synchronized:
			case LITERAL_volatile:
			case LITERAL_class:
			case LITERAL_interface:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			{
			_loop5:
			do {
				if ((LA(1)==IMPORT)) {
					importDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop5;
				}
				
			} while (true);
			}
			{
			_loop7:
			do {
				if ((_tokenSet_1.member(LA(1)))) {
					typeDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop7;
				}
				
			} while (true);
			}
			Token unlabelledAtom1 = LT(1);
			AST unlabelledAtom1_AST = null;
			match(Token.EOF_TYPE);
			if (inputState.guessing==0) {
				unlabelledAtom1_AST = (AST)astFactory.create(unlabelledAtom1);
				astFactory.addASTChild(currentAST, unlabelledAtom1_AST);
			}
			compilationUnit_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_2.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				Token leftoverToken;
				do
				{
					leftoverToken = LT( 1 );
					AST leftover_AST = (AST)astFactory.create( leftoverToken );
					astFactory.addASTChild( currentAST, leftover_AST );
					consume();
				} while( leftoverToken.getType() != Token.EOF_TYPE );
				compilationUnit_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 1, "compilationUnit" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( compilationUnit_AST );
		returnAST.setType( 10001 );
	}
	
	public final void packageDefinition() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST packageDefinition_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10002 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom2 = LT(1);
			AST unlabelledAtom2_AST = null;
			match(PACKAGE_DEF);
			if (inputState.guessing==0) {
				unlabelledAtom2_AST = (AST)astFactory.create(unlabelledAtom2);
				astFactory.addASTChild(currentAST, unlabelledAtom2_AST);
			}
			identifier();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom3 = LT(1);
			AST unlabelledAtom3_AST = null;
			match(SEMI);
			if (inputState.guessing==0) {
				unlabelledAtom3_AST = (AST)astFactory.create(unlabelledAtom3);
				astFactory.addASTChild(currentAST, unlabelledAtom3_AST);
			}
			packageDefinition_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_3.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				packageDefinition_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 2, "packageDefinition" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( packageDefinition_AST );
		returnAST.setType( 10002 );
	}
	
	public final void importDefinition() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST importDefinition_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10003 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom4 = LT(1);
			AST unlabelledAtom4_AST = null;
			match(IMPORT);
			if (inputState.guessing==0) {
				unlabelledAtom4_AST = (AST)astFactory.create(unlabelledAtom4);
				astFactory.addASTChild(currentAST, unlabelledAtom4_AST);
			}
			identifierStar();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom5 = LT(1);
			AST unlabelledAtom5_AST = null;
			match(SEMI);
			if (inputState.guessing==0) {
				unlabelledAtom5_AST = (AST)astFactory.create(unlabelledAtom5);
				astFactory.addASTChild(currentAST, unlabelledAtom5_AST);
			}
			importDefinition_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_3.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				importDefinition_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 3, "importDefinition" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( importDefinition_AST );
		returnAST.setType( 10003 );
	}
	
	public final void typeDefinition() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST typeDefinition_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10004 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case FINAL:
			case ABSTRACT:
			case LITERAL_private:
			case LITERAL_public:
			case LITERAL_protected:
			case LITERAL_static:
			case LITERAL_transient:
			case LITERAL_native:
			case LITERAL_threadsafe:
			case LITERAL_synchronized:
			case LITERAL_volatile:
			case LITERAL_class:
			case LITERAL_interface:
			{
				modifiers();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				switch ( LA(1)) {
				case LITERAL_class:
				{
					classDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case LITERAL_interface:
				{
					interfaceDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				typeDefinition_AST = (AST)currentAST.root;
				break;
			}
			case SEMI:
			{
				Token unlabelledAtom6 = LT(1);
				AST unlabelledAtom6_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom6_AST = (AST)astFactory.create(unlabelledAtom6);
					astFactory.addASTChild(currentAST, unlabelledAtom6_AST);
				}
				typeDefinition_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_4.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				typeDefinition_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 4, "typeDefinition" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( typeDefinition_AST );
		returnAST.setType( 10004 );
	}
	
	public final void identifier() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST identifier_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10005 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom7 = LT(1);
			AST unlabelledAtom7_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom7_AST = (AST)astFactory.create(unlabelledAtom7);
				astFactory.addASTChild(currentAST, unlabelledAtom7_AST);
			}
			{
			_loop23:
			do {
				if ((LA(1)==DOT)) {
					Token unlabelledAtom8 = LT(1);
					AST unlabelledAtom8_AST = null;
					match(DOT);
					if (inputState.guessing==0) {
						unlabelledAtom8_AST = (AST)astFactory.create(unlabelledAtom8);
						astFactory.addASTChild(currentAST, unlabelledAtom8_AST);
					}
					Token unlabelledAtom9 = LT(1);
					AST unlabelledAtom9_AST = null;
					match(IDENT);
					if (inputState.guessing==0) {
						unlabelledAtom9_AST = (AST)astFactory.create(unlabelledAtom9);
						astFactory.addASTChild(currentAST, unlabelledAtom9_AST);
					}
				}
				else {
					break _loop23;
				}
				
			} while (true);
			}
			identifier_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_5.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				identifier_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 5, "identifier" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( identifier_AST );
		returnAST.setType( 10005 );
	}
	
	public final void identifierStar() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST identifierStar_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10006 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom10 = LT(1);
			AST unlabelledAtom10_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom10_AST = (AST)astFactory.create(unlabelledAtom10);
				astFactory.addASTChild(currentAST, unlabelledAtom10_AST);
			}
			{
			_loop26:
			do {
				if ((LA(1)==DOT) && (LA(2)==IDENT)) {
					Token unlabelledAtom11 = LT(1);
					AST unlabelledAtom11_AST = null;
					match(DOT);
					if (inputState.guessing==0) {
						unlabelledAtom11_AST = (AST)astFactory.create(unlabelledAtom11);
						astFactory.addASTChild(currentAST, unlabelledAtom11_AST);
					}
					Token unlabelledAtom12 = LT(1);
					AST unlabelledAtom12_AST = null;
					match(IDENT);
					if (inputState.guessing==0) {
						unlabelledAtom12_AST = (AST)astFactory.create(unlabelledAtom12);
						astFactory.addASTChild(currentAST, unlabelledAtom12_AST);
					}
				}
				else {
					break _loop26;
				}
				
			} while (true);
			}
			{
			switch ( LA(1)) {
			case DOT:
			{
				Token unlabelledAtom13 = LT(1);
				AST unlabelledAtom13_AST = null;
				match(DOT);
				if (inputState.guessing==0) {
					unlabelledAtom13_AST = (AST)astFactory.create(unlabelledAtom13);
					astFactory.addASTChild(currentAST, unlabelledAtom13_AST);
				}
				Token unlabelledAtom14 = LT(1);
				AST unlabelledAtom14_AST = null;
				match(STAR);
				if (inputState.guessing==0) {
					unlabelledAtom14_AST = (AST)astFactory.create(unlabelledAtom14);
					astFactory.addASTChild(currentAST, unlabelledAtom14_AST);
				}
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			identifierStar_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				identifierStar_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 6, "identifierStar" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( identifierStar_AST );
		returnAST.setType( 10006 );
	}
	
	public final void modifiers() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST modifiers_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10007 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			_loop15:
			do {
				if ((_tokenSet_7.member(LA(1)))) {
					modifier();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop15;
				}
				
			} while (true);
			}
			modifiers_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_8.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				modifiers_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 7, "modifiers" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( modifiers_AST );
		returnAST.setType( 10007 );
	}
	
	public final void classDefinition() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST classDefinition_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10008 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp15_AST = null;
			if (inputState.guessing==0) {
				tmp15_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp15_AST);
			}
			match(LITERAL_class);
			Token unlabelledAtom16 = LT(1);
			AST unlabelledAtom16_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom16_AST = (AST)astFactory.create(unlabelledAtom16);
				astFactory.addASTChild(currentAST, unlabelledAtom16_AST);
			}
			superClassClause();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			implementsClause();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			classBlock();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			classDefinition_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_9.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				classDefinition_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 8, "classDefinition" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( classDefinition_AST );
		returnAST.setType( 10008 );
	}
	
	public final void interfaceDefinition() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST interfaceDefinition_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10009 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp17_AST = null;
			if (inputState.guessing==0) {
				tmp17_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp17_AST);
			}
			match(LITERAL_interface);
			Token unlabelledAtom18 = LT(1);
			AST unlabelledAtom18_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom18_AST = (AST)astFactory.create(unlabelledAtom18);
				astFactory.addASTChild(currentAST, unlabelledAtom18_AST);
			}
			interfaceExtends();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			classBlock();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			interfaceDefinition_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_10.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				interfaceDefinition_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 9, "interfaceDefinition" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( interfaceDefinition_AST );
		returnAST.setType( 10009 );
	}
	
/** A declaration is the creation of a reference or primitive-type variable
 *  Create a separate Type/Var tree for each var in the var list.
 */
	public final void declaration() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST declaration_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10010 );
		if( returnAST != null )
			return;
		try {      // for error handling
			modifiers();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			typeSpec();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			variableDefinitions();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			declaration_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				declaration_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 10, "declaration" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( declaration_AST );
		returnAST.setType( 10010 );
	}
	
	public final void typeSpec() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST typeSpec_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10011 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case IDENT:
			{
				classTypeSpec();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				typeSpec_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			{
				builtInTypeSpec();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				typeSpec_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_11.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				typeSpec_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 11, "typeSpec" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( typeSpec_AST );
		returnAST.setType( 10011 );
	}
	
	public final void variableDefinitions() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST variableDefinitions_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10012 );
		if( returnAST != null )
			return;
		try {      // for error handling
			variableDeclarator();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop51:
			do {
				if ((LA(1)==COMMA)) {
					Token unlabelledAtom19 = LT(1);
					AST unlabelledAtom19_AST = null;
					match(COMMA);
					if (inputState.guessing==0) {
						unlabelledAtom19_AST = (AST)astFactory.create(unlabelledAtom19);
						astFactory.addASTChild(currentAST, unlabelledAtom19_AST);
					}
					variableDeclarator();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop51;
				}
				
			} while (true);
			}
			variableDefinitions_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				variableDefinitions_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 12, "variableDefinitions" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( variableDefinitions_AST );
		returnAST.setType( 10012 );
	}
	
	public final void modifier() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST modifier_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10013 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case LITERAL_private:
			{
				AST tmp20_AST = null;
				if (inputState.guessing==0) {
					tmp20_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp20_AST);
				}
				match(LITERAL_private);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_public:
			{
				AST tmp21_AST = null;
				if (inputState.guessing==0) {
					tmp21_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp21_AST);
				}
				match(LITERAL_public);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_protected:
			{
				AST tmp22_AST = null;
				if (inputState.guessing==0) {
					tmp22_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp22_AST);
				}
				match(LITERAL_protected);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_static:
			{
				AST tmp23_AST = null;
				if (inputState.guessing==0) {
					tmp23_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp23_AST);
				}
				match(LITERAL_static);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_transient:
			{
				AST tmp24_AST = null;
				if (inputState.guessing==0) {
					tmp24_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp24_AST);
				}
				match(LITERAL_transient);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case FINAL:
			{
				AST tmp25_AST = null;
				if (inputState.guessing==0) {
					tmp25_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp25_AST);
				}
				match(FINAL);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case ABSTRACT:
			{
				AST tmp26_AST = null;
				if (inputState.guessing==0) {
					tmp26_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp26_AST);
				}
				match(ABSTRACT);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_native:
			{
				AST tmp27_AST = null;
				if (inputState.guessing==0) {
					tmp27_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp27_AST);
				}
				match(LITERAL_native);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_threadsafe:
			{
				AST tmp28_AST = null;
				if (inputState.guessing==0) {
					tmp28_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp28_AST);
				}
				match(LITERAL_threadsafe);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_synchronized:
			{
				AST tmp29_AST = null;
				if (inputState.guessing==0) {
					tmp29_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp29_AST);
				}
				match(LITERAL_synchronized);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_volatile:
			{
				AST tmp30_AST = null;
				if (inputState.guessing==0) {
					tmp30_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp30_AST);
				}
				match(LITERAL_volatile);
				modifier_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_12.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				modifier_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 13, "modifier" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( modifier_AST );
		returnAST.setType( 10013 );
	}
	
	public final void classTypeSpec() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST classTypeSpec_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10014 );
		if( returnAST != null )
			return;
		try {      // for error handling
			identifier();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			declaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			classTypeSpec_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_11.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				classTypeSpec_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 14, "classTypeSpec" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( classTypeSpec_AST );
		returnAST.setType( 10014 );
	}
	
	public final void builtInTypeSpec() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST builtInTypeSpec_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10015 );
		if( returnAST != null )
			return;
		try {      // for error handling
			builtInType();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			declaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			builtInTypeSpec_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_11.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				builtInTypeSpec_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 15, "builtInTypeSpec" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( builtInTypeSpec_AST );
		returnAST.setType( 10015 );
	}
	
	public final void declaratorBrackets() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST declaratorBrackets_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10016 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			_loop55:
			do {
				if ((LA(1)==LBRACK)) {
					Token unlabelledAtom31 = LT(1);
					AST unlabelledAtom31_AST = null;
					match(LBRACK);
					if (inputState.guessing==0) {
						unlabelledAtom31_AST = (AST)astFactory.create(unlabelledAtom31);
						astFactory.addASTChild(currentAST, unlabelledAtom31_AST);
					}
					Token unlabelledAtom32 = LT(1);
					AST unlabelledAtom32_AST = null;
					match(RBRACK);
					if (inputState.guessing==0) {
						unlabelledAtom32_AST = (AST)astFactory.create(unlabelledAtom32);
						astFactory.addASTChild(currentAST, unlabelledAtom32_AST);
					}
				}
				else {
					break _loop55;
				}
				
			} while (true);
			}
			declaratorBrackets_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_13.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				declaratorBrackets_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 16, "declaratorBrackets" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( declaratorBrackets_AST );
		returnAST.setType( 10016 );
	}
	
	public final void builtInType() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST builtInType_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10017 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case LITERAL_void:
			{
				AST tmp33_AST = null;
				if (inputState.guessing==0) {
					tmp33_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp33_AST);
				}
				match(LITERAL_void);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_boolean:
			{
				AST tmp34_AST = null;
				if (inputState.guessing==0) {
					tmp34_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp34_AST);
				}
				match(LITERAL_boolean);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_byte:
			{
				AST tmp35_AST = null;
				if (inputState.guessing==0) {
					tmp35_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp35_AST);
				}
				match(LITERAL_byte);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_char:
			{
				AST tmp36_AST = null;
				if (inputState.guessing==0) {
					tmp36_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp36_AST);
				}
				match(LITERAL_char);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_short:
			{
				AST tmp37_AST = null;
				if (inputState.guessing==0) {
					tmp37_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp37_AST);
				}
				match(LITERAL_short);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_int:
			{
				AST tmp38_AST = null;
				if (inputState.guessing==0) {
					tmp38_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp38_AST);
				}
				match(LITERAL_int);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_float:
			{
				AST tmp39_AST = null;
				if (inputState.guessing==0) {
					tmp39_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp39_AST);
				}
				match(LITERAL_float);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_long:
			{
				AST tmp40_AST = null;
				if (inputState.guessing==0) {
					tmp40_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp40_AST);
				}
				match(LITERAL_long);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_double:
			{
				AST tmp41_AST = null;
				if (inputState.guessing==0) {
					tmp41_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp41_AST);
				}
				match(LITERAL_double);
				builtInType_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_14.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				builtInType_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 17, "builtInType" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( builtInType_AST );
		returnAST.setType( 10017 );
	}
	
	public final void type() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST type_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10018 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case IDENT:
			{
				identifier();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				type_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			{
				builtInType();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				type_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_15.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				type_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 18, "type" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( type_AST );
		returnAST.setType( 10018 );
	}
	
	public final void superClassClause() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST superClassClause_AST = null;
		AST id_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10019 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_extends:
			{
				AST tmp42_AST = null;
				if (inputState.guessing==0) {
					tmp42_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp42_AST);
				}
				match(LITERAL_extends);
				identifier();
				if (inputState.guessing==0) {
					id_AST = (AST)returnAST;
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case LCURLY:
			case LITERAL_implements:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			superClassClause_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_16.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				superClassClause_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 19, "superClassClause" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( superClassClause_AST );
		returnAST.setType( 10019 );
	}
	
	public final void implementsClause() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST implementsClause_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10020 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_implements:
			{
				AST tmp43_AST = null;
				if (inputState.guessing==0) {
					tmp43_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp43_AST);
				}
				match(LITERAL_implements);
				identifier();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop43:
				do {
					if ((LA(1)==COMMA)) {
						Token unlabelledAtom44 = LT(1);
						AST unlabelledAtom44_AST = null;
						match(COMMA);
						if (inputState.guessing==0) {
							unlabelledAtom44_AST = (AST)astFactory.create(unlabelledAtom44);
							astFactory.addASTChild(currentAST, unlabelledAtom44_AST);
						}
						identifier();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop43;
					}
					
				} while (true);
				}
				break;
			}
			case LCURLY:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			implementsClause_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_17.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				implementsClause_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 20, "implementsClause" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( implementsClause_AST );
		returnAST.setType( 10020 );
	}
	
	public final void classBlock() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST classBlock_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10021 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom45 = LT(1);
			AST unlabelledAtom45_AST = null;
			match(LCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom45_AST = (AST)astFactory.create(unlabelledAtom45);
				astFactory.addASTChild(currentAST, unlabelledAtom45_AST);
			}
			{
			_loop35:
			do {
				switch ( LA(1)) {
				case FINAL:
				case ABSTRACT:
				case LITERAL_void:
				case LITERAL_boolean:
				case LITERAL_byte:
				case LITERAL_char:
				case LITERAL_short:
				case LITERAL_int:
				case LITERAL_float:
				case LITERAL_long:
				case LITERAL_double:
				case IDENT:
				case LITERAL_private:
				case LITERAL_public:
				case LITERAL_protected:
				case LITERAL_static:
				case LITERAL_transient:
				case LITERAL_native:
				case LITERAL_threadsafe:
				case LITERAL_synchronized:
				case LITERAL_volatile:
				case LITERAL_class:
				case LITERAL_interface:
				case LCURLY:
				{
					field();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case SEMI:
				{
					Token unlabelledAtom46 = LT(1);
					AST unlabelledAtom46_AST = null;
					match(SEMI);
					if (inputState.guessing==0) {
						unlabelledAtom46_AST = (AST)astFactory.create(unlabelledAtom46);
						astFactory.addASTChild(currentAST, unlabelledAtom46_AST);
					}
					break;
				}
				default:
				{
					break _loop35;
				}
				}
			} while (true);
			}
			Token unlabelledAtom47 = LT(1);
			AST unlabelledAtom47_AST = null;
			match(RCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom47_AST = (AST)astFactory.create(unlabelledAtom47);
				astFactory.addASTChild(currentAST, unlabelledAtom47_AST);
			}
			classBlock_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_18.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				classBlock_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 21, "classBlock" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( classBlock_AST );
		returnAST.setType( 10021 );
	}
	
	public final void interfaceExtends() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST interfaceExtends_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10022 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_extends:
			{
				AST tmp48_AST = null;
				if (inputState.guessing==0) {
					tmp48_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp48_AST);
				}
				match(LITERAL_extends);
				identifier();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop39:
				do {
					if ((LA(1)==COMMA)) {
						Token unlabelledAtom49 = LT(1);
						AST unlabelledAtom49_AST = null;
						match(COMMA);
						if (inputState.guessing==0) {
							unlabelledAtom49_AST = (AST)astFactory.create(unlabelledAtom49);
							astFactory.addASTChild(currentAST, unlabelledAtom49_AST);
						}
						identifier();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop39;
					}
					
				} while (true);
				}
				break;
			}
			case LCURLY:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			interfaceExtends_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_17.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				interfaceExtends_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 22, "interfaceExtends" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( interfaceExtends_AST );
		returnAST.setType( 10022 );
	}
	
	public final void field() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST field_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10023 );
		if( returnAST != null )
			return;
		try {      // for error handling
			if ((_tokenSet_12.member(LA(1))) && (_tokenSet_19.member(LA(2)))) {
				modifiers();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				switch ( LA(1)) {
				case LITERAL_class:
				{
					classDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case LITERAL_interface:
				{
					interfaceDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				default:
					if ((LA(1)==IDENT) && (LA(2)==LPAREN)) {
						ctorHead();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						compoundStatement();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else if (((LA(1) >= LITERAL_void && LA(1) <= IDENT)) && (_tokenSet_20.member(LA(2)))) {
						typeSpec();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						{
						if ((LA(1)==IDENT) && (LA(2)==LPAREN)) {
							Token unlabelledAtom50 = LT(1);
							AST unlabelledAtom50_AST = null;
							match(IDENT);
							if (inputState.guessing==0) {
								unlabelledAtom50_AST = (AST)astFactory.create(unlabelledAtom50);
								astFactory.addASTChild(currentAST, unlabelledAtom50_AST);
							}
							Token unlabelledAtom51 = LT(1);
							AST unlabelledAtom51_AST = null;
							match(LPAREN);
							if (inputState.guessing==0) {
								unlabelledAtom51_AST = (AST)astFactory.create(unlabelledAtom51);
								astFactory.addASTChild(currentAST, unlabelledAtom51_AST);
							}
							parameterDeclarationList();
							if (inputState.guessing==0) {
								astFactory.addASTChild(currentAST, returnAST);
							}
							Token unlabelledAtom52 = LT(1);
							AST unlabelledAtom52_AST = null;
							match(RPAREN);
							if (inputState.guessing==0) {
								unlabelledAtom52_AST = (AST)astFactory.create(unlabelledAtom52);
								astFactory.addASTChild(currentAST, unlabelledAtom52_AST);
							}
							returnTypeBrackersOnEndOfMethodHead();
							if (inputState.guessing==0) {
								astFactory.addASTChild(currentAST, returnAST);
							}
							{
							switch ( LA(1)) {
							case LITERAL_throws:
							{
								throwsClause();
								if (inputState.guessing==0) {
									astFactory.addASTChild(currentAST, returnAST);
								}
								break;
							}
							case SEMI:
							case LCURLY:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							}
							}
							{
							switch ( LA(1)) {
							case LCURLY:
							{
								compoundStatement();
								if (inputState.guessing==0) {
									astFactory.addASTChild(currentAST, returnAST);
								}
								break;
							}
							case SEMI:
							{
								Token unlabelledAtom53 = LT(1);
								AST unlabelledAtom53_AST = null;
								match(SEMI);
								if (inputState.guessing==0) {
									unlabelledAtom53_AST = (AST)astFactory.create(unlabelledAtom53);
									astFactory.addASTChild(currentAST, unlabelledAtom53_AST);
								}
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							}
							}
						}
						else if ((LA(1)==IDENT) && (_tokenSet_21.member(LA(2)))) {
							variableDefinitions();
							if (inputState.guessing==0) {
								astFactory.addASTChild(currentAST, returnAST);
							}
							Token unlabelledAtom54 = LT(1);
							AST unlabelledAtom54_AST = null;
							match(SEMI);
							if (inputState.guessing==0) {
								unlabelledAtom54_AST = (AST)astFactory.create(unlabelledAtom54);
								astFactory.addASTChild(currentAST, unlabelledAtom54_AST);
							}
						}
						else {
							throw new NoViableAltException(LT(1), getFilename());
						}
						
						}
					}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				field_AST = (AST)currentAST.root;
			}
			else if ((LA(1)==LITERAL_static) && (LA(2)==LCURLY)) {
				AST tmp55_AST = null;
				if (inputState.guessing==0) {
					tmp55_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp55_AST);
				}
				match(LITERAL_static);
				compoundStatement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				field_AST = (AST)currentAST.root;
			}
			else if ((LA(1)==LCURLY)) {
				compoundStatement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				field_AST = (AST)currentAST.root;
			}
			else {
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_22.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				field_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 23, "field" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( field_AST );
		returnAST.setType( 10023 );
	}
	
	public final void ctorHead() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST ctorHead_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10024 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom56 = LT(1);
			AST unlabelledAtom56_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom56_AST = (AST)astFactory.create(unlabelledAtom56);
				astFactory.addASTChild(currentAST, unlabelledAtom56_AST);
			}
			Token unlabelledAtom57 = LT(1);
			AST unlabelledAtom57_AST = null;
			match(LPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom57_AST = (AST)astFactory.create(unlabelledAtom57);
				astFactory.addASTChild(currentAST, unlabelledAtom57_AST);
			}
			parameterDeclarationList();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom58 = LT(1);
			AST unlabelledAtom58_AST = null;
			match(RPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom58_AST = (AST)astFactory.create(unlabelledAtom58);
				astFactory.addASTChild(currentAST, unlabelledAtom58_AST);
			}
			{
			switch ( LA(1)) {
			case LITERAL_throws:
			{
				throwsClause();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case LCURLY:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			ctorHead_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_17.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				ctorHead_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 24, "ctorHead" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( ctorHead_AST );
		returnAST.setType( 10024 );
	}
	
	public final void compoundStatement() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST compoundStatement_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10025 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom59 = LT(1);
			AST unlabelledAtom59_AST = null;
			match(LCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom59_AST = (AST)astFactory.create(unlabelledAtom59);
				astFactory.addASTChild(currentAST, unlabelledAtom59_AST);
			}
			{
			_loop80:
			do {
				if ((_tokenSet_23.member(LA(1)))) {
					statement();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop80;
				}
				
			} while (true);
			}
			Token unlabelledAtom60 = LT(1);
			AST unlabelledAtom60_AST = null;
			match(RCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom60_AST = (AST)astFactory.create(unlabelledAtom60);
				astFactory.addASTChild(currentAST, unlabelledAtom60_AST);
			}
			compoundStatement_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_24.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				compoundStatement_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 25, "compoundStatement" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( compoundStatement_AST );
		returnAST.setType( 10025 );
	}
	
	public final void parameterDeclarationList() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST parameterDeclarationList_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10026 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case FINAL:
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			{
				parameterDeclaration();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop73:
				do {
					if ((LA(1)==COMMA)) {
						Token unlabelledAtom61 = LT(1);
						AST unlabelledAtom61_AST = null;
						match(COMMA);
						if (inputState.guessing==0) {
							unlabelledAtom61_AST = (AST)astFactory.create(unlabelledAtom61);
							astFactory.addASTChild(currentAST, unlabelledAtom61_AST);
						}
						parameterDeclaration();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop73;
					}
					
				} while (true);
				}
				break;
			}
			case RPAREN:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			parameterDeclarationList_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_25.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				parameterDeclarationList_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 26, "parameterDeclarationList" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( parameterDeclarationList_AST );
		returnAST.setType( 10026 );
	}
	
	public final void returnTypeBrackersOnEndOfMethodHead() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST returnTypeBrackersOnEndOfMethodHead_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10027 );
		if( returnAST != null )
			return;
		try {      // for error handling
			declaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			returnTypeBrackersOnEndOfMethodHead_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_26.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				returnTypeBrackersOnEndOfMethodHead_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 27, "returnTypeBrackersOnEndOfMethodHead" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( returnTypeBrackersOnEndOfMethodHead_AST );
		returnAST.setType( 10027 );
	}
	
	public final void throwsClause() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST throwsClause_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10028 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp62_AST = null;
			if (inputState.guessing==0) {
				tmp62_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp62_AST);
			}
			match(LITERAL_throws);
			identifier();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop68:
			do {
				if ((LA(1)==COMMA)) {
					Token unlabelledAtom63 = LT(1);
					AST unlabelledAtom63_AST = null;
					match(COMMA);
					if (inputState.guessing==0) {
						unlabelledAtom63_AST = (AST)astFactory.create(unlabelledAtom63);
						astFactory.addASTChild(currentAST, unlabelledAtom63_AST);
					}
					identifier();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop68;
				}
				
			} while (true);
			}
			throwsClause_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_27.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				throwsClause_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 28, "throwsClause" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( throwsClause_AST );
		returnAST.setType( 10028 );
	}
	
/** Declaration of a variable.  This can be a class/instance variable,
 *   or a local variable in a method
 * It can also include possible initialization.
 */
	public final void variableDeclarator() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST variableDeclarator_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10029 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom64 = LT(1);
			AST unlabelledAtom64_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom64_AST = (AST)astFactory.create(unlabelledAtom64);
				astFactory.addASTChild(currentAST, unlabelledAtom64_AST);
			}
			declaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			varInitializer();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			variableDeclarator_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_28.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				variableDeclarator_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 29, "variableDeclarator" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( variableDeclarator_AST );
		returnAST.setType( 10029 );
	}
	
	public final void varInitializer() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST varInitializer_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10030 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case ASSIGN:
			{
				Token unlabelledAtom65 = LT(1);
				AST unlabelledAtom65_AST = null;
				match(ASSIGN);
				if (inputState.guessing==0) {
					unlabelledAtom65_AST = (AST)astFactory.create(unlabelledAtom65);
					astFactory.addASTChild(currentAST, unlabelledAtom65_AST);
				}
				initializer();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case SEMI:
			case COMMA:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			varInitializer_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_28.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				varInitializer_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 30, "varInitializer" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( varInitializer_AST );
		returnAST.setType( 10030 );
	}
	
	public final void initializer() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST initializer_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10031 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				initializer_AST = (AST)currentAST.root;
				break;
			}
			case LCURLY:
			{
				arrayInitializer();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				initializer_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_29.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				initializer_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 31, "initializer" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( initializer_AST );
		returnAST.setType( 10031 );
	}
	
	public final void arrayInitializer() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST arrayInitializer_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10032 );
		if( returnAST != null )
			return;
		try {      // for error handling
			Token unlabelledAtom66 = LT(1);
			AST unlabelledAtom66_AST = null;
			match(LCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom66_AST = (AST)astFactory.create(unlabelledAtom66);
				astFactory.addASTChild(currentAST, unlabelledAtom66_AST);
			}
			{
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LCURLY:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				initializer();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop61:
				do {
					if ((LA(1)==COMMA) && (_tokenSet_30.member(LA(2)))) {
						Token unlabelledAtom67 = LT(1);
						AST unlabelledAtom67_AST = null;
						match(COMMA);
						if (inputState.guessing==0) {
							unlabelledAtom67_AST = (AST)astFactory.create(unlabelledAtom67);
							astFactory.addASTChild(currentAST, unlabelledAtom67_AST);
						}
						initializer();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop61;
					}
					
				} while (true);
				}
				{
				switch ( LA(1)) {
				case COMMA:
				{
					Token unlabelledAtom68 = LT(1);
					AST unlabelledAtom68_AST = null;
					match(COMMA);
					if (inputState.guessing==0) {
						unlabelledAtom68_AST = (AST)astFactory.create(unlabelledAtom68);
						astFactory.addASTChild(currentAST, unlabelledAtom68_AST);
					}
					break;
				}
				case RCURLY:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				break;
			}
			case RCURLY:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			Token unlabelledAtom69 = LT(1);
			AST unlabelledAtom69_AST = null;
			match(RCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom69_AST = (AST)astFactory.create(unlabelledAtom69);
				astFactory.addASTChild(currentAST, unlabelledAtom69_AST);
			}
			arrayInitializer_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_31.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				arrayInitializer_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 32, "arrayInitializer" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( arrayInitializer_AST );
		returnAST.setType( 10032 );
	}
	
	public final void expression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST expression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10033 );
		if( returnAST != null )
			return;
		try {      // for error handling
			assignmentExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			expression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_32.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				expression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 33, "expression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( expression_AST );
		returnAST.setType( 10033 );
	}
	
	public final void parameterDeclaration() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST parameterDeclaration_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10034 );
		if( returnAST != null )
			return;
		try {      // for error handling
			parameterModifier();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			typeSpec();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom70 = LT(1);
			AST unlabelledAtom70_AST = null;
			match(IDENT);
			if (inputState.guessing==0) {
				unlabelledAtom70_AST = (AST)astFactory.create(unlabelledAtom70);
				astFactory.addASTChild(currentAST, unlabelledAtom70_AST);
			}
			parameterDeclaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			parameterDeclaration_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_33.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				parameterDeclaration_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 34, "parameterDeclaration" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( parameterDeclaration_AST );
		returnAST.setType( 10034 );
	}
	
	public final void parameterModifier() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST parameterModifier_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10035 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case FINAL:
			{
				AST tmp71_AST = null;
				if (inputState.guessing==0) {
					tmp71_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp71_AST);
				}
				match(FINAL);
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			parameterModifier_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_34.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				parameterModifier_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 35, "parameterModifier" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( parameterModifier_AST );
		returnAST.setType( 10035 );
	}
	
	public final void parameterDeclaratorBrackets() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST parameterDeclaratorBrackets_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10036 );
		if( returnAST != null )
			return;
		try {      // for error handling
			declaratorBrackets();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			parameterDeclaratorBrackets_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_33.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				parameterDeclaratorBrackets_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 36, "parameterDeclaratorBrackets" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( parameterDeclaratorBrackets_AST );
		returnAST.setType( 10036 );
	}
	
	public final void statement() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST statement_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10037 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case LCURLY:
			{
				compoundStatement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_class:
			{
				classDefinition();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_if:
			{
				AST tmp72_AST = null;
				if (inputState.guessing==0) {
					tmp72_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp72_AST);
				}
				match(LITERAL_if);
				Token unlabelledAtom73 = LT(1);
				AST unlabelledAtom73_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom73_AST = (AST)astFactory.create(unlabelledAtom73);
					astFactory.addASTChild(currentAST, unlabelledAtom73_AST);
				}
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom74 = LT(1);
				AST unlabelledAtom74_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom74_AST = (AST)astFactory.create(unlabelledAtom74);
					astFactory.addASTChild(currentAST, unlabelledAtom74_AST);
				}
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				if ((LA(1)==LITERAL_else) && (_tokenSet_23.member(LA(2)))) {
					AST tmp75_AST = null;
					if (inputState.guessing==0) {
						tmp75_AST = (AST)astFactory.create(LT(1));
						astFactory.addASTChild(currentAST, tmp75_AST);
					}
					match(LITERAL_else);
					statement();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else if ((_tokenSet_35.member(LA(1))) && (_tokenSet_36.member(LA(2)))) {
				}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_for:
			{
				AST tmp76_AST = null;
				if (inputState.guessing==0) {
					tmp76_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp76_AST);
				}
				match(LITERAL_for);
				Token unlabelledAtom77 = LT(1);
				AST unlabelledAtom77_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom77_AST = (AST)astFactory.create(unlabelledAtom77);
					astFactory.addASTChild(currentAST, unlabelledAtom77_AST);
				}
				forInit();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom78 = LT(1);
				AST unlabelledAtom78_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom78_AST = (AST)astFactory.create(unlabelledAtom78);
					astFactory.addASTChild(currentAST, unlabelledAtom78_AST);
				}
				forCond();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom79 = LT(1);
				AST unlabelledAtom79_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom79_AST = (AST)astFactory.create(unlabelledAtom79);
					astFactory.addASTChild(currentAST, unlabelledAtom79_AST);
				}
				forIter();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom80 = LT(1);
				AST unlabelledAtom80_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom80_AST = (AST)astFactory.create(unlabelledAtom80);
					astFactory.addASTChild(currentAST, unlabelledAtom80_AST);
				}
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_while:
			{
				AST tmp81_AST = null;
				if (inputState.guessing==0) {
					tmp81_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp81_AST);
				}
				match(LITERAL_while);
				Token unlabelledAtom82 = LT(1);
				AST unlabelledAtom82_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom82_AST = (AST)astFactory.create(unlabelledAtom82);
					astFactory.addASTChild(currentAST, unlabelledAtom82_AST);
				}
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom83 = LT(1);
				AST unlabelledAtom83_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom83_AST = (AST)astFactory.create(unlabelledAtom83);
					astFactory.addASTChild(currentAST, unlabelledAtom83_AST);
				}
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_do:
			{
				AST tmp84_AST = null;
				if (inputState.guessing==0) {
					tmp84_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp84_AST);
				}
				match(LITERAL_do);
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				AST tmp85_AST = null;
				if (inputState.guessing==0) {
					tmp85_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp85_AST);
				}
				match(LITERAL_while);
				Token unlabelledAtom86 = LT(1);
				AST unlabelledAtom86_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom86_AST = (AST)astFactory.create(unlabelledAtom86);
					astFactory.addASTChild(currentAST, unlabelledAtom86_AST);
				}
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom87 = LT(1);
				AST unlabelledAtom87_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom87_AST = (AST)astFactory.create(unlabelledAtom87);
					astFactory.addASTChild(currentAST, unlabelledAtom87_AST);
				}
				Token unlabelledAtom88 = LT(1);
				AST unlabelledAtom88_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom88_AST = (AST)astFactory.create(unlabelledAtom88);
					astFactory.addASTChild(currentAST, unlabelledAtom88_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_break:
			{
				AST tmp89_AST = null;
				if (inputState.guessing==0) {
					tmp89_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp89_AST);
				}
				match(LITERAL_break);
				{
				switch ( LA(1)) {
				case IDENT:
				{
					Token unlabelledAtom90 = LT(1);
					AST unlabelledAtom90_AST = null;
					match(IDENT);
					if (inputState.guessing==0) {
						unlabelledAtom90_AST = (AST)astFactory.create(unlabelledAtom90);
						astFactory.addASTChild(currentAST, unlabelledAtom90_AST);
					}
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				Token unlabelledAtom91 = LT(1);
				AST unlabelledAtom91_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom91_AST = (AST)astFactory.create(unlabelledAtom91);
					astFactory.addASTChild(currentAST, unlabelledAtom91_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_continue:
			{
				AST tmp92_AST = null;
				if (inputState.guessing==0) {
					tmp92_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp92_AST);
				}
				match(LITERAL_continue);
				{
				switch ( LA(1)) {
				case IDENT:
				{
					Token unlabelledAtom93 = LT(1);
					AST unlabelledAtom93_AST = null;
					match(IDENT);
					if (inputState.guessing==0) {
						unlabelledAtom93_AST = (AST)astFactory.create(unlabelledAtom93);
						astFactory.addASTChild(currentAST, unlabelledAtom93_AST);
					}
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				Token unlabelledAtom94 = LT(1);
				AST unlabelledAtom94_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom94_AST = (AST)astFactory.create(unlabelledAtom94);
					astFactory.addASTChild(currentAST, unlabelledAtom94_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_return:
			{
				AST tmp95_AST = null;
				if (inputState.guessing==0) {
					tmp95_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp95_AST);
				}
				match(LITERAL_return);
				{
				switch ( LA(1)) {
				case LITERAL_void:
				case LITERAL_boolean:
				case LITERAL_byte:
				case LITERAL_char:
				case LITERAL_short:
				case LITERAL_int:
				case LITERAL_float:
				case LITERAL_long:
				case LITERAL_double:
				case IDENT:
				case LPAREN:
				case PLUS:
				case MINUS:
				case INC:
				case DEC:
				case BNOT:
				case LNOT:
				case LITERAL_this:
				case LITERAL_super:
				case LITERAL_true:
				case LITERAL_false:
				case LITERAL_null:
				case LITERAL_new:
				case NUM_INT:
				case CHAR_LITERAL:
				case STRING_LITERAL:
				case NUM_FLOAT:
				{
					expression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				Token unlabelledAtom96 = LT(1);
				AST unlabelledAtom96_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom96_AST = (AST)astFactory.create(unlabelledAtom96);
					astFactory.addASTChild(currentAST, unlabelledAtom96_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_switch:
			{
				AST tmp97_AST = null;
				if (inputState.guessing==0) {
					tmp97_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp97_AST);
				}
				match(LITERAL_switch);
				Token unlabelledAtom98 = LT(1);
				AST unlabelledAtom98_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom98_AST = (AST)astFactory.create(unlabelledAtom98);
					astFactory.addASTChild(currentAST, unlabelledAtom98_AST);
				}
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom99 = LT(1);
				AST unlabelledAtom99_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom99_AST = (AST)astFactory.create(unlabelledAtom99);
					astFactory.addASTChild(currentAST, unlabelledAtom99_AST);
				}
				Token unlabelledAtom100 = LT(1);
				AST unlabelledAtom100_AST = null;
				match(LCURLY);
				if (inputState.guessing==0) {
					unlabelledAtom100_AST = (AST)astFactory.create(unlabelledAtom100);
					astFactory.addASTChild(currentAST, unlabelledAtom100_AST);
				}
				{
				_loop89:
				do {
					if ((LA(1)==LITERAL_case||LA(1)==LITERAL_default)) {
						casesGroup();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop89;
					}
					
				} while (true);
				}
				Token unlabelledAtom101 = LT(1);
				AST unlabelledAtom101_AST = null;
				match(RCURLY);
				if (inputState.guessing==0) {
					unlabelledAtom101_AST = (AST)astFactory.create(unlabelledAtom101);
					astFactory.addASTChild(currentAST, unlabelledAtom101_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_try:
			{
				tryBlock();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_throw:
			{
				AST tmp102_AST = null;
				if (inputState.guessing==0) {
					tmp102_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp102_AST);
				}
				match(LITERAL_throw);
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom103 = LT(1);
				AST unlabelledAtom103_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom103_AST = (AST)astFactory.create(unlabelledAtom103);
					astFactory.addASTChild(currentAST, unlabelledAtom103_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case SEMI:
			{
				Token unlabelledAtom104 = LT(1);
				AST unlabelledAtom104_AST = null;
				match(SEMI);
				if (inputState.guessing==0) {
					unlabelledAtom104_AST = (AST)astFactory.create(unlabelledAtom104);
					astFactory.addASTChild(currentAST, unlabelledAtom104_AST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			default:
				if ((LA(1)==FINAL) && (LA(2)==LITERAL_class)) {
					AST tmp105_AST = null;
					if (inputState.guessing==0) {
						tmp105_AST = (AST)astFactory.create(LT(1));
						astFactory.addASTChild(currentAST, tmp105_AST);
					}
					match(FINAL);
					classDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					statement_AST = (AST)currentAST.root;
				}
				else if ((LA(1)==ABSTRACT) && (LA(2)==LITERAL_class)) {
					AST tmp106_AST = null;
					if (inputState.guessing==0) {
						tmp106_AST = (AST)astFactory.create(LT(1));
						astFactory.addASTChild(currentAST, tmp106_AST);
					}
					match(ABSTRACT);
					classDefinition();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					statement_AST = (AST)currentAST.root;
				}
				else {
					boolean synPredMatched83 = false;
					if (((_tokenSet_37.member(LA(1))) && (_tokenSet_38.member(LA(2))))) {
						int _m83 = mark();
						synPredMatched83 = true;
						inputState.guessing++;
						try {
							{
							declaration();
							}
						}
						catch (RecognitionException pe) {
							synPredMatched83 = false;
						}
						rewind(_m83);
						inputState.guessing--;
					}
					if ( synPredMatched83 ) {
						declaration();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						Token unlabelledAtom107 = LT(1);
						AST unlabelledAtom107_AST = null;
						match(SEMI);
						if (inputState.guessing==0) {
							unlabelledAtom107_AST = (AST)astFactory.create(unlabelledAtom107);
							astFactory.addASTChild(currentAST, unlabelledAtom107_AST);
						}
						statement_AST = (AST)currentAST.root;
					}
					else if ((_tokenSet_39.member(LA(1))) && (_tokenSet_40.member(LA(2)))) {
						expression();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						Token unlabelledAtom108 = LT(1);
						AST unlabelledAtom108_AST = null;
						match(SEMI);
						if (inputState.guessing==0) {
							unlabelledAtom108_AST = (AST)astFactory.create(unlabelledAtom108);
							astFactory.addASTChild(currentAST, unlabelledAtom108_AST);
						}
						statement_AST = (AST)currentAST.root;
					}
					else if ((LA(1)==IDENT) && (LA(2)==COLON)) {
						Token unlabelledAtom109 = LT(1);
						AST unlabelledAtom109_AST = null;
						match(IDENT);
						if (inputState.guessing==0) {
							unlabelledAtom109_AST = (AST)astFactory.create(unlabelledAtom109);
							astFactory.addASTChild(currentAST, unlabelledAtom109_AST);
						}
						Token unlabelledAtom110 = LT(1);
						AST unlabelledAtom110_AST = null;
						match(COLON);
						if (inputState.guessing==0) {
							unlabelledAtom110_AST = (AST)astFactory.create(unlabelledAtom110);
							astFactory.addASTChild(currentAST, unlabelledAtom110_AST);
						}
						statement();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						statement_AST = (AST)currentAST.root;
					}
					else if ((LA(1)==LITERAL_synchronized) && (LA(2)==LPAREN)) {
						AST tmp111_AST = null;
						if (inputState.guessing==0) {
							tmp111_AST = (AST)astFactory.create(LT(1));
							astFactory.addASTChild(currentAST, tmp111_AST);
						}
						match(LITERAL_synchronized);
						Token unlabelledAtom112 = LT(1);
						AST unlabelledAtom112_AST = null;
						match(LPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom112_AST = (AST)astFactory.create(unlabelledAtom112);
							astFactory.addASTChild(currentAST, unlabelledAtom112_AST);
						}
						expression();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						Token unlabelledAtom113 = LT(1);
						AST unlabelledAtom113_AST = null;
						match(RPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom113_AST = (AST)astFactory.create(unlabelledAtom113);
							astFactory.addASTChild(currentAST, unlabelledAtom113_AST);
						}
						compoundStatement();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						statement_AST = (AST)currentAST.root;
					}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				}}
			}
			catch (RecognitionException ex) {
				if (inputState.guessing==0) {
					reportError(ex);
					// BSE non-discarding error handler
					parseFailed = true;
					ASTPair err_AST = new ASTPair();
					do
					{
						if( LA( 1 ) == Token.EOF_TYPE ) break;
						AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
						astFactory.addASTChild( err_AST, badToken_AST );
						consume();
					} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_35.member( LA( 1 ) )) );
					if( err_AST.child != null )
					{
						AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
						astFactory.makeASTRoot( err_AST, err_AST_root );
						astFactory.addASTChild( currentAST, err_AST.root );
					}
					statement_AST = (AST)currentAST.root;
				} else {
				  throw ex;
				}
			}
			returnAST = astFactory.create( 37, "statement" +
				( parseFailed ? "<bad>" : "" ) );
			returnAST.addChild( statement_AST );
			returnAST.setType( 10037 );
		}
		
	public final void forInit() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST forInit_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10038 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			boolean synPredMatched101 = false;
			if (((_tokenSet_37.member(LA(1))) && (_tokenSet_38.member(LA(2))))) {
				int _m101 = mark();
				synPredMatched101 = true;
				inputState.guessing++;
				try {
					{
					declaration();
					}
				}
				catch (RecognitionException pe) {
					synPredMatched101 = false;
				}
				rewind(_m101);
				inputState.guessing--;
			}
			if ( synPredMatched101 ) {
				declaration();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
			}
			else if ((_tokenSet_39.member(LA(1))) && (_tokenSet_41.member(LA(2)))) {
				expressionList();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
			}
			else if ((LA(1)==SEMI)) {
			}
			else {
				throw new NoViableAltException(LT(1), getFilename());
			}
			
			}
			forInit_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				forInit_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 38, "forInit" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( forInit_AST );
		returnAST.setType( 10038 );
	}
	
	public final void forCond() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST forCond_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10039 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case SEMI:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			forCond_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				forCond_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 39, "forCond" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( forCond_AST );
		returnAST.setType( 10039 );
	}
	
	public final void forIter() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST forIter_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10040 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				expressionList();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case RPAREN:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			forIter_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_25.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				forIter_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 40, "forIter" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( forIter_AST );
		returnAST.setType( 10040 );
	}
	
	public final void casesGroup() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST casesGroup_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10041 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			int _cnt92=0;
			_loop92:
			do {
				if ((LA(1)==LITERAL_case||LA(1)==LITERAL_default) && (_tokenSet_42.member(LA(2)))) {
					aCase();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					if ( _cnt92>=1 ) { break _loop92; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt92++;
			} while (true);
			}
			caseSList();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			casesGroup_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_43.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				casesGroup_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 41, "casesGroup" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( casesGroup_AST );
		returnAST.setType( 10041 );
	}
	
	public final void tryBlock() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST tryBlock_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10042 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp114_AST = null;
			if (inputState.guessing==0) {
				tmp114_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp114_AST);
			}
			match(LITERAL_try);
			compoundStatement();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop108:
			do {
				if ((LA(1)==LITERAL_catch)) {
					handler();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop108;
				}
				
			} while (true);
			}
			{
			switch ( LA(1)) {
			case LITERAL_finally:
			{
				AST tmp115_AST = null;
				if (inputState.guessing==0) {
					tmp115_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp115_AST);
				}
				match(LITERAL_finally);
				compoundStatement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case FINAL:
			case ABSTRACT:
			case SEMI:
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LITERAL_private:
			case LITERAL_public:
			case LITERAL_protected:
			case LITERAL_static:
			case LITERAL_transient:
			case LITERAL_native:
			case LITERAL_threadsafe:
			case LITERAL_synchronized:
			case LITERAL_volatile:
			case LITERAL_class:
			case LCURLY:
			case RCURLY:
			case LPAREN:
			case LITERAL_if:
			case LITERAL_else:
			case LITERAL_for:
			case LITERAL_while:
			case LITERAL_do:
			case LITERAL_break:
			case LITERAL_continue:
			case LITERAL_return:
			case LITERAL_switch:
			case LITERAL_throw:
			case LITERAL_case:
			case LITERAL_default:
			case LITERAL_try:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			tryBlock_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_35.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				tryBlock_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 42, "tryBlock" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( tryBlock_AST );
		returnAST.setType( 10042 );
	}
	
	public final void aCase() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST aCase_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10043 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_case:
			{
				AST tmp116_AST = null;
				if (inputState.guessing==0) {
					tmp116_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp116_AST);
				}
				match(LITERAL_case);
				expression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case LITERAL_default:
			{
				AST tmp117_AST = null;
				if (inputState.guessing==0) {
					tmp117_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp117_AST);
				}
				match(LITERAL_default);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			Token unlabelledAtom118 = LT(1);
			AST unlabelledAtom118_AST = null;
			match(COLON);
			if (inputState.guessing==0) {
				unlabelledAtom118_AST = (AST)astFactory.create(unlabelledAtom118);
				astFactory.addASTChild(currentAST, unlabelledAtom118_AST);
			}
			aCase_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_44.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				aCase_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 43, "aCase" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( aCase_AST );
		returnAST.setType( 10043 );
	}
	
	public final void caseSList() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST caseSList_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10044 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			_loop97:
			do {
				if ((_tokenSet_23.member(LA(1)))) {
					statement();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop97;
				}
				
			} while (true);
			}
			caseSList_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_43.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				caseSList_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 44, "caseSList" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( caseSList_AST );
		returnAST.setType( 10044 );
	}
	
	public final void expressionList() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST expressionList_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10045 );
		if( returnAST != null )
			return;
		try {      // for error handling
			expression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop114:
			do {
				if ((LA(1)==COMMA)) {
					Token unlabelledAtom119 = LT(1);
					AST unlabelledAtom119_AST = null;
					match(COMMA);
					if (inputState.guessing==0) {
						unlabelledAtom119_AST = (AST)astFactory.create(unlabelledAtom119);
						astFactory.addASTChild(currentAST, unlabelledAtom119_AST);
					}
					expression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop114;
				}
				
			} while (true);
			}
			expressionList_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_45.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				expressionList_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 45, "expressionList" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( expressionList_AST );
		returnAST.setType( 10045 );
	}
	
	public final void handler() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST handler_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10046 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp120_AST = null;
			if (inputState.guessing==0) {
				tmp120_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp120_AST);
			}
			match(LITERAL_catch);
			Token unlabelledAtom121 = LT(1);
			AST unlabelledAtom121_AST = null;
			match(LPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom121_AST = (AST)astFactory.create(unlabelledAtom121);
				astFactory.addASTChild(currentAST, unlabelledAtom121_AST);
			}
			parameterDeclaration();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom122 = LT(1);
			AST unlabelledAtom122_AST = null;
			match(RPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom122_AST = (AST)astFactory.create(unlabelledAtom122);
				astFactory.addASTChild(currentAST, unlabelledAtom122_AST);
			}
			compoundStatement();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			handler_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_46.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				handler_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 46, "handler" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( handler_AST );
		returnAST.setType( 10046 );
	}
	
	public final void assignmentExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST assignmentExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10047 );
		if( returnAST != null )
			return;
		try {      // for error handling
			conditionalExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			switch ( LA(1)) {
			case ASSIGN:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			{
				{
				switch ( LA(1)) {
				case ASSIGN:
				{
					Token unlabelledAtom123 = LT(1);
					AST unlabelledAtom123_AST = null;
					match(ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom123_AST = (AST)astFactory.create(unlabelledAtom123);
						astFactory.addASTChild(currentAST, unlabelledAtom123_AST);
					}
					break;
				}
				case PLUS_ASSIGN:
				{
					Token unlabelledAtom124 = LT(1);
					AST unlabelledAtom124_AST = null;
					match(PLUS_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom124_AST = (AST)astFactory.create(unlabelledAtom124);
						astFactory.addASTChild(currentAST, unlabelledAtom124_AST);
					}
					break;
				}
				case MINUS_ASSIGN:
				{
					Token unlabelledAtom125 = LT(1);
					AST unlabelledAtom125_AST = null;
					match(MINUS_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom125_AST = (AST)astFactory.create(unlabelledAtom125);
						astFactory.addASTChild(currentAST, unlabelledAtom125_AST);
					}
					break;
				}
				case STAR_ASSIGN:
				{
					Token unlabelledAtom126 = LT(1);
					AST unlabelledAtom126_AST = null;
					match(STAR_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom126_AST = (AST)astFactory.create(unlabelledAtom126);
						astFactory.addASTChild(currentAST, unlabelledAtom126_AST);
					}
					break;
				}
				case DIV_ASSIGN:
				{
					Token unlabelledAtom127 = LT(1);
					AST unlabelledAtom127_AST = null;
					match(DIV_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom127_AST = (AST)astFactory.create(unlabelledAtom127);
						astFactory.addASTChild(currentAST, unlabelledAtom127_AST);
					}
					break;
				}
				case MOD_ASSIGN:
				{
					Token unlabelledAtom128 = LT(1);
					AST unlabelledAtom128_AST = null;
					match(MOD_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom128_AST = (AST)astFactory.create(unlabelledAtom128);
						astFactory.addASTChild(currentAST, unlabelledAtom128_AST);
					}
					break;
				}
				case SR_ASSIGN:
				{
					Token unlabelledAtom129 = LT(1);
					AST unlabelledAtom129_AST = null;
					match(SR_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom129_AST = (AST)astFactory.create(unlabelledAtom129);
						astFactory.addASTChild(currentAST, unlabelledAtom129_AST);
					}
					break;
				}
				case BSR_ASSIGN:
				{
					Token unlabelledAtom130 = LT(1);
					AST unlabelledAtom130_AST = null;
					match(BSR_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom130_AST = (AST)astFactory.create(unlabelledAtom130);
						astFactory.addASTChild(currentAST, unlabelledAtom130_AST);
					}
					break;
				}
				case SL_ASSIGN:
				{
					Token unlabelledAtom131 = LT(1);
					AST unlabelledAtom131_AST = null;
					match(SL_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom131_AST = (AST)astFactory.create(unlabelledAtom131);
						astFactory.addASTChild(currentAST, unlabelledAtom131_AST);
					}
					break;
				}
				case BAND_ASSIGN:
				{
					Token unlabelledAtom132 = LT(1);
					AST unlabelledAtom132_AST = null;
					match(BAND_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom132_AST = (AST)astFactory.create(unlabelledAtom132);
						astFactory.addASTChild(currentAST, unlabelledAtom132_AST);
					}
					break;
				}
				case BXOR_ASSIGN:
				{
					Token unlabelledAtom133 = LT(1);
					AST unlabelledAtom133_AST = null;
					match(BXOR_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom133_AST = (AST)astFactory.create(unlabelledAtom133);
						astFactory.addASTChild(currentAST, unlabelledAtom133_AST);
					}
					break;
				}
				case BOR_ASSIGN:
				{
					Token unlabelledAtom134 = LT(1);
					AST unlabelledAtom134_AST = null;
					match(BOR_ASSIGN);
					if (inputState.guessing==0) {
						unlabelledAtom134_AST = (AST)astFactory.create(unlabelledAtom134);
						astFactory.addASTChild(currentAST, unlabelledAtom134_AST);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				assignmentExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case SEMI:
			case RCURLY:
			case COMMA:
			case RPAREN:
			case RBRACK:
			case COLON:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			assignmentExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_32.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				assignmentExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 47, "assignmentExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( assignmentExpression_AST );
		returnAST.setType( 10047 );
	}
	
	public final void conditionalExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST conditionalExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10048 );
		if( returnAST != null )
			return;
		try {      // for error handling
			logicalOrExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			switch ( LA(1)) {
			case QUESTION:
			{
				Token unlabelledAtom135 = LT(1);
				AST unlabelledAtom135_AST = null;
				match(QUESTION);
				if (inputState.guessing==0) {
					unlabelledAtom135_AST = (AST)astFactory.create(unlabelledAtom135);
					astFactory.addASTChild(currentAST, unlabelledAtom135_AST);
				}
				assignmentExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom136 = LT(1);
				AST unlabelledAtom136_AST = null;
				match(COLON);
				if (inputState.guessing==0) {
					unlabelledAtom136_AST = (AST)astFactory.create(unlabelledAtom136);
					astFactory.addASTChild(currentAST, unlabelledAtom136_AST);
				}
				conditionalExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case SEMI:
			case RCURLY:
			case COMMA:
			case RPAREN:
			case RBRACK:
			case ASSIGN:
			case COLON:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			conditionalExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_47.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				conditionalExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 48, "conditionalExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( conditionalExpression_AST );
		returnAST.setType( 10048 );
	}
	
	public final void logicalOrExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST logicalOrExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10049 );
		if( returnAST != null )
			return;
		try {      // for error handling
			logicalAndExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop122:
			do {
				if ((LA(1)==LOR)) {
					Token unlabelledAtom137 = LT(1);
					AST unlabelledAtom137_AST = null;
					match(LOR);
					if (inputState.guessing==0) {
						unlabelledAtom137_AST = (AST)astFactory.create(unlabelledAtom137);
						astFactory.addASTChild(currentAST, unlabelledAtom137_AST);
					}
					logicalAndExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop122;
				}
				
			} while (true);
			}
			logicalOrExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_48.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				logicalOrExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 49, "logicalOrExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( logicalOrExpression_AST );
		returnAST.setType( 10049 );
	}
	
	public final void logicalAndExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST logicalAndExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10050 );
		if( returnAST != null )
			return;
		try {      // for error handling
			inclusiveOrExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop125:
			do {
				if ((LA(1)==LAND)) {
					Token unlabelledAtom138 = LT(1);
					AST unlabelledAtom138_AST = null;
					match(LAND);
					if (inputState.guessing==0) {
						unlabelledAtom138_AST = (AST)astFactory.create(unlabelledAtom138);
						astFactory.addASTChild(currentAST, unlabelledAtom138_AST);
					}
					inclusiveOrExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop125;
				}
				
			} while (true);
			}
			logicalAndExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_49.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				logicalAndExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 50, "logicalAndExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( logicalAndExpression_AST );
		returnAST.setType( 10050 );
	}
	
	public final void inclusiveOrExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST inclusiveOrExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10051 );
		if( returnAST != null )
			return;
		try {      // for error handling
			exclusiveOrExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop128:
			do {
				if ((LA(1)==BOR)) {
					Token unlabelledAtom139 = LT(1);
					AST unlabelledAtom139_AST = null;
					match(BOR);
					if (inputState.guessing==0) {
						unlabelledAtom139_AST = (AST)astFactory.create(unlabelledAtom139);
						astFactory.addASTChild(currentAST, unlabelledAtom139_AST);
					}
					exclusiveOrExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop128;
				}
				
			} while (true);
			}
			inclusiveOrExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_50.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				inclusiveOrExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 51, "inclusiveOrExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( inclusiveOrExpression_AST );
		returnAST.setType( 10051 );
	}
	
	public final void exclusiveOrExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST exclusiveOrExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10052 );
		if( returnAST != null )
			return;
		try {      // for error handling
			andExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop131:
			do {
				if ((LA(1)==BXOR)) {
					Token unlabelledAtom140 = LT(1);
					AST unlabelledAtom140_AST = null;
					match(BXOR);
					if (inputState.guessing==0) {
						unlabelledAtom140_AST = (AST)astFactory.create(unlabelledAtom140);
						astFactory.addASTChild(currentAST, unlabelledAtom140_AST);
					}
					andExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop131;
				}
				
			} while (true);
			}
			exclusiveOrExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_51.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				exclusiveOrExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 52, "exclusiveOrExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( exclusiveOrExpression_AST );
		returnAST.setType( 10052 );
	}
	
	public final void andExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST andExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10053 );
		if( returnAST != null )
			return;
		try {      // for error handling
			equalityExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop134:
			do {
				if ((LA(1)==BAND)) {
					Token unlabelledAtom141 = LT(1);
					AST unlabelledAtom141_AST = null;
					match(BAND);
					if (inputState.guessing==0) {
						unlabelledAtom141_AST = (AST)astFactory.create(unlabelledAtom141);
						astFactory.addASTChild(currentAST, unlabelledAtom141_AST);
					}
					equalityExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop134;
				}
				
			} while (true);
			}
			andExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_52.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				andExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 53, "andExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( andExpression_AST );
		returnAST.setType( 10053 );
	}
	
	public final void equalityExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST equalityExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10054 );
		if( returnAST != null )
			return;
		try {      // for error handling
			relationalExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop138:
			do {
				if ((LA(1)==NOT_EQUAL||LA(1)==EQUAL)) {
					{
					switch ( LA(1)) {
					case NOT_EQUAL:
					{
						Token unlabelledAtom142 = LT(1);
						AST unlabelledAtom142_AST = null;
						match(NOT_EQUAL);
						if (inputState.guessing==0) {
							unlabelledAtom142_AST = (AST)astFactory.create(unlabelledAtom142);
							astFactory.addASTChild(currentAST, unlabelledAtom142_AST);
						}
						break;
					}
					case EQUAL:
					{
						Token unlabelledAtom143 = LT(1);
						AST unlabelledAtom143_AST = null;
						match(EQUAL);
						if (inputState.guessing==0) {
							unlabelledAtom143_AST = (AST)astFactory.create(unlabelledAtom143);
							astFactory.addASTChild(currentAST, unlabelledAtom143_AST);
						}
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					relationalExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop138;
				}
				
			} while (true);
			}
			equalityExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_53.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				equalityExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 54, "equalityExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( equalityExpression_AST );
		returnAST.setType( 10054 );
	}
	
	public final void relationalExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST relationalExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10055 );
		if( returnAST != null )
			return;
		try {      // for error handling
			shiftExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			switch ( LA(1)) {
			case SEMI:
			case RCURLY:
			case COMMA:
			case RPAREN:
			case RBRACK:
			case ASSIGN:
			case COLON:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
			case QUESTION:
			case LOR:
			case LAND:
			case BOR:
			case BXOR:
			case BAND:
			case NOT_EQUAL:
			case EQUAL:
			case LT:
			case GT:
			case LE:
			case GE:
			{
				{
				_loop143:
				do {
					if (((LA(1) >= LT && LA(1) <= GE))) {
						{
						switch ( LA(1)) {
						case LT:
						{
							Token unlabelledAtom144 = LT(1);
							AST unlabelledAtom144_AST = null;
							match(LT);
							if (inputState.guessing==0) {
								unlabelledAtom144_AST = (AST)astFactory.create(unlabelledAtom144);
								astFactory.addASTChild(currentAST, unlabelledAtom144_AST);
							}
							break;
						}
						case GT:
						{
							Token unlabelledAtom145 = LT(1);
							AST unlabelledAtom145_AST = null;
							match(GT);
							if (inputState.guessing==0) {
								unlabelledAtom145_AST = (AST)astFactory.create(unlabelledAtom145);
								astFactory.addASTChild(currentAST, unlabelledAtom145_AST);
							}
							break;
						}
						case LE:
						{
							Token unlabelledAtom146 = LT(1);
							AST unlabelledAtom146_AST = null;
							match(LE);
							if (inputState.guessing==0) {
								unlabelledAtom146_AST = (AST)astFactory.create(unlabelledAtom146);
								astFactory.addASTChild(currentAST, unlabelledAtom146_AST);
							}
							break;
						}
						case GE:
						{
							Token unlabelledAtom147 = LT(1);
							AST unlabelledAtom147_AST = null;
							match(GE);
							if (inputState.guessing==0) {
								unlabelledAtom147_AST = (AST)astFactory.create(unlabelledAtom147);
								astFactory.addASTChild(currentAST, unlabelledAtom147_AST);
							}
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						}
						}
						shiftExpression();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop143;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_instanceof:
			{
				AST tmp148_AST = null;
				if (inputState.guessing==0) {
					tmp148_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp148_AST);
				}
				match(LITERAL_instanceof);
				typeSpec();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			relationalExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_54.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				relationalExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 55, "relationalExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( relationalExpression_AST );
		returnAST.setType( 10055 );
	}
	
	public final void shiftExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST shiftExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10056 );
		if( returnAST != null )
			return;
		try {      // for error handling
			additiveExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop147:
			do {
				if (((LA(1) >= SL && LA(1) <= BSR))) {
					{
					switch ( LA(1)) {
					case SL:
					{
						Token unlabelledAtom149 = LT(1);
						AST unlabelledAtom149_AST = null;
						match(SL);
						if (inputState.guessing==0) {
							unlabelledAtom149_AST = (AST)astFactory.create(unlabelledAtom149);
							astFactory.addASTChild(currentAST, unlabelledAtom149_AST);
						}
						break;
					}
					case SR:
					{
						Token unlabelledAtom150 = LT(1);
						AST unlabelledAtom150_AST = null;
						match(SR);
						if (inputState.guessing==0) {
							unlabelledAtom150_AST = (AST)astFactory.create(unlabelledAtom150);
							astFactory.addASTChild(currentAST, unlabelledAtom150_AST);
						}
						break;
					}
					case BSR:
					{
						Token unlabelledAtom151 = LT(1);
						AST unlabelledAtom151_AST = null;
						match(BSR);
						if (inputState.guessing==0) {
							unlabelledAtom151_AST = (AST)astFactory.create(unlabelledAtom151);
							astFactory.addASTChild(currentAST, unlabelledAtom151_AST);
						}
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					additiveExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop147;
				}
				
			} while (true);
			}
			shiftExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_55.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				shiftExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 56, "shiftExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( shiftExpression_AST );
		returnAST.setType( 10056 );
	}
	
	public final void additiveExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST additiveExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10057 );
		if( returnAST != null )
			return;
		try {      // for error handling
			multiplicativeExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop151:
			do {
				if ((LA(1)==PLUS||LA(1)==MINUS)) {
					{
					switch ( LA(1)) {
					case PLUS:
					{
						Token unlabelledAtom152 = LT(1);
						AST unlabelledAtom152_AST = null;
						match(PLUS);
						if (inputState.guessing==0) {
							unlabelledAtom152_AST = (AST)astFactory.create(unlabelledAtom152);
							astFactory.addASTChild(currentAST, unlabelledAtom152_AST);
						}
						break;
					}
					case MINUS:
					{
						Token unlabelledAtom153 = LT(1);
						AST unlabelledAtom153_AST = null;
						match(MINUS);
						if (inputState.guessing==0) {
							unlabelledAtom153_AST = (AST)astFactory.create(unlabelledAtom153);
							astFactory.addASTChild(currentAST, unlabelledAtom153_AST);
						}
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					multiplicativeExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop151;
				}
				
			} while (true);
			}
			additiveExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_56.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				additiveExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 57, "additiveExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( additiveExpression_AST );
		returnAST.setType( 10057 );
	}
	
	public final void multiplicativeExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST multiplicativeExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10058 );
		if( returnAST != null )
			return;
		try {      // for error handling
			unaryExpression();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop155:
			do {
				if ((_tokenSet_57.member(LA(1)))) {
					{
					switch ( LA(1)) {
					case STAR:
					{
						Token unlabelledAtom154 = LT(1);
						AST unlabelledAtom154_AST = null;
						match(STAR);
						if (inputState.guessing==0) {
							unlabelledAtom154_AST = (AST)astFactory.create(unlabelledAtom154);
							astFactory.addASTChild(currentAST, unlabelledAtom154_AST);
						}
						break;
					}
					case DIV:
					{
						Token unlabelledAtom155 = LT(1);
						AST unlabelledAtom155_AST = null;
						match(DIV);
						if (inputState.guessing==0) {
							unlabelledAtom155_AST = (AST)astFactory.create(unlabelledAtom155);
							astFactory.addASTChild(currentAST, unlabelledAtom155_AST);
						}
						break;
					}
					case MOD:
					{
						Token unlabelledAtom156 = LT(1);
						AST unlabelledAtom156_AST = null;
						match(MOD);
						if (inputState.guessing==0) {
							unlabelledAtom156_AST = (AST)astFactory.create(unlabelledAtom156);
							astFactory.addASTChild(currentAST, unlabelledAtom156_AST);
						}
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					unaryExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop155;
				}
				
			} while (true);
			}
			multiplicativeExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_58.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				multiplicativeExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 58, "multiplicativeExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( multiplicativeExpression_AST );
		returnAST.setType( 10058 );
	}
	
	public final void unaryExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST unaryExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10059 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case INC:
			{
				Token unlabelledAtom157 = LT(1);
				AST unlabelledAtom157_AST = null;
				match(INC);
				if (inputState.guessing==0) {
					unlabelledAtom157_AST = (AST)astFactory.create(unlabelledAtom157);
					astFactory.addASTChild(currentAST, unlabelledAtom157_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case DEC:
			{
				Token unlabelledAtom158 = LT(1);
				AST unlabelledAtom158_AST = null;
				match(DEC);
				if (inputState.guessing==0) {
					unlabelledAtom158_AST = (AST)astFactory.create(unlabelledAtom158);
					astFactory.addASTChild(currentAST, unlabelledAtom158_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case MINUS:
			{
				Token unlabelledAtom159 = LT(1);
				AST unlabelledAtom159_AST = null;
				match(MINUS);
				if (inputState.guessing==0) {
					unlabelledAtom159_AST = (AST)astFactory.create(unlabelledAtom159);
					astFactory.addASTChild(currentAST, unlabelledAtom159_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case PLUS:
			{
				Token unlabelledAtom160 = LT(1);
				AST unlabelledAtom160_AST = null;
				match(PLUS);
				if (inputState.guessing==0) {
					unlabelledAtom160_AST = (AST)astFactory.create(unlabelledAtom160);
					astFactory.addASTChild(currentAST, unlabelledAtom160_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				unaryExpressionNotPlusMinus();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpression_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_59.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				unaryExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 59, "unaryExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( unaryExpression_AST );
		returnAST.setType( 10059 );
	}
	
	public final void unaryExpressionNotPlusMinus() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST unaryExpressionNotPlusMinus_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10060 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case BNOT:
			{
				Token unlabelledAtom161 = LT(1);
				AST unlabelledAtom161_AST = null;
				match(BNOT);
				if (inputState.guessing==0) {
					unlabelledAtom161_AST = (AST)astFactory.create(unlabelledAtom161);
					astFactory.addASTChild(currentAST, unlabelledAtom161_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
				break;
			}
			case LNOT:
			{
				Token unlabelledAtom162 = LT(1);
				AST unlabelledAtom162_AST = null;
				match(LNOT);
				if (inputState.guessing==0) {
					unlabelledAtom162_AST = (AST)astFactory.create(unlabelledAtom162);
					astFactory.addASTChild(currentAST, unlabelledAtom162_AST);
				}
				unaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				{
				if ((LA(1)==LPAREN) && ((LA(2) >= LITERAL_void && LA(2) <= LITERAL_double))) {
					Token unlabelledAtom163 = LT(1);
					AST unlabelledAtom163_AST = null;
					match(LPAREN);
					if (inputState.guessing==0) {
						unlabelledAtom163_AST = (AST)astFactory.create(unlabelledAtom163);
						astFactory.addASTChild(currentAST, unlabelledAtom163_AST);
					}
					builtInTypeSpec();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					Token unlabelledAtom164 = LT(1);
					AST unlabelledAtom164_AST = null;
					match(RPAREN);
					if (inputState.guessing==0) {
						unlabelledAtom164_AST = (AST)astFactory.create(unlabelledAtom164);
						astFactory.addASTChild(currentAST, unlabelledAtom164_AST);
					}
					unaryExpression();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					boolean synPredMatched160 = false;
					if (((LA(1)==LPAREN) && (LA(2)==IDENT))) {
						int _m160 = mark();
						synPredMatched160 = true;
						inputState.guessing++;
						try {
							{
							match(LPAREN);
							classTypeSpec();
							match(RPAREN);
							unaryExpressionNotPlusMinus();
							}
						}
						catch (RecognitionException pe) {
							synPredMatched160 = false;
						}
						rewind(_m160);
						inputState.guessing--;
					}
					if ( synPredMatched160 ) {
						Token unlabelledAtom165 = LT(1);
						AST unlabelledAtom165_AST = null;
						match(LPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom165_AST = (AST)astFactory.create(unlabelledAtom165);
							astFactory.addASTChild(currentAST, unlabelledAtom165_AST);
						}
						classTypeSpec();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						Token unlabelledAtom166 = LT(1);
						AST unlabelledAtom166_AST = null;
						match(RPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom166_AST = (AST)astFactory.create(unlabelledAtom166);
							astFactory.addASTChild(currentAST, unlabelledAtom166_AST);
						}
						unaryExpressionNotPlusMinus();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else if ((_tokenSet_60.member(LA(1))) && (_tokenSet_61.member(LA(2)))) {
						postfixExpression();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
			}
			catch (RecognitionException ex) {
				if (inputState.guessing==0) {
					reportError(ex);
					// BSE non-discarding error handler
					parseFailed = true;
					ASTPair err_AST = new ASTPair();
					do
					{
						if( LA( 1 ) == Token.EOF_TYPE ) break;
						AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
						astFactory.addASTChild( err_AST, badToken_AST );
						consume();
					} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_59.member( LA( 1 ) )) );
					if( err_AST.child != null )
					{
						AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
						astFactory.makeASTRoot( err_AST, err_AST_root );
						astFactory.addASTChild( currentAST, err_AST.root );
					}
					unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
				} else {
				  throw ex;
				}
			}
			returnAST = astFactory.create( 60, "unaryExpressionNotPlusMinus" +
				( parseFailed ? "<bad>" : "" ) );
			returnAST.addChild( unaryExpressionNotPlusMinus_AST );
			returnAST.setType( 10060 );
		}
		
	public final void postfixExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST postfixExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10061 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case IDENT:
			case LPAREN:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				primaryExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop167:
				do {
					switch ( LA(1)) {
					case DOT:
					{
						Token unlabelledAtom167 = LT(1);
						AST unlabelledAtom167_AST = null;
						match(DOT);
						if (inputState.guessing==0) {
							unlabelledAtom167_AST = (AST)astFactory.create(unlabelledAtom167);
							astFactory.addASTChild(currentAST, unlabelledAtom167_AST);
						}
						{
						switch ( LA(1)) {
						case IDENT:
						{
							Token unlabelledAtom168 = LT(1);
							AST unlabelledAtom168_AST = null;
							match(IDENT);
							if (inputState.guessing==0) {
								unlabelledAtom168_AST = (AST)astFactory.create(unlabelledAtom168);
								astFactory.addASTChild(currentAST, unlabelledAtom168_AST);
							}
							break;
						}
						case LITERAL_this:
						{
							AST tmp169_AST = null;
							if (inputState.guessing==0) {
								tmp169_AST = (AST)astFactory.create(LT(1));
								astFactory.addASTChild(currentAST, tmp169_AST);
							}
							match(LITERAL_this);
							break;
						}
						case LITERAL_class:
						{
							AST tmp170_AST = null;
							if (inputState.guessing==0) {
								tmp170_AST = (AST)astFactory.create(LT(1));
								astFactory.addASTChild(currentAST, tmp170_AST);
							}
							match(LITERAL_class);
							break;
						}
						case LITERAL_new:
						{
							newExpression();
							if (inputState.guessing==0) {
								astFactory.addASTChild(currentAST, returnAST);
							}
							break;
						}
						case LITERAL_super:
						{
							AST tmp171_AST = null;
							if (inputState.guessing==0) {
								tmp171_AST = (AST)astFactory.create(LT(1));
								astFactory.addASTChild(currentAST, tmp171_AST);
							}
							match(LITERAL_super);
							Token unlabelledAtom172 = LT(1);
							AST unlabelledAtom172_AST = null;
							match(LPAREN);
							if (inputState.guessing==0) {
								unlabelledAtom172_AST = (AST)astFactory.create(unlabelledAtom172);
								astFactory.addASTChild(currentAST, unlabelledAtom172_AST);
							}
							{
							switch ( LA(1)) {
							case LITERAL_void:
							case LITERAL_boolean:
							case LITERAL_byte:
							case LITERAL_char:
							case LITERAL_short:
							case LITERAL_int:
							case LITERAL_float:
							case LITERAL_long:
							case LITERAL_double:
							case IDENT:
							case LPAREN:
							case PLUS:
							case MINUS:
							case INC:
							case DEC:
							case BNOT:
							case LNOT:
							case LITERAL_this:
							case LITERAL_super:
							case LITERAL_true:
							case LITERAL_false:
							case LITERAL_null:
							case LITERAL_new:
							case NUM_INT:
							case CHAR_LITERAL:
							case STRING_LITERAL:
							case NUM_FLOAT:
							{
								expressionList();
								if (inputState.guessing==0) {
									astFactory.addASTChild(currentAST, returnAST);
								}
								break;
							}
							case RPAREN:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							}
							}
							Token unlabelledAtom173 = LT(1);
							AST unlabelledAtom173_AST = null;
							match(RPAREN);
							if (inputState.guessing==0) {
								unlabelledAtom173_AST = (AST)astFactory.create(unlabelledAtom173);
								astFactory.addASTChild(currentAST, unlabelledAtom173_AST);
							}
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						}
						}
						break;
					}
					case LPAREN:
					{
						Token unlabelledAtom174 = LT(1);
						AST unlabelledAtom174_AST = null;
						match(LPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom174_AST = (AST)astFactory.create(unlabelledAtom174);
							astFactory.addASTChild(currentAST, unlabelledAtom174_AST);
						}
						argList();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						Token unlabelledAtom175 = LT(1);
						AST unlabelledAtom175_AST = null;
						match(RPAREN);
						if (inputState.guessing==0) {
							unlabelledAtom175_AST = (AST)astFactory.create(unlabelledAtom175);
							astFactory.addASTChild(currentAST, unlabelledAtom175_AST);
						}
						break;
					}
					default:
						if ((LA(1)==LBRACK) && (LA(2)==RBRACK)) {
							{
							int _cnt166=0;
							_loop166:
							do {
								if ((LA(1)==LBRACK)) {
									Token unlabelledAtom176 = LT(1);
									AST unlabelledAtom176_AST = null;
									match(LBRACK);
									if (inputState.guessing==0) {
										unlabelledAtom176_AST = (AST)astFactory.create(unlabelledAtom176);
										astFactory.addASTChild(currentAST, unlabelledAtom176_AST);
									}
									Token unlabelledAtom177 = LT(1);
									AST unlabelledAtom177_AST = null;
									match(RBRACK);
									if (inputState.guessing==0) {
										unlabelledAtom177_AST = (AST)astFactory.create(unlabelledAtom177);
										astFactory.addASTChild(currentAST, unlabelledAtom177_AST);
									}
								}
								else {
									if ( _cnt166>=1 ) { break _loop166; } else {throw new NoViableAltException(LT(1), getFilename());}
								}
								
								_cnt166++;
							} while (true);
							}
							Token unlabelledAtom178 = LT(1);
							AST unlabelledAtom178_AST = null;
							match(DOT);
							if (inputState.guessing==0) {
								unlabelledAtom178_AST = (AST)astFactory.create(unlabelledAtom178);
								astFactory.addASTChild(currentAST, unlabelledAtom178_AST);
							}
							AST tmp179_AST = null;
							if (inputState.guessing==0) {
								tmp179_AST = (AST)astFactory.create(LT(1));
								astFactory.addASTChild(currentAST, tmp179_AST);
							}
							match(LITERAL_class);
						}
						else if ((LA(1)==LBRACK) && (_tokenSet_39.member(LA(2)))) {
							Token unlabelledAtom180 = LT(1);
							AST unlabelledAtom180_AST = null;
							match(LBRACK);
							if (inputState.guessing==0) {
								unlabelledAtom180_AST = (AST)astFactory.create(unlabelledAtom180);
								astFactory.addASTChild(currentAST, unlabelledAtom180_AST);
							}
							expression();
							if (inputState.guessing==0) {
								astFactory.addASTChild(currentAST, returnAST);
							}
							Token unlabelledAtom181 = LT(1);
							AST unlabelledAtom181_AST = null;
							match(RBRACK);
							if (inputState.guessing==0) {
								unlabelledAtom181_AST = (AST)astFactory.create(unlabelledAtom181);
								astFactory.addASTChild(currentAST, unlabelledAtom181_AST);
							}
						}
					else {
						break _loop167;
					}
					}
				} while (true);
				}
				{
				switch ( LA(1)) {
				case INC:
				{
					Token unlabelledAtom182 = LT(1);
					AST unlabelledAtom182_AST = null;
					match(INC);
					if (inputState.guessing==0) {
						unlabelledAtom182_AST = (AST)astFactory.create(unlabelledAtom182);
						astFactory.addASTChild(currentAST, unlabelledAtom182_AST);
					}
					break;
				}
				case DEC:
				{
					Token unlabelledAtom183 = LT(1);
					AST unlabelledAtom183_AST = null;
					match(DEC);
					if (inputState.guessing==0) {
						unlabelledAtom183_AST = (AST)astFactory.create(unlabelledAtom183);
						astFactory.addASTChild(currentAST, unlabelledAtom183_AST);
					}
					break;
				}
				case SEMI:
				case STAR:
				case RCURLY:
				case COMMA:
				case RPAREN:
				case RBRACK:
				case ASSIGN:
				case COLON:
				case PLUS_ASSIGN:
				case MINUS_ASSIGN:
				case STAR_ASSIGN:
				case DIV_ASSIGN:
				case MOD_ASSIGN:
				case SR_ASSIGN:
				case BSR_ASSIGN:
				case SL_ASSIGN:
				case BAND_ASSIGN:
				case BXOR_ASSIGN:
				case BOR_ASSIGN:
				case QUESTION:
				case LOR:
				case LAND:
				case BOR:
				case BXOR:
				case BAND:
				case NOT_EQUAL:
				case EQUAL:
				case LT:
				case GT:
				case LE:
				case GE:
				case LITERAL_instanceof:
				case SL:
				case SR:
				case BSR:
				case PLUS:
				case MINUS:
				case DIV:
				case MOD:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				postfixExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			{
				builtInType();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				declaratorBrackets();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom184 = LT(1);
				AST unlabelledAtom184_AST = null;
				match(DOT);
				if (inputState.guessing==0) {
					unlabelledAtom184_AST = (AST)astFactory.create(unlabelledAtom184);
					astFactory.addASTChild(currentAST, unlabelledAtom184_AST);
				}
				AST tmp185_AST = null;
				if (inputState.guessing==0) {
					tmp185_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp185_AST);
				}
				match(LITERAL_class);
				postfixExpression_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_59.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				postfixExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 61, "postfixExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( postfixExpression_AST );
		returnAST.setType( 10061 );
	}
	
	public final void primaryExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST primaryExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10062 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case IDENT:
			{
				Token unlabelledAtom186 = LT(1);
				AST unlabelledAtom186_AST = null;
				match(IDENT);
				if (inputState.guessing==0) {
					unlabelledAtom186_AST = (AST)astFactory.create(unlabelledAtom186);
					astFactory.addASTChild(currentAST, unlabelledAtom186_AST);
				}
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_new:
			{
				newExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				constant();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_super:
			{
				AST tmp187_AST = null;
				if (inputState.guessing==0) {
					tmp187_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp187_AST);
				}
				match(LITERAL_super);
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_true:
			{
				AST tmp188_AST = null;
				if (inputState.guessing==0) {
					tmp188_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp188_AST);
				}
				match(LITERAL_true);
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_false:
			{
				AST tmp189_AST = null;
				if (inputState.guessing==0) {
					tmp189_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp189_AST);
				}
				match(LITERAL_false);
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_this:
			{
				AST tmp190_AST = null;
				if (inputState.guessing==0) {
					tmp190_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp190_AST);
				}
				match(LITERAL_this);
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LITERAL_null:
			{
				AST tmp191_AST = null;
				if (inputState.guessing==0) {
					tmp191_AST = (AST)astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp191_AST);
				}
				match(LITERAL_null);
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			case LPAREN:
			{
				Token unlabelledAtom192 = LT(1);
				AST unlabelledAtom192_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom192_AST = (AST)astFactory.create(unlabelledAtom192);
					astFactory.addASTChild(currentAST, unlabelledAtom192_AST);
				}
				assignmentExpression();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom193 = LT(1);
				AST unlabelledAtom193_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom193_AST = (AST)astFactory.create(unlabelledAtom193);
					astFactory.addASTChild(currentAST, unlabelledAtom193_AST);
				}
				primaryExpression_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_31.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				primaryExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 62, "primaryExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( primaryExpression_AST );
		returnAST.setType( 10062 );
	}
	
/** object instantiation.
 *  Trees are built as illustrated by the following input/tree pairs:
 *
 *  new T()
 *
 *  new
 *   |
 *   T --  ELIST
 *           |
 *          arg1 -- arg2 -- .. -- argn
 *
 *  new int[]
 *
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *
 *  new int[] {1,2}
 *
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR -- ARRAY_INIT
 *                                  |
 *                                EXPR -- EXPR
 *                                  |      |
 *                                  1      2
 *
 *  new int[3]
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *                |
 *              EXPR
 *                |
 *                3
 *
 *  new int[1][2]
 *
 *  new
 *   |
 *  int -- ARRAY_DECLARATOR
 *               |
 *         ARRAY_DECLARATOR -- EXPR
 *               |              |
 *             EXPR             1
 *               |
 *               2
 *
 */
	public final void newExpression() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST newExpression_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10063 );
		if( returnAST != null )
			return;
		try {      // for error handling
			AST tmp194_AST = null;
			if (inputState.guessing==0) {
				tmp194_AST = (AST)astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp194_AST);
			}
			match(LITERAL_new);
			type();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			switch ( LA(1)) {
			case LPAREN:
			{
				Token unlabelledAtom195 = LT(1);
				AST unlabelledAtom195_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom195_AST = (AST)astFactory.create(unlabelledAtom195);
					astFactory.addASTChild(currentAST, unlabelledAtom195_AST);
				}
				argList();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom196 = LT(1);
				AST unlabelledAtom196_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom196_AST = (AST)astFactory.create(unlabelledAtom196);
					astFactory.addASTChild(currentAST, unlabelledAtom196_AST);
				}
				{
				switch ( LA(1)) {
				case LCURLY:
				{
					classBlock();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case SEMI:
				case DOT:
				case STAR:
				case RCURLY:
				case COMMA:
				case LPAREN:
				case RPAREN:
				case LBRACK:
				case RBRACK:
				case ASSIGN:
				case COLON:
				case PLUS_ASSIGN:
				case MINUS_ASSIGN:
				case STAR_ASSIGN:
				case DIV_ASSIGN:
				case MOD_ASSIGN:
				case SR_ASSIGN:
				case BSR_ASSIGN:
				case SL_ASSIGN:
				case BAND_ASSIGN:
				case BXOR_ASSIGN:
				case BOR_ASSIGN:
				case QUESTION:
				case LOR:
				case LAND:
				case BOR:
				case BXOR:
				case BAND:
				case NOT_EQUAL:
				case EQUAL:
				case LT:
				case GT:
				case LE:
				case GE:
				case LITERAL_instanceof:
				case SL:
				case SR:
				case BSR:
				case PLUS:
				case MINUS:
				case DIV:
				case MOD:
				case INC:
				case DEC:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				break;
			}
			case LBRACK:
			{
				newArrayDeclarator();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				switch ( LA(1)) {
				case LCURLY:
				{
					arrayInitializer();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					break;
				}
				case SEMI:
				case DOT:
				case STAR:
				case RCURLY:
				case COMMA:
				case LPAREN:
				case RPAREN:
				case LBRACK:
				case RBRACK:
				case ASSIGN:
				case COLON:
				case PLUS_ASSIGN:
				case MINUS_ASSIGN:
				case STAR_ASSIGN:
				case DIV_ASSIGN:
				case MOD_ASSIGN:
				case SR_ASSIGN:
				case BSR_ASSIGN:
				case SL_ASSIGN:
				case BAND_ASSIGN:
				case BXOR_ASSIGN:
				case BOR_ASSIGN:
				case QUESTION:
				case LOR:
				case LAND:
				case BOR:
				case BXOR:
				case BAND:
				case NOT_EQUAL:
				case EQUAL:
				case LT:
				case GT:
				case LE:
				case GE:
				case LITERAL_instanceof:
				case SL:
				case SR:
				case BSR:
				case PLUS:
				case MINUS:
				case DIV:
				case MOD:
				case INC:
				case DEC:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			newExpression_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_31.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				newExpression_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 63, "newExpression" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( newExpression_AST );
		returnAST.setType( 10063 );
	}
	
	public final void argList() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST argList_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10064 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case IDENT:
			case LPAREN:
			case PLUS:
			case MINUS:
			case INC:
			case DEC:
			case BNOT:
			case LNOT:
			case LITERAL_this:
			case LITERAL_super:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case LITERAL_new:
			case NUM_INT:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			{
				expressionList();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case RPAREN:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			argList_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_25.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				argList_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 64, "argList" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( argList_AST );
		returnAST.setType( 10064 );
	}
	
	public final void constant() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST constant_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10065 );
		if( returnAST != null )
			return;
		try {      // for error handling
			switch ( LA(1)) {
			case NUM_INT:
			{
				Token unlabelledAtom197 = LT(1);
				AST unlabelledAtom197_AST = null;
				match(NUM_INT);
				if (inputState.guessing==0) {
					unlabelledAtom197_AST = (AST)astFactory.create(unlabelledAtom197);
					astFactory.addASTChild(currentAST, unlabelledAtom197_AST);
				}
				constant_AST = (AST)currentAST.root;
				break;
			}
			case CHAR_LITERAL:
			{
				Token unlabelledAtom198 = LT(1);
				AST unlabelledAtom198_AST = null;
				match(CHAR_LITERAL);
				if (inputState.guessing==0) {
					unlabelledAtom198_AST = (AST)astFactory.create(unlabelledAtom198);
					astFactory.addASTChild(currentAST, unlabelledAtom198_AST);
				}
				constant_AST = (AST)currentAST.root;
				break;
			}
			case STRING_LITERAL:
			{
				Token unlabelledAtom199 = LT(1);
				AST unlabelledAtom199_AST = null;
				match(STRING_LITERAL);
				if (inputState.guessing==0) {
					unlabelledAtom199_AST = (AST)astFactory.create(unlabelledAtom199);
					astFactory.addASTChild(currentAST, unlabelledAtom199_AST);
				}
				constant_AST = (AST)currentAST.root;
				break;
			}
			case NUM_FLOAT:
			{
				Token unlabelledAtom200 = LT(1);
				AST unlabelledAtom200_AST = null;
				match(NUM_FLOAT);
				if (inputState.guessing==0) {
					unlabelledAtom200_AST = (AST)astFactory.create(unlabelledAtom200);
					astFactory.addASTChild(currentAST, unlabelledAtom200_AST);
				}
				constant_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_31.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				constant_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 65, "constant" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( constant_AST );
		returnAST.setType( 10065 );
	}
	
	public final void newArrayDeclarator() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST newArrayDeclarator_AST = null;
		
		boolean parseFailed = false;
		returnAST = extractExistingASTFromInput( 10066 );
		if( returnAST != null )
			return;
		try {      // for error handling
			{
			int _cnt179=0;
			_loop179:
			do {
				if ((LA(1)==LBRACK) && (_tokenSet_62.member(LA(2)))) {
					Token unlabelledAtom201 = LT(1);
					AST unlabelledAtom201_AST = null;
					match(LBRACK);
					if (inputState.guessing==0) {
						unlabelledAtom201_AST = (AST)astFactory.create(unlabelledAtom201);
						astFactory.addASTChild(currentAST, unlabelledAtom201_AST);
					}
					{
					switch ( LA(1)) {
					case LITERAL_void:
					case LITERAL_boolean:
					case LITERAL_byte:
					case LITERAL_char:
					case LITERAL_short:
					case LITERAL_int:
					case LITERAL_float:
					case LITERAL_long:
					case LITERAL_double:
					case IDENT:
					case LPAREN:
					case PLUS:
					case MINUS:
					case INC:
					case DEC:
					case BNOT:
					case LNOT:
					case LITERAL_this:
					case LITERAL_super:
					case LITERAL_true:
					case LITERAL_false:
					case LITERAL_null:
					case LITERAL_new:
					case NUM_INT:
					case CHAR_LITERAL:
					case STRING_LITERAL:
					case NUM_FLOAT:
					{
						expression();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
						break;
					}
					case RBRACK:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					Token unlabelledAtom202 = LT(1);
					AST unlabelledAtom202_AST = null;
					match(RBRACK);
					if (inputState.guessing==0) {
						unlabelledAtom202_AST = (AST)astFactory.create(unlabelledAtom202);
					}
				}
				else {
					if ( _cnt179>=1 ) { break _loop179; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt179++;
			} while (true);
			}
			newArrayDeclarator_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				// BSE non-discarding error handler
				parseFailed = true;
				ASTPair err_AST = new ASTPair();
				do
				{
					if( LA( 1 ) == Token.EOF_TYPE ) break;
					AST badToken_AST = (AST)astFactory.create( LT( 1 ) );
					astFactory.addASTChild( err_AST, badToken_AST );
					consume();
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_63.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				newArrayDeclarator_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 66, "newArrayDeclarator" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( newArrayDeclarator_AST );
		returnAST.setType( 10066 );
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"BLOCK",
		"MODIFIERS",
		"OBJBLOCK",
		"SLIST",
		"CTOR_DEF",
		"METHOD_DEF",
		"VARIABLE_DEF",
		"INSTANCE_INIT",
		"STATIC_INIT",
		"TYPE",
		"CLASS_DEF",
		"INTERFACE_DEF",
		"\"package\"",
		"ARRAY_DECLARATOR",
		"EXTENDS_CLAUSE",
		"IMPLEMENTS_CLAUSE",
		"PARAMETERS",
		"PARAMETER_DEF",
		"LABELED_STAT",
		"TYPECAST",
		"INDEX_OP",
		"POST_INC",
		"POST_DEC",
		"METHOD_CALL",
		"EXPR",
		"ARRAY_INIT",
		"\"import\"",
		"UNARY_MINUS",
		"UNARY_PLUS",
		"CASE_GROUP",
		"ELIST",
		"FOR_INIT",
		"FOR_CONDITION",
		"FOR_ITERATOR",
		"EMPTY_STAT",
		"\"final\"",
		"\"abstract\"",
		"WS",
		"ML_COMMENT",
		"SL_COMMENT",
		"SEMI",
		"\"void\"",
		"\"boolean\"",
		"\"byte\"",
		"\"char\"",
		"\"short\"",
		"\"int\"",
		"\"float\"",
		"\"long\"",
		"\"double\"",
		"IDENT",
		"DOT",
		"STAR",
		"\"private\"",
		"\"public\"",
		"\"protected\"",
		"\"static\"",
		"\"transient\"",
		"\"native\"",
		"\"threadsafe\"",
		"\"synchronized\"",
		"\"volatile\"",
		"\"class\"",
		"\"extends\"",
		"\"interface\"",
		"LCURLY",
		"RCURLY",
		"COMMA",
		"\"implements\"",
		"LPAREN",
		"RPAREN",
		"LBRACK",
		"RBRACK",
		"ASSIGN",
		"\"throws\"",
		"COLON",
		"\"if\"",
		"\"else\"",
		"\"for\"",
		"\"while\"",
		"\"do\"",
		"\"break\"",
		"\"continue\"",
		"\"return\"",
		"\"switch\"",
		"\"throw\"",
		"\"case\"",
		"\"default\"",
		"\"try\"",
		"\"finally\"",
		"\"catch\"",
		"PLUS_ASSIGN",
		"MINUS_ASSIGN",
		"STAR_ASSIGN",
		"DIV_ASSIGN",
		"MOD_ASSIGN",
		"SR_ASSIGN",
		"BSR_ASSIGN",
		"SL_ASSIGN",
		"BAND_ASSIGN",
		"BXOR_ASSIGN",
		"BOR_ASSIGN",
		"QUESTION",
		"LOR",
		"LAND",
		"BOR",
		"BXOR",
		"BAND",
		"NOT_EQUAL",
		"EQUAL",
		"LT",
		"GT",
		"LE",
		"GE",
		"\"instanceof\"",
		"SL",
		"SR",
		"BSR",
		"PLUS",
		"MINUS",
		"DIV",
		"MOD",
		"INC",
		"DEC",
		"BNOT",
		"LNOT",
		"\"this\"",
		"\"super\"",
		"\"true\"",
		"\"false\"",
		"\"null\"",
		"\"new\"",
		"NUM_INT",
		"CHAR_LITERAL",
		"STRING_LITERAL",
		"NUM_FLOAT",
		"ESC",
		"HEX_DIGIT",
		"VOCAB",
		"EXPONENT",
		"FLOAT_SUFFIX"
	};
	
	private static final long _tokenSet_0_data_[] = { 15393162788864L, 0L, 0L };
	public static final BitSet _tokenSet_0 = new BitSet(_tokenSet_0_data_);
	private static final long _tokenSet_1_data_[] = { -144095946622369792L, 23L, 0L, 0L };
	public static final BitSet _tokenSet_1 = new BitSet(_tokenSet_1_data_);
	private static final long _tokenSet_2_data_[] = { 2L, 0L, 0L };
	public static final BitSet _tokenSet_2 = new BitSet(_tokenSet_2_data_);
	private static final long _tokenSet_3_data_[] = { -144095945548627966L, 23L, 0L, 0L };
	public static final BitSet _tokenSet_3 = new BitSet(_tokenSet_3_data_);
	private static final long _tokenSet_4_data_[] = { -144095946622369790L, 23L, 0L, 0L };
	public static final BitSet _tokenSet_4 = new BitSet(_tokenSet_4_data_);
	private static final long _tokenSet_5_data_[] = { 18031990695526400L, 1125897759408096L, 0L, 0L };
	public static final BitSet _tokenSet_5 = new BitSet(_tokenSet_5_data_);
	private static final long _tokenSet_6_data_[] = { 17592186044416L, 0L, 0L };
	public static final BitSet _tokenSet_6 = new BitSet(_tokenSet_6_data_);
	private static final long _tokenSet_7_data_[] = { -144113538808414208L, 3L, 0L, 0L };
	public static final BitSet _tokenSet_7 = new BitSet(_tokenSet_7_data_);
	private static final long _tokenSet_8_data_[] = { 35993612646875136L, 20L, 0L, 0L };
	public static final BitSet _tokenSet_8 = new BitSet(_tokenSet_8_data_);
	private static final long _tokenSet_9_data_[] = { -108102333975494654L, -3746994889435446665L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_9 = new BitSet(_tokenSet_9_data_);
	private static final long _tokenSet_10_data_[] = { -108102333975494654L, 119L, 0L, 0L };
	public static final BitSet _tokenSet_10 = new BitSet(_tokenSet_10_data_);
	private static final long _tokenSet_11_data_[] = { 18031990695526400L, 1125897759405248L, 0L, 0L };
	public static final BitSet _tokenSet_11 = new BitSet(_tokenSet_11_data_);
	private static final long _tokenSet_12_data_[] = { -108119926161539072L, 23L, 0L, 0L };
	public static final BitSet _tokenSet_12 = new BitSet(_tokenSet_12_data_);
	private static final long _tokenSet_13_data_[] = { 54060787714490368L, 1125897759421664L, 0L, 0L };
	public static final BitSet _tokenSet_13 = new BitSet(_tokenSet_13_data_);
	private static final long _tokenSet_14_data_[] = { 54060787714490368L, 1125897759407808L, 0L, 0L };
	public static final BitSet _tokenSet_14 = new BitSet(_tokenSet_14_data_);
	private static final long _tokenSet_15_data_[] = { 0L, 2560L, 0L, 0L };
	public static final BitSet _tokenSet_15 = new BitSet(_tokenSet_15_data_);
	private static final long _tokenSet_16_data_[] = { 0L, 288L, 0L, 0L };
	public static final BitSet _tokenSet_16 = new BitSet(_tokenSet_16_data_);
	private static final long _tokenSet_17_data_[] = { 0L, 32L, 0L, 0L };
	public static final BitSet _tokenSet_17 = new BitSet(_tokenSet_17_data_);
	private static final long _tokenSet_18_data_[] = { -15942918602750L, -1610629385L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_18 = new BitSet(_tokenSet_18_data_);
	private static final long _tokenSet_19_data_[] = { -72091129142575104L, 2583L, 0L, 0L };
	public static final BitSet _tokenSet_19 = new BitSet(_tokenSet_19_data_);
	private static final long _tokenSet_20_data_[] = { 54043195528445952L, 2048L, 0L, 0L };
	public static final BitSet _tokenSet_20 = new BitSet(_tokenSet_20_data_);
	private static final long _tokenSet_21_data_[] = { 17592186044416L, 10368L, 0L, 0L };
	public static final BitSet _tokenSet_21 = new BitSet(_tokenSet_21_data_);
	private static final long _tokenSet_22_data_[] = { -108102333975494656L, 119L, 0L, 0L };
	public static final BitSet _tokenSet_22 = new BitSet(_tokenSet_22_data_);
	private static final long _tokenSet_23_data_[] = { -108102333975494656L, -3746994889636904409L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_23 = new BitSet(_tokenSet_23_data_);
	private static final long _tokenSet_24_data_[] = { -108102333975494656L, -3746994887824833929L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_24 = new BitSet(_tokenSet_24_data_);
	private static final long _tokenSet_25_data_[] = { 0L, 1024L, 0L, 0L };
	public static final BitSet _tokenSet_25 = new BitSet(_tokenSet_25_data_);
	private static final long _tokenSet_26_data_[] = { 17592186044416L, 16416L, 0L, 0L };
	public static final BitSet _tokenSet_26 = new BitSet(_tokenSet_26_data_);
	private static final long _tokenSet_27_data_[] = { 17592186044416L, 32L, 0L, 0L };
	public static final BitSet _tokenSet_27 = new BitSet(_tokenSet_27_data_);
	private static final long _tokenSet_28_data_[] = { 17592186044416L, 128L, 0L, 0L };
	public static final BitSet _tokenSet_28 = new BitSet(_tokenSet_28_data_);
	private static final long _tokenSet_29_data_[] = { 17592186044416L, 192L, 0L, 0L };
	public static final BitSet _tokenSet_29 = new BitSet(_tokenSet_29_data_);
	private static final long _tokenSet_30_data_[] = { 35993612646875136L, -3746994889972252128L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_30 = new BitSet(_tokenSet_30_data_);
	private static final long _tokenSet_31_data_[] = { 108103983242936320L, -2147434816L, 0L, 0L };
	public static final BitSet _tokenSet_31 = new BitSet(_tokenSet_31_data_);
	private static final long _tokenSet_32_data_[] = { 17592186044416L, 38080L, 0L, 0L };
	public static final BitSet _tokenSet_32 = new BitSet(_tokenSet_32_data_);
	private static final long _tokenSet_33_data_[] = { 0L, 1152L, 0L, 0L };
	public static final BitSet _tokenSet_33 = new BitSet(_tokenSet_33_data_);
	private static final long _tokenSet_34_data_[] = { 35993612646875136L, 0L, 0L };
	public static final BitSet _tokenSet_34 = new BitSet(_tokenSet_34_data_);
	private static final long _tokenSet_35_data_[] = { -108102333975494656L, -3746994889435446681L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_35 = new BitSet(_tokenSet_35_data_);
	private static final long _tokenSet_36_data_[] = { -15942918602752L, -21897L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_36 = new BitSet(_tokenSet_36_data_);
	private static final long _tokenSet_37_data_[] = { -108119926161539072L, 3L, 0L, 0L };
	public static final BitSet _tokenSet_37 = new BitSet(_tokenSet_37_data_);
	private static final long _tokenSet_38_data_[] = { -72091129142575104L, 2051L, 0L, 0L };
	public static final BitSet _tokenSet_38 = new BitSet(_tokenSet_38_data_);
	private static final long _tokenSet_39_data_[] = { 35993612646875136L, -3746994889972252160L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_39 = new BitSet(_tokenSet_39_data_);
	private static final long _tokenSet_40_data_[] = { 144097595889811456L, -2147472896L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_40 = new BitSet(_tokenSet_40_data_);
	private static final long _tokenSet_41_data_[] = { 144097595889811456L, -2147472768L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_41 = new BitSet(_tokenSet_41_data_);
	private static final long _tokenSet_42_data_[] = { 35993612646875136L, -3746994889972219392L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_42 = new BitSet(_tokenSet_42_data_);
	private static final long _tokenSet_43_data_[] = { 0L, 201326656L, 0L, 0L };
	public static final BitSet _tokenSet_43 = new BitSet(_tokenSet_43_data_);
	private static final long _tokenSet_44_data_[] = { -108102333975494656L, -3746994889435577753L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_44 = new BitSet(_tokenSet_44_data_);
	private static final long _tokenSet_45_data_[] = { 17592186044416L, 1024L, 0L, 0L };
	public static final BitSet _tokenSet_45 = new BitSet(_tokenSet_45_data_);
	private static final long _tokenSet_46_data_[] = { -108102333975494656L, -3746994887824833945L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_46 = new BitSet(_tokenSet_46_data_);
	private static final long _tokenSet_47_data_[] = { 17592186044416L, 4395899073728L, 0L, 0L };
	public static final BitSet _tokenSet_47 = new BitSet(_tokenSet_47_data_);
	private static final long _tokenSet_48_data_[] = { 17592186044416L, 8793945584832L, 0L, 0L };
	public static final BitSet _tokenSet_48 = new BitSet(_tokenSet_48_data_);
	private static final long _tokenSet_49_data_[] = { 17592186044416L, 17590038607040L, 0L, 0L };
	public static final BitSet _tokenSet_49 = new BitSet(_tokenSet_49_data_);
	private static final long _tokenSet_50_data_[] = { 17592186044416L, 35182224651456L, 0L, 0L };
	public static final BitSet _tokenSet_50 = new BitSet(_tokenSet_50_data_);
	private static final long _tokenSet_51_data_[] = { 17592186044416L, 70366596740288L, 0L, 0L };
	public static final BitSet _tokenSet_51 = new BitSet(_tokenSet_51_data_);
	private static final long _tokenSet_52_data_[] = { 17592186044416L, 140735340917952L, 0L, 0L };
	public static final BitSet _tokenSet_52 = new BitSet(_tokenSet_52_data_);
	private static final long _tokenSet_53_data_[] = { 17592186044416L, 281472829273280L, 0L, 0L };
	public static final BitSet _tokenSet_53 = new BitSet(_tokenSet_53_data_);
	private static final long _tokenSet_54_data_[] = { 17592186044416L, 1125897759405248L, 0L, 0L };
	public static final BitSet _tokenSet_54 = new BitSet(_tokenSet_54_data_);
	private static final long _tokenSet_55_data_[] = { 17592186044416L, 36028794871526592L, 0L, 0L };
	public static final BitSet _tokenSet_55 = new BitSet(_tokenSet_55_data_);
	private static final long _tokenSet_56_data_[] = { 17592186044416L, 288230374004274368L, 0L, 0L };
	public static final BitSet _tokenSet_56 = new BitSet(_tokenSet_56_data_);
	private static final long _tokenSet_57_data_[] = { 72057594037927936L, 3458764513820540928L, 0L, 0L };
	public static final BitSet _tokenSet_57 = new BitSet(_tokenSet_57_data_);
	private static final long _tokenSet_58_data_[] = { 17592186044416L, 1152921502459409600L, 0L, 0L };
	public static final BitSet _tokenSet_58 = new BitSet(_tokenSet_58_data_);
	private static final long _tokenSet_59_data_[] = { 72075186223972352L, 4611686016279950528L, 0L, 0L };
	public static final BitSet _tokenSet_59 = new BitSet(_tokenSet_59_data_);
	private static final long _tokenSet_60_data_[] = { 35993612646875136L, 512L, 4092L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_60 = new BitSet(_tokenSet_60_data_);
	private static final long _tokenSet_61_data_[] = { 144097595889811456L, -2147434816L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_61 = new BitSet(_tokenSet_61_data_);
	private static final long _tokenSet_62_data_[] = { 35993612646875136L, -3746994889972248064L, 4095L, 0L, 0L, 0L };
	public static final BitSet _tokenSet_62 = new BitSet(_tokenSet_62_data_);
	private static final long _tokenSet_63_data_[] = { 108103983242936320L, -2147434784L, 0L, 0L };
	public static final BitSet _tokenSet_63 = new BitSet(_tokenSet_63_data_);
	
	}
