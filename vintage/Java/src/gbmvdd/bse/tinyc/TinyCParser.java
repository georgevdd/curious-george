// $ANTLR 2.7.1: "tinyc.g" -> "TinyCParser.java"$
// GBMvdD's BSE code generator made this.

package gbmvdd.bse.tinyc;

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

//import java.io.*;
import gbmvdd.util.*;
import gbmvdd.bse.*;

public class TinyCParser extends gbmvdd.bse.BSEParser
       implements TinyCParserTokenTypes
 {



	public gbmvdd.bse.ParseTree parseEntireStream()
	{
		try
		{
			program();
			return (gbmvdd.bse.ParseTree)getAST();
		}
		catch( Exception e )
		{
			return null;
		}
	}


	public TinyCParser( ParseTreeTokenBuffer pttb, int k_ )
	{
		super( pttb, k_ );
	}

	public int k() { return 1; }


protected TinyCParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public TinyCParser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected TinyCParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public TinyCParser(TokenStream lexer) {
  this(lexer,1);
}

public TinyCParser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
}

	public final void program() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST program_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			{
			_loop3:
			do {
				if ((LA(1)==TK_int||LA(1)==TK_char||LA(1)==ID)) {
					declaration();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop3;
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
			program_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_0.member( LA( 1 ) )) );
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
				program_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 0, "program" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( program_AST );
	}
	
	public final void declaration() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST declaration_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			boolean synPredMatched6 = false;
			if (((LA(1)==TK_int||LA(1)==TK_char||LA(1)==ID))) {
				int _m6 = mark();
				synPredMatched6 = true;
				inputState.guessing++;
				try {
					{
					variable();
					}
				}
				catch (RecognitionException pe) {
					synPredMatched6 = false;
				}
				rewind(_m6);
				inputState.guessing--;
			}
			if ( synPredMatched6 ) {
				variable();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				declaration_AST = (AST)currentAST.root;
			}
			else if ((LA(1)==TK_int||LA(1)==TK_char||LA(1)==ID)) {
				function();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				declaration_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_1.member( LA( 1 ) )) );
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
		returnAST = astFactory.create( 1, "declaration" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( declaration_AST );
	}
	
	public final void variable() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST variable_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			type();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			declarator();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			Token unlabelledAtom2 = LT(1);
			AST unlabelledAtom2_AST = null;
			match(SEMI);
			if (inputState.guessing==0) {
				unlabelledAtom2_AST = (AST)astFactory.create(unlabelledAtom2);
				astFactory.addASTChild(currentAST, unlabelledAtom2_AST);
			}
			variable_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_1.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				variable_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 2, "variable" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( variable_AST );
	}
	
	public final void function() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST function_AST = null;
		Token  id = null;
		AST id_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			type();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			id = LT(1);
			match(ID);
			if (inputState.guessing==0) {
				id_AST = (AST)astFactory.create(id);
				astFactory.addASTChild(currentAST, id_AST);
			}
			Token unlabelledAtom3 = LT(1);
			AST unlabelledAtom3_AST = null;
			match(LPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom3_AST = (AST)astFactory.create(unlabelledAtom3);
				astFactory.addASTChild(currentAST, unlabelledAtom3_AST);
			}
			{
			switch ( LA(1)) {
			case TK_int:
			case TK_char:
			case ID:
			{
				formalParameter();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				_loop12:
				do {
					if ((LA(1)==COMMA)) {
						Token unlabelledAtom4 = LT(1);
						AST unlabelledAtom4_AST = null;
						match(COMMA);
						if (inputState.guessing==0) {
							unlabelledAtom4_AST = (AST)astFactory.create(unlabelledAtom4);
							astFactory.addASTChild(currentAST, unlabelledAtom4_AST);
						}
						formalParameter();
						if (inputState.guessing==0) {
							astFactory.addASTChild(currentAST, returnAST);
						}
					}
					else {
						break _loop12;
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
			Token unlabelledAtom5 = LT(1);
			AST unlabelledAtom5_AST = null;
			match(RPAREN);
			if (inputState.guessing==0) {
				unlabelledAtom5_AST = (AST)astFactory.create(unlabelledAtom5);
				astFactory.addASTChild(currentAST, unlabelledAtom5_AST);
			}
			block();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			function_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_1.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				function_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 3, "function" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( function_AST );
	}
	
	public final void declarator() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST declarator_AST = null;
		Token  id = null;
		AST id_AST = null;
		Token  id2 = null;
		AST id2_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			switch ( LA(1)) {
			case ID:
			{
				id = LT(1);
				match(ID);
				if (inputState.guessing==0) {
					id_AST = (AST)astFactory.create(id);
					astFactory.addASTChild(currentAST, id_AST);
				}
				declarator_AST = (AST)currentAST.root;
				break;
			}
			case STAR:
			{
				Token unlabelledAtom6 = LT(1);
				AST unlabelledAtom6_AST = null;
				match(STAR);
				if (inputState.guessing==0) {
					unlabelledAtom6_AST = (AST)astFactory.create(unlabelledAtom6);
					astFactory.addASTChild(currentAST, unlabelledAtom6_AST);
				}
				id2 = LT(1);
				match(ID);
				if (inputState.guessing==0) {
					id2_AST = (AST)astFactory.create(id2);
					astFactory.addASTChild(currentAST, id2_AST);
				}
				declarator_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_2.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				declarator_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 4, "declarator" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( declarator_AST );
	}
	
	public final void type() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST type_AST = null;
		Token  id = null;
		AST id_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			{
			switch ( LA(1)) {
			case TK_int:
			{
				Token unlabelledAtom7 = LT(1);
				AST unlabelledAtom7_AST = null;
				match(TK_int);
				if (inputState.guessing==0) {
					unlabelledAtom7_AST = (AST)astFactory.create(unlabelledAtom7);
					astFactory.addASTChild(currentAST, unlabelledAtom7_AST);
				}
				break;
			}
			case TK_char:
			{
				Token unlabelledAtom8 = LT(1);
				AST unlabelledAtom8_AST = null;
				match(TK_char);
				if (inputState.guessing==0) {
					unlabelledAtom8_AST = (AST)astFactory.create(unlabelledAtom8);
					astFactory.addASTChild(currentAST, unlabelledAtom8_AST);
				}
				break;
			}
			case ID:
			{
				id = LT(1);
				match(ID);
				if (inputState.guessing==0) {
					id_AST = (AST)astFactory.create(id);
					astFactory.addASTChild(currentAST, id_AST);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			type_AST = (AST)currentAST.root;
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
				type_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 5, "type" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( type_AST );
	}
	
	public final void formalParameter() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST formalParameter_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			type();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			declarator();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			formalParameter_AST = (AST)currentAST.root;
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
				formalParameter_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 6, "formalParameter" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( formalParameter_AST );
	}
	
	public final void block() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST block_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			Token unlabelledAtom9 = LT(1);
			AST unlabelledAtom9_AST = null;
			match(LCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom9_AST = (AST)astFactory.create(unlabelledAtom9);
				astFactory.addASTChild(currentAST, unlabelledAtom9_AST);
			}
			{
			_loop18:
			do {
				if ((_tokenSet_5.member(LA(1)))) {
					statement();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop18;
				}
				
			} while (true);
			}
			Token unlabelledAtom10 = LT(1);
			AST unlabelledAtom10_AST = null;
			match(RCURLY);
			if (inputState.guessing==0) {
				unlabelledAtom10_AST = (AST)astFactory.create(unlabelledAtom10);
				astFactory.addASTChild(currentAST, unlabelledAtom10_AST);
			}
			block_AST = (AST)currentAST.root;
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_1.member( LA( 1 ) )) );
				if( err_AST.child != null )
				{
					AST err_AST_root = astFactory.create( Token.INVALID_TYPE, ex.getMessage() );
					astFactory.makeASTRoot( err_AST, err_AST_root );
					astFactory.addASTChild( currentAST, err_AST.root );
				}
				block_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 7, "block" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( block_AST );
	}
	
	public final void statement() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST statement_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			switch ( LA(1)) {
			case TK_if:
			{
				Token unlabelledAtom11 = LT(1);
				AST unlabelledAtom11_AST = null;
				match(TK_if);
				if (inputState.guessing==0) {
					unlabelledAtom11_AST = (AST)astFactory.create(unlabelledAtom11);
					astFactory.addASTChild(currentAST, unlabelledAtom11_AST);
				}
				Token unlabelledAtom12 = LT(1);
				AST unlabelledAtom12_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom12_AST = (AST)astFactory.create(unlabelledAtom12);
					astFactory.addASTChild(currentAST, unlabelledAtom12_AST);
				}
				expr();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom13 = LT(1);
				AST unlabelledAtom13_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom13_AST = (AST)astFactory.create(unlabelledAtom13);
					astFactory.addASTChild(currentAST, unlabelledAtom13_AST);
				}
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				{
				if ((LA(1)==TK_else)) {
					Token unlabelledAtom14 = LT(1);
					AST unlabelledAtom14_AST = null;
					match(TK_else);
					if (inputState.guessing==0) {
						unlabelledAtom14_AST = (AST)astFactory.create(unlabelledAtom14);
						astFactory.addASTChild(currentAST, unlabelledAtom14_AST);
					}
					statement();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else if ((_tokenSet_6.member(LA(1)))) {
				}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case TK_while:
			{
				Token unlabelledAtom15 = LT(1);
				AST unlabelledAtom15_AST = null;
				match(TK_while);
				if (inputState.guessing==0) {
					unlabelledAtom15_AST = (AST)astFactory.create(unlabelledAtom15);
					astFactory.addASTChild(currentAST, unlabelledAtom15_AST);
				}
				Token unlabelledAtom16 = LT(1);
				AST unlabelledAtom16_AST = null;
				match(LPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom16_AST = (AST)astFactory.create(unlabelledAtom16);
					astFactory.addASTChild(currentAST, unlabelledAtom16_AST);
				}
				expr();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				Token unlabelledAtom17 = LT(1);
				AST unlabelledAtom17_AST = null;
				match(RPAREN);
				if (inputState.guessing==0) {
					unlabelledAtom17_AST = (AST)astFactory.create(unlabelledAtom17);
					astFactory.addASTChild(currentAST, unlabelledAtom17_AST);
				}
				statement();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			case LCURLY:
			{
				block();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				statement_AST = (AST)currentAST.root;
				break;
			}
			default:
				boolean synPredMatched21 = false;
				if (((LA(1)==TK_int||LA(1)==TK_char||LA(1)==ID))) {
					int _m21 = mark();
					synPredMatched21 = true;
					inputState.guessing++;
					try {
						{
						declaration();
						}
					}
					catch (RecognitionException pe) {
						synPredMatched21 = false;
					}
					rewind(_m21);
					inputState.guessing--;
				}
				if ( synPredMatched21 ) {
					declaration();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					statement_AST = (AST)currentAST.root;
				}
				else if ((_tokenSet_7.member(LA(1)))) {
					expr();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
					Token unlabelledAtom18 = LT(1);
					AST unlabelledAtom18_AST = null;
					match(SEMI);
					if (inputState.guessing==0) {
						unlabelledAtom18_AST = (AST)astFactory.create(unlabelledAtom18);
						astFactory.addASTChild(currentAST, unlabelledAtom18_AST);
					}
					statement_AST = (AST)currentAST.root;
				}
			else {
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
				} while( (LA( 1 ) != Token.EOF_TYPE) && (!_tokenSet_6.member( LA( 1 ) )) );
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
		returnAST = astFactory.create( 8, "statement" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( statement_AST );
	}
	
	public final void expr() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST expr_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			assignExpr();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			expr_AST = (AST)currentAST.root;
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
				expr_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 9, "expr" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( expr_AST );
	}
	
	public final void assignExpr() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST assignExpr_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			aexpr();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			switch ( LA(1)) {
			case ASSIGN:
			{
				Token unlabelledAtom19 = LT(1);
				AST unlabelledAtom19_AST = null;
				match(ASSIGN);
				if (inputState.guessing==0) {
					unlabelledAtom19_AST = (AST)astFactory.create(unlabelledAtom19);
					astFactory.addASTChild(currentAST, unlabelledAtom19_AST);
				}
				assignExpr();
				if (inputState.guessing==0) {
					astFactory.addASTChild(currentAST, returnAST);
				}
				break;
			}
			case RPAREN:
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
			assignExpr_AST = (AST)currentAST.root;
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
				assignExpr_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 10, "assignExpr" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( assignExpr_AST );
	}
	
	public final void aexpr() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST aexpr_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			mexpr();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop28:
			do {
				if ((LA(1)==PLUS)) {
					Token unlabelledAtom20 = LT(1);
					AST unlabelledAtom20_AST = null;
					match(PLUS);
					if (inputState.guessing==0) {
						unlabelledAtom20_AST = (AST)astFactory.create(unlabelledAtom20);
						astFactory.addASTChild(currentAST, unlabelledAtom20_AST);
					}
					mexpr();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop28;
				}
				
			} while (true);
			}
			aexpr_AST = (AST)currentAST.root;
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
				aexpr_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 11, "aexpr" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( aexpr_AST );
	}
	
	public final void mexpr() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST mexpr_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			atom();
			if (inputState.guessing==0) {
				astFactory.addASTChild(currentAST, returnAST);
			}
			{
			_loop31:
			do {
				if ((LA(1)==STAR)) {
					Token unlabelledAtom21 = LT(1);
					AST unlabelledAtom21_AST = null;
					match(STAR);
					if (inputState.guessing==0) {
						unlabelledAtom21_AST = (AST)astFactory.create(unlabelledAtom21);
						astFactory.addASTChild(currentAST, unlabelledAtom21_AST);
					}
					atom();
					if (inputState.guessing==0) {
						astFactory.addASTChild(currentAST, returnAST);
					}
				}
				else {
					break _loop31;
				}
				
			} while (true);
			}
			mexpr_AST = (AST)currentAST.root;
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
				mexpr_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 12, "mexpr" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( mexpr_AST );
	}
	
	public final void atom() throws RecognitionException, TokenStreamException {
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST atom_AST = null;
		
		boolean parseFailed = false;
		try {      // for error handling
			switch ( LA(1)) {
			case ID:
			{
				Token unlabelledAtom22 = LT(1);
				AST unlabelledAtom22_AST = null;
				match(ID);
				if (inputState.guessing==0) {
					unlabelledAtom22_AST = (AST)astFactory.create(unlabelledAtom22);
					astFactory.addASTChild(currentAST, unlabelledAtom22_AST);
				}
				atom_AST = (AST)currentAST.root;
				break;
			}
			case INT:
			{
				Token unlabelledAtom23 = LT(1);
				AST unlabelledAtom23_AST = null;
				match(INT);
				if (inputState.guessing==0) {
					unlabelledAtom23_AST = (AST)astFactory.create(unlabelledAtom23);
					astFactory.addASTChild(currentAST, unlabelledAtom23_AST);
				}
				atom_AST = (AST)currentAST.root;
				break;
			}
			case CHAR_LITERAL:
			{
				Token unlabelledAtom24 = LT(1);
				AST unlabelledAtom24_AST = null;
				match(CHAR_LITERAL);
				if (inputState.guessing==0) {
					unlabelledAtom24_AST = (AST)astFactory.create(unlabelledAtom24);
					astFactory.addASTChild(currentAST, unlabelledAtom24_AST);
				}
				atom_AST = (AST)currentAST.root;
				break;
			}
			case STRING_LITERAL:
			{
				Token unlabelledAtom25 = LT(1);
				AST unlabelledAtom25_AST = null;
				match(STRING_LITERAL);
				if (inputState.guessing==0) {
					unlabelledAtom25_AST = (AST)astFactory.create(unlabelledAtom25);
					astFactory.addASTChild(currentAST, unlabelledAtom25_AST);
				}
				atom_AST = (AST)currentAST.root;
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
				atom_AST = (AST)currentAST.root;
			} else {
			  throw ex;
			}
		}
		returnAST = astFactory.create( 13, "atom" +
			( parseFailed ? "<bad>" : "" ) );
		returnAST.addChild( atom_AST );
	}
	
	public BitSet getIgnoredTokens()
	{
		return _tokenSet_12;
	}
	

	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"\"int\"",
		"\"char\"",
		"\"if\"",
		"\"else\"",
		"\"while\"",
		"WS",
		"SL_COMMENT",
		"ML_COMMENT",
		"'('",
		"')'",
		"LCURLY",
		"RCURLY",
		"STAR",
		"PLUS",
		"ASSIGN",
		"SEMI",
		"COMMA",
		"CHAR_LITERAL",
		"STRING_LITERAL",
		"ESC",
		"DIGIT",
		"INT",
		"an identifier"
	};
	
	private static final long _tokenSet_0_data_[] = { 2L, 0L };
	public static final BitSet _tokenSet_0 = new BitSet(_tokenSet_0_data_);
	private static final long _tokenSet_1_data_[] = { 107004402L, 0L };
	public static final BitSet _tokenSet_1 = new BitSet(_tokenSet_1_data_);
	private static final long _tokenSet_2_data_[] = { 1581056L, 0L };
	public static final BitSet _tokenSet_2 = new BitSet(_tokenSet_2_data_);
	private static final long _tokenSet_3_data_[] = { 67174400L, 0L };
	public static final BitSet _tokenSet_3 = new BitSet(_tokenSet_3_data_);
	private static final long _tokenSet_4_data_[] = { 1056768L, 0L };
	public static final BitSet _tokenSet_4 = new BitSet(_tokenSet_4_data_);
	private static final long _tokenSet_5_data_[] = { 106971504L, 0L };
	public static final BitSet _tokenSet_5 = new BitSet(_tokenSet_5_data_);
	private static final long _tokenSet_6_data_[] = { 107004400L, 0L };
	public static final BitSet _tokenSet_6 = new BitSet(_tokenSet_6_data_);
	private static final long _tokenSet_7_data_[] = { 106954752L, 0L };
	public static final BitSet _tokenSet_7 = new BitSet(_tokenSet_7_data_);
	private static final long _tokenSet_8_data_[] = { 532480L, 0L };
	public static final BitSet _tokenSet_8 = new BitSet(_tokenSet_8_data_);
	private static final long _tokenSet_9_data_[] = { 794624L, 0L };
	public static final BitSet _tokenSet_9 = new BitSet(_tokenSet_9_data_);
	private static final long _tokenSet_10_data_[] = { 925696L, 0L };
	public static final BitSet _tokenSet_10 = new BitSet(_tokenSet_10_data_);
	private static final long _tokenSet_11_data_[] = { 991232L, 0L };
	public static final BitSet _tokenSet_11 = new BitSet(_tokenSet_11_data_);
	private static final long _tokenSet_12_data_[] = { 3584L, 0L };
	public static final BitSet _tokenSet_12 = new BitSet(_tokenSet_12_data_);
	
	}
