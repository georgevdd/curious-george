package gbmvdd.bse;

import antlr.*;
import antlr.collections.*;
import java.util.*;

public class ParseTree implements AST, ParseTreeChild
{
//////////////////////
// Member variables //
//////////////////////

/** This may be another AST, or a BSEToken.
 * If it is a BSEToken, then it will have
 * been set by initialise( BSEToken ), and
 * the child will have no siblings. If it
 * is an AST, it will have been set by
 * addChild( AST node ), and it could have
 * siblings.
 */
	protected ParseTreeChild child = null;

	// This must be another AST or null.
	protected ParseTree nextSibling = null;

/** The number of lines that this AST adds to the length of
 * the file. An AST whose tokens contain no newlines will
 * have a newlineCount of 0, though it occupies one line.
 */
	protected int newlineCount = 0;

/** The length of this AST when represented as text. */
	protected int charCount = 0;

/** Temporary member; not needed. */
	private String text = "<no text>";



//////////////////
// Constructors //
//////////////////

	public ParseTree() {}
	public ParseTree( Token tok )
	{
		initialize( tok );
	}



///////////////////////////////
// Unimplemented AST methods //
///////////////////////////////

	public void initialize(AST t) { throw new UnsupportedOperationException(); }

	// TODO: fill these in, so that TreeWalkers can be used with ParseTrees.
	public boolean equals(AST t) { throw new UnsupportedOperationException(); }
	public boolean equalsList(AST t) { throw new UnsupportedOperationException(); }
	public boolean equalsListPartial(AST t) { throw new UnsupportedOperationException(); }
	public boolean equalsTree(AST t) { throw new UnsupportedOperationException(); }
	public boolean equalsTreePartial(AST t) { throw new UnsupportedOperationException(); }
	public ASTEnumeration findAll(AST tree) { throw new UnsupportedOperationException(); }
	public ASTEnumeration findAllPartial(AST subtree) { throw new UnsupportedOperationException(); }
	public String toStringList() { throw new UnsupportedOperationException(); }
	public String toStringTree() { throw new UnsupportedOperationException(); }



///////////////////////////
// Implementation of AST //
///////////////////////////

	public void initialize(int t, String txt)
	{
		setType(t);
		setText(txt);
	}

	public void initialize( Token tok )
	{
		child = (ParseTreeChild)tok;

		// Calculate the extent of this ParseTree.
		newlineCount = 0;
		charCount = 0;
		BSEToken t = (BSEToken)child;

		do
		{
			newlineCount += t.getNewlineCount();
			charCount += t.getCharCount();
			t = (BSEToken)t.getNextSiblingParseTreeChild();
		} while( (t != null) && (t != child) );
	}

	public void addChild( AST node )
	{
		if( node == null )
			return;

		// If an existing child is a Token and not an AST,
		// then a ClassCastException will be thrown here.
		// This is correct behaviour.
		ParseTree t = (ParseTree)child;
		if( t != null )
		{
			while( t.nextSibling != null )
			{
				t = t.nextSibling;
			}
			t.nextSibling = (ParseTree)node;
		}
		else
		{
			child = (ParseTree)node;
		}

		// Any siblings
		// that the added child already has will
		// implicitly be added to this parent, so they
		// must be taken into account. That is why
		// there is a loop here.
		gbmvdd.bse.ParseTreeChild n = (ParseTreeChild)node;
		while( n != null )
		{
			newlineCount += n.getNewlineCount();
			charCount += n.getCharCount();

			n = n.getNextSiblingParseTreeChild();
		}
	}

	public void setFirstChild(AST c) { child = (ParseTreeChild)c; }
	public void setNextSibling(AST n) { nextSibling = (ParseTree)n; }
	public void setText(String text) { this.text = text; }
	public void setType(int ttype) {}

	public AST getFirstChild() { if( child instanceof AST ) return (AST)child; else return null; }
	public AST getNextSibling() { return nextSibling; }
	public String getText()
	{
		if( child instanceof BSEToken )
			return ((BSEToken)child).getText();
		else
			return text;
	}
	public int getType()
	{
		if( child instanceof BSEToken )
			return ((BSEToken)child).getType();
		else
			return Token.INVALID_TYPE;
	}

	public String toString() { return String.valueOf( getType() ) + " (" + getText() + ")"; }



////////////////////////////////
// ParseTree-specific methods //
////////////////////////////////

/** Returns a TokenStream whose nextToken() method will return the
 * tokens of this parse tree in the order that they were encountered
 * in the input stream.
 *
 * @param includeWhitespace if true, whitespace and comment tokens
 * will be included in the token stream. If false, only tokens that
 * the parser encountered will be present in the token stream.
 */
	public TokenStream getTokenStream( boolean includeWhitespace )
	{
		return new ParseTreeTokenStream( this, includeWhitespace );
	}

/** Returns a TokenStream whose nextToken() method will return the
 * tokens of this parse tree in the order that they were encountered
 * in the input stream. Only tokens that the parser encountered will
 * be present in the token stream.
 */
	public TokenStream getTokenStream() { return getTokenStream( false ); }

	public boolean beginsWith( int parseTreeType )
	{
		if( getType() == parseTreeType )
			return true;
		if( child instanceof ParseTree )
			return ((ParseTree)child).beginsWith( parseTreeType );
		return false;
	}

// The following are quivalents to AST methods, but returning
// ParseTreeChildren instead.
// If a tree is viewed as an AST, then its leaves are ASTs. If it
// is viewed as a ParseTree, then its leaves are BSETokens and its
// internal nodes are ParseTrees.

/** Returns the first child of this node. If the children are BSETokens
 * rather than ParseTrees, then this will return the child of this node
 * that came first in the input stream. Because whitespace and comment
 * tokens are stored in the same node as the visible token that comes
 * after them, this will probably be a whitespace or comment token.
 */
	public ParseTreeChild getFirstParseTreeChild()
	{
		if( child instanceof BSEToken )
			return child.getNextSiblingParseTreeChild();
		else
			return child;
	}

/** Returns the child that the Parser actually saw when constructing this
 * parse tree. Because whitespace and comment tokens are stored in the same
 * node as the visible token that comes after them, this will be the child
 * of this node that appeared last in the input stream.
 */
	public ParseTreeChild getVisibleParseTreeChild() { return child; }



//////////////////////////////////////
// Implementation of ParseTreeChild //
//////////////////////////////////////

/** Returns the next sibling of this parse tree, or null if there is none.
 * This behaviour is identical to that of @AST.getNextSibling().
 */
	public ParseTreeChild getNextSiblingParseTreeChild() { return nextSibling; }

/** Returns the number of newline characters in the text spanned by
 * this parse tree.
 */
	public int getNewlineCount() { return newlineCount; }

/** Returns the number of characters in the text spanned
 * by this parse tree.
 */
	public int getCharCount() { return charCount; }

/** Converts an offset into the text of this parse tree into a
 * ParseTreeLocation.
 *
 * @param offset a 0-based offset in characters into the text
 * of this parse tree.
 *
 * @return a ParseTreeLocation corresponding to the offset
 * supplied, or null if (offset < 0 || offset >= charCount).
 */
	public ParseTreeLocation getLocationForOffset( int offset )
	{
		ParseTreeLocation result = new ParseTreeLocation();
		return getLocationForOffset( offset, result );
	}

	public ParseTreeLocation getLocationForOffset( int offset,
		ParseTreeLocation partialLocation )
	{
		if( offset >= charCount )
			return null;

		partialLocation.push( this );

		ParseTreeChild curChild = getFirstParseTreeChild();

		while( true )
		{
			if( curChild == null )
				System.out.println( "Help!" );

			if( curChild.getCharCount() > offset )
				return curChild.getLocationForOffset( offset, partialLocation );

			else
			{
				offset -= curChild.getCharCount();
				curChild = curChild.getNextSiblingParseTreeChild();
			}
		}
	}

/** Converts a line number into an offset.
 *
 * @param lineNumber a 0-based line number.
 *
 * @return the 0-based offset, in characters into the text of
 * this parse tree, of the line. If lineNumber is 0, the result
 * is always 0. If lineNumber is less than 0 or greater than
 * the number of newlines in this parse tree, the result is -1.
 */
	public int getOffsetForLineNumber( int lineNumber )
	{
		if( lineNumber > getNewlineCount() || lineNumber < 0 )
			return -1;
			
		// This shortcut is necessary. If this ParseTree has no
		// children, then the if condition below will throw a
		// NullPointerException. But in that case, this ParseTree
		// will have a newlineCount of 0, so it is known that this
		// routine could only have been called if lineNumber is 0
		// too.
		if( lineNumber == 0 )
			return 0;

		int offset = 0;

		ParseTreeChild curChild = getFirstParseTreeChild();
		while( true )
		{
			if( curChild.getNewlineCount() >= lineNumber )
			{
				return offset + curChild.getOffsetForLineNumber( lineNumber );
			}
			else
			{
				lineNumber -= curChild.getNewlineCount();
				offset += curChild.getCharCount();
				curChild = curChild.getNextSiblingParseTreeChild();
			}
		}
	}

/** Returns the number of the line which contains an offset.
 *
 * @param offset a 0-based character offset into the document.
 */
	public int getLineNumberForOffset( int offset )
	{
		if( offset > getCharCount() || offset < 0 )
			return -1;

		// This shortcut is necessary: see the corresponding
		// code in getOffsetForLineNumber for details.
		if( offset == 0 )
			return 0;

		int lineNum = 0;

		ParseTreeChild curChild = getFirstParseTreeChild();
		while( true )
		{
			if( curChild.getCharCount() >= offset )
			{
				return lineNum + curChild.getLineNumberForOffset( offset );
			}
			else
			{
				offset -= curChild.getCharCount();
				lineNum += curChild.getNewlineCount();
				curChild = curChild.getNextSiblingParseTreeChild();
			}
		}
	}
}