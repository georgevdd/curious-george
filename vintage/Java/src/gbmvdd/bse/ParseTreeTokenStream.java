package gbmvdd.bse;

import java.util.*;
import antlr.*;

public class ParseTreeTokenStream implements TokenStream
{
	boolean returnNullAtEOF;
	private final boolean withWhitespace;
	
	/** The path needs to be accessed from TokenStreamReader
	 * so that it can be constructed for ParseTreeLocations that
	 * are not token-aligned. That is why it is not private.
	 */
	ParseTreeLocation path;

	public ParseTreeTokenStream( ParseTree root, boolean withWhitespace,
		boolean returnNullAtEOF )
	{
		this.withWhitespace = withWhitespace;
		this.returnNullAtEOF = returnNullAtEOF;

		path = new ParseTreeLocation();
		ParseTree dummy = new ParseTree();
		dummy.setNextSibling( root );
		path.push( dummy );
		goToNextValidNode();
	}
	
	public ParseTreeTokenStream( ParseTreeLocation start, boolean withWhitespace,
		boolean returnNullAtEOF )
	{
		this.withWhitespace = withWhitespace;
		this.returnNullAtEOF = returnNullAtEOF;
		path = start;

		// If the current location points to ignored tokens, then it should be
		// advanced to the next non-ignored token.
		if( !withWhitespace && !path.empty() )
		{
			BSEToken currentToken = (BSEToken)path.pop();
			path.push( ((ParseTree)path.peek()).getVisibleParseTreeChild() );
		}
	}

	public ParseTreeTokenStream( ParseTree root, boolean withWhitespace )
	{ this( root, withWhitespace, false ); }
	
	public ParseTreeTokenStream( ParseTreeLocation start, boolean withWhitespace )
	{ this( start, withWhitespace, false ); }



	public Token nextToken()
	{
		if( path.empty() )
			if( returnNullAtEOF )
				return null;
			else
				return new BSEToken( Token.EOF_TYPE, "<PTTS EOF>" );

		// At this point the top of the stack always is a BSEToken.
		// This Token will be returned.
		
		ParseTreeChild curPos = (ParseTreeChild)path.pop();
		BSEToken result = (BSEToken)curPos;

		if( withWhitespace )
		{
			// If the token's next sibling is the first child of its parent,
			// then all the parent's children have been extracted, so move
			// on to the next ParseTree.
			BSEToken nextChild = (BSEToken)curPos.getNextSiblingParseTreeChild();
			if( nextChild == (BSEToken)(((ParseTree)path.peek()).getFirstParseTreeChild()) )
				goToNextValidNode();
			else
				path.push( nextChild );
		}		
		else
			goToNextValidNode();
	

		return result;
	}
		
/** Advances the ParseTreeLocation to point to the next token in
 * the parse tree. The next token is defined as the first token, to
 * the right of the current location, which is not a descendant of
 * every internal node listed in the current location. If the location
 * already points to a valid node, then upon return the location will
 * either be another valid node, or null (i.e. an empty stack).
 */
	private void goToNextValidNode()
	{
		// This needs to be in a loop because some ParseTrees could
		// have no children. So if step (3) finishes and there is
		// no BSEToken on the bottom, the whole process must be
		// repeated.
		do
		{
			ParseTreeChild curPos = (ParseTree)path.peek();
			// Go to next internal node (ParseTree):

			// (1) Go up the tree until a node has a sibling.
			while( (curPos.getNextSiblingParseTreeChild() == null) )
			{
				path.pop();
				if( path.empty() )
					break;
				curPos = (ParseTree)path.peek();
			}

			if( !path.empty() )
			{
				// (2) Go across to that sibling.
				path.pop(); // Popped object is same as curPos.
				// curPos is guaranteed non-null because
				// the while loop terminated and the path is not empty.
				curPos = curPos.getNextSiblingParseTreeChild();
				path.push( curPos );

				// (3) Go down as deep as possible.
				while( curPos instanceof ParseTree )
				{
					curPos = withWhitespace ?
						((ParseTree)curPos).getFirstParseTreeChild() :
						((ParseTree)curPos).getVisibleParseTreeChild();
					if( curPos != null )
						path.push( curPos );
				}
			}
		} while( !path.empty() && !(path.peek() instanceof BSEToken) );
	}

}
