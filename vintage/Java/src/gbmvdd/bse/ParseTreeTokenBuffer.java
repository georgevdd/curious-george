package gbmvdd.bse;

import antlr.*;

import java.util.*;

public class ParseTreeTokenBuffer
{
	private ParseTreeLocation currentPath;
	private ParseTreeLocation lookaheadPath;

	private ParseTree firstParseTree = null;

	// TODO: replace this with a more efficient implementation.
	private List lookaheadCache = new LinkedList();
	private int numToConsume = 0;

	/** For keeping track of mark()/rewind(). markerOffset
	 * holds the number of tokens in the lookahead buffer
	 * before the current consume point. */
	private int markerOffset = 0;
	private int numMarkers = 0;

	public ParseTreeTokenBuffer() {}

	public ParseTreeTokenBuffer( ParseTree parseTree )
	{
		firstParseTree = parseTree;
		ParseTreeLocation newPath = new ParseTreeLocation();
		goToFirstValidNode( newPath );
		init( parseTree, newPath );
	}

	public ParseTreeTokenBuffer( ParseTree parseTree, ParseTreeLocation start )
	{
		init( parseTree, start );
	}

	private void init( ParseTree parseTree, ParseTreeLocation start )
	{
		firstParseTree = parseTree;
		currentPath = start;
		lookaheadPath = (ParseTreeLocation)currentPath.clone();
	}

/** The following five methods are called by parsers. */
	public void consume()
	{
		numToConsume++;
	}

	public int LA( int i ) throws TokenStreamException
	{
		fill( i );
		return LT( (i - 1) + markerOffset ).getType();
	}

	public Token LT( int i ) throws TokenStreamException
	{
		fill( i );
		return ((BSEToken)lookaheadCache.get( (i - 1) + markerOffset ));
	}

	public int mark()
	{
		numMarkers++;
		return markerOffset;
	}

	public void rewind( int pos )
	{
		numMarkers--;
		markerOffset = pos;
	}

/** Searches up the left edge of the parse tree, looking
 * for internal nodes with the requested type.
 *
 * @return	true if such a node is found; false otherwise.
 */
	public boolean beginsWith( int parseTreeType )
	{
		for( int i = currentPath.size() - 2; i >= 0; i-- )
		{
			ParseTree currentTree = (ParseTree)currentPath.get( i );
			if( currentTree.getType() == parseTreeType )
				return true;
		}

		return false;
	}

/** Picks a tree of type parseTreeType off the front of the input
 * tree and returns it. This method assumes that a call to
 * beginsWith( parseTreeType ) has returned true since the parse
 * tree was last modified. If this is not the case, the behaviour
 * is undefined!
 */
	public ParseTree consumeParseTree( int parseTreeType )
	{
		// Get rid of token on end of current path.
		BSEToken ignoreToken = (BSEToken)currentPath.pop();

		while( currentPath.size() > 0 )
		{
			int i = currentPath.size() - 1;

			ParseTree currentTree = (ParseTree)currentPath.peek();
			if( currentTree.getType() == parseTreeType )
			{
				// Found the subtree to consume.

				// All references to nodes in the subtree about to be
				// extracted have been popped from currentPath, so at
				// this point goToNextValidNode() will find the first
				// valid node that is not in that subtree - just what
				// is needed.
				goToNextValidNode( currentPath );

				ParseTree siblingTree = (ParseTree)currentTree.getNextSibling();

				// Cut the tree to be returned away from its siblings.
				// This will allow garbage collection to proceed
				// properly, as well as avoiding numous subtle bugs.
				currentTree.setNextSibling( null );

				if( i > 1 )
				{
					// currentTree has a parent.
					ParseTree parentTree = (ParseTree)currentPath.get( i - 1 );

					// This will cut currentTree out of the parse tree.
					parentTree.setFirstChild( siblingTree );
				}
				else
				{
					// currentTree is the root! There will be no more
					// tokens left after this operation is complete,
					// so empty the last node from the path.
					currentPath.pop();
				}

				// Now currentPath points to the correct next token
				// and the subtree has been excised. Keep lookaheadPath
				// in sync by flushing the lookahead buffer.
				lookaheadPath = (ParseTreeLocation)currentPath.clone();
				lookaheadCache.clear();

				// Finally return the tree.
				return currentTree;
			}
			else
				// Didn't find the right tree - pop the path and
				// try again.
				currentPath.pop();
		}

		// Code should never reach here - if it does, everything
		// is mucked up.
		throw new RuntimeException( "ParseTree corrupted by invalid call" +
			" to consumeParseTree()." );
	}

	public String toString()
	{
		StringBuffer result = new StringBuffer();
		result.append( "[" );
		result.append( "numToConsume=" + numToConsume + "; " );
		result.append( "numMarkers=" + numMarkers + "; " );
		result.append( "markerOffset=" + markerOffset + ";\n" );
		result.append( "currentPath=" + currentPath +";\n" );
		result.append( "lookaheadCache=" + lookaheadCache + ";\n" );
		result.append( "lookaheadPath=" + lookaheadPath );
		result.append( "]" );
		return result.toString();
	}

/** Eats all the tokens that have logically been consumed
 * If there are markers set (from mark()) then no tokens are
 * eaten; instead the position in the lookahead buffer is
 * bumped.
 */
	private void syncConsume()
	{
		while( numToConsume > 0 )
		{
			if( numMarkers > 0 )
			{
				markerOffset++;
			}
			else
			{
				lookaheadCache.remove( 0 );
//				consumeOneTokenFromParseTree();
				if( !currentPath.empty() )
				{
					currentPath.pop();
					goToNextValidNode( currentPath );
				}
			}
			numToConsume--;
		}
	}

/* Ensures that at least a given number of lookahead tokens are
 * available.
 *
 * Note about garbage collection: the only siblings a token can
 * have are the preceding whitespace/comment tokens, which need
 * to remain associated with the token anyway. As there are no
 * upward/backward links in the parse tree, there is no need to
 * unlink a token from the tree when reading it into the cache.
 */
	private void fill( int numTokens )
	{
		syncConsume();
		while( lookaheadCache.size() < numTokens )
		{
			if( lookaheadPath.empty() )
				lookaheadCache.add( new BSEToken( Token.EOF_TYPE, "<PTTS EOF>" ) );
			else
			{
				lookaheadCache.add( (BSEToken)lookaheadPath.pop() );
				goToNextValidNode( lookaheadPath );
			}
		}
	}

/** Moves a ParseTreeLocation to point to the first interesting
 * token in the parse tree.
 */
	private void goToFirstValidNode( ParseTreeLocation path )
	{
		ParseTree dummy = new ParseTree();
		dummy.setNextSibling( firstParseTree );
		path.push( dummy );

		goToNextValidNode( path );
	}

/** Moves a ParseTreeLocation to point to the next interesting
 * token in the parse tree.
 * Lifted from ParseTreeWhitespaceTokenStream. Different only
 * in that a parameter is passed to this version to say which
 * ParseTreeLocation to operate on, and no option to preserve
 * whitespace is given.
 */
	private void goToNextValidNode( ParseTreeLocation path )
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
					curPos = ((ParseTree)curPos).getVisibleParseTreeChild();
					if( curPos != null )
						path.push( curPos );
				}
			}
		} while( !path.empty() && !(path.peek() instanceof BSEToken) );
	}
}