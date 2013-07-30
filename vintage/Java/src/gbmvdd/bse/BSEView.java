package gbmvdd.bse;

import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;
import java.util.*;

import gbmvdd.util.*;

import antlr.*;

public class BSEView extends Canvas implements BSEModelListener, MouseMotionListener,
	MouseListener, KeyListener, AdjustmentListener, FocusListener, ComponentListener
{
	private Log log = Log.defaultLog;
	public void setLog( Log log ) { this.log = log; log.log( "[BSEView set to log here]" ); }

	private BSEModel model = null;
	private Image backBuffer;
	private static final Toolkit toolkit = Toolkit.getDefaultToolkit();
	private static final Dimension minSize = new Dimension( 0, 0 );

	private int tabWidth = 8;
	private Font font;
	FontMetrics fontMetrics = null;
	int lineHeight; // Height of a line in pixels.
	int lineMaxAscent; // Maximum height of a text line above its baseline.
	int charWidth; // Width of a character in pixels.
	boolean useMagicCaret = true;
	/** Currently ignored */
	private final boolean wordWrap = false;
	private final boolean virtualSpace = false;
	private final boolean blockCaret = true;

	/** Caret position. */
	private int selStart = 0;
	private int selEnd = 0;
	private int magicCaretCol = 0;
	
	private Color caretColourFocus = Color.white;
	private Color caretColourNoFocus = Color.darkGray;
	private Color curCaretColour = caretColourNoFocus;

	/** Coordinates of top left of view in document. */
	private int firstVisibleLine = 0, firstVisibleColumn = 0;

	/** Updated by RenderContext during painting. */
	private int visibleRows = 0;
	private int visibleColumns = 0;
	private int longestVisibleRowLength = 0;

	private Vector visibleTabLists = new Vector();

	private Scrollbar hScrollbar = null, vScrollbar = null;
	private PopupMenu popupMenu = null;

	private void reset()
	{
		firstVisibleLine = firstVisibleColumn = 0;
		selStart = selEnd = 0;
		magicCaretCol = 0;
		visibleRows = visibleColumns = 0;
		longestVisibleRowLength = 0;
		visibleTabLists.clear();
	}

//////////////////////
// Public interface //
//////////////////////

	public BSEView( BSEModel model )
	{
		font = new Font( "Monospaced", Font.BOLD, 14 );

		addMouseListener( this );
		addMouseMotionListener( this );
		addKeyListener( this );
		addFocusListener( this );
		addComponentListener( this );
		setModel( model );
	}
	
	public void setModel( BSEModel newModel )
	{
		if( model != null )
			model.removeBSEModelListener( this );
		
		reset();

		model = newModel;
		if( model != null ) model.addBSEModelListener( this );
		
		repaint();
	}

	public BSEModel getModel()
	{
		return model;
	}

	/** Sets the start and end offsets of the selection. If the
	* offsets are the same, the caret is positioned (the caret is effectively
	* a selection of length zero). No checks are made as to the validity
	* of the offsets. If necessary, the selection is erased and redrawn at
	* its new position.
	*
	* @param newSelStart the start of the selection.
	* @param newSelEnd the end of the selection.
	*/
	public void setSelection( int newSelStart, int newSelEnd )
	{
		setSelection( newSelStart, newSelEnd, true );
	}
	
	/** Consults the language object to find out what colour a
	* particular token should be.
	*
	* @param token the token whose colour is to be found.
	*
	* @return the colour in which the token should be painted.
	*/
	public Color getColourForToken( BSEToken token )
	{
		return model.getLanguage().getTokenColour
			( token, model.getLanguageExtraData() );
	}
	
	/** Consults the language object to find out what colour a
	* particular token's background should be.
	*
	* @param token the token whose background colour is to be found.
	*
	* @return the background colour for the token.
	*/
	public Color getBackgroundForToken( BSEToken token )
	{
		return model.getLanguage().getBackgroundColour
			( token, model.getLanguageExtraData() );
	}

	/** Gets the monospaced font in which text is drawn. */
	public Font getFont() { return font; }

	public int getMaxHScroll()
	{
		int max = longestVisibleRowLength - getHCaretInset();
		return (max < 0) ? 0 : max;
	}
	
	public int getMaxVScroll()
	{
		int max = model.getLineCount() - getVCaretInset();
		return (max < 0) ? 0 : max;
	}

	public void setHScrollbar( Scrollbar scrollbar )
	{
		if( hScrollbar != null )
			hScrollbar.removeAdjustmentListener( this );

		hScrollbar = scrollbar;
		if( hScrollbar != null ) hScrollbar.addAdjustmentListener( this );
	}

	public void setVScrollbar( Scrollbar scrollbar )
	{
		if( vScrollbar != null )
			vScrollbar.removeAdjustmentListener( this );

		vScrollbar = scrollbar;
		if( vScrollbar != null ) vScrollbar.addAdjustmentListener( this );
	}
	
	public void setPopupMenu( PopupMenu pm )
	{
		if( popupMenu != null )
			remove( popupMenu );
		popupMenu = pm;
		if( popupMenu != null )
			add( popupMenu );
	}

	/** Positions the view in the document so that a given column and row
	* are at the top left of the view.
	*
	* @param column the column that is to be leftmost on screen. If outside
	* [0, getMaxHScroll()], the value is clamped to that range.
	*
	* @param line the line that is to be topmost on screen. If outside
	* [0, getMaxVScroll()], the value is clamped to that range.
	*/
	public void moveViewToPosition( int column, int line )
	{
		if( line < 0 )
			line = 0;
		else if( line > getMaxVScroll() )
			line = getMaxVScroll();

		if( column < 0 )
			column = 0;
		else if( column > getMaxHScroll() )
			column = getMaxHScroll();

		firstVisibleLine = line; firstVisibleColumn = column;

		repaint();
	}
	
	/** Positions the view so that a given offset is visible on
	* the screen, and not too close to the edge if possible.
	*/
	public void ensureOffsetIsVisible( int offset )
	{
		if( offset < 0 || offset > model.getMaxOffset() )
			throw new IllegalArgumentException();

		int line = model.getLineNumberForOffset( offset );
		int offsetInLine = offset - model.getOffsetForLineNumber( line );
		int col = getColForChar( offsetInLine, line );
		
		int newFirstVisibleColumn = firstVisibleColumn;
		int newFirstVisibleLine = firstVisibleLine;

		if( newFirstVisibleColumn < (col + getHCaretInset()) - visibleColumns )
			newFirstVisibleColumn = (col + getHCaretInset()) - visibleColumns;
		else if( newFirstVisibleColumn > (col - getHCaretInset()) )
			newFirstVisibleColumn = col - getHCaretInset();

		if( newFirstVisibleLine < (line + getVCaretInset()) - visibleRows )
			newFirstVisibleLine = (line + getVCaretInset()) - visibleRows;
		else if( newFirstVisibleLine > (line - getVCaretInset()) )
			newFirstVisibleLine = line - getVCaretInset();
		

		if( newFirstVisibleColumn != firstVisibleColumn ||
			newFirstVisibleLine != firstVisibleLine )
			moveViewToPosition( newFirstVisibleColumn, newFirstVisibleLine );
	}

	/** Overridden so that this component can be an input focus. */
	public boolean isFocusTraversable() { return true; }
	
	public void update( Graphics g )
	{
		paint( g );
	}
	
	/** Overridden so that JScrollPane doesn't throw a fit */
	public Dimension getMinimumSize()
	{
		return minSize;
	}



////////////
// Redraw //
////////////

	public void paint( Graphics screenGraphics )
	{
		Graphics g = backBuffer.getGraphics();

		// (1) Recalculate the various dimensions of the view.
		fontMetrics = getGraphics().getFontMetrics( font );
		lineHeight = fontMetrics.getHeight();
		lineMaxAscent = fontMetrics.getMaxAscent();
		charWidth = fontMetrics.getWidths()['M'];

		g.setColor( Color.white );
		g.fillRect( Integer.MIN_VALUE>>1, Integer.MIN_VALUE>>1,
					Integer.MAX_VALUE, Integer.MAX_VALUE );
		g.translate( charWidth * -firstVisibleColumn, 0 );

		visibleRows = (int)Math.ceil( (double)getHeight() / lineHeight );
		visibleColumns = (int)Math.ceil( (double)getWidth() / charWidth );
		longestVisibleRowLength = 0;

		visibleTabLists.clear();

		// (2) Extract and render tokens from the parse tree.

		try
		{
			RenderContext r = new RenderContext( g );

			for( int i = 0; i < visibleRows; i++ )
			{
				visibleTabLists.add( new Vector() );

				if( firstVisibleLine + i > model.getNewlineCount() )
					break;

				r.cursor.x = 0;
				r.cursor.y = i;
				r.charsDrawn = 0;

				int currentLineLength = model.getLineLength( firstVisibleLine + i );

				ParseTreeLocation lineStart = model.getLocationForLineNumber( firstVisibleLine + i );
				TokenStream tokenStream = lineStart.getTokenStream( true );
				BSEToken token = (BSEToken)tokenStream.nextToken();

				if( token.getType() != Token.EOF_TYPE )
				{
					// The first token might be only partly on screen.
					token.paint( r, lineStart.getOffsetInToken() );

					token = (BSEToken)tokenStream.nextToken();
					
					// Need to draw the whole line, not just up to the
					// edge of the view, otherwise horizontal scrollbar
					// gets confused because longestVisibleRowLength is
					// incorrect.
					while( r.charsDrawn < currentLineLength )
					{
						if( token.getType() == Token.EOF_TYPE )
							break;

						token.paint( r );
						token = (BSEToken)tokenStream.nextToken();
					}
				}

				if( longestVisibleRowLength < r.cursor.x )
					longestVisibleRowLength = r.cursor.x;

			}
		}
		catch( TokenStreamException e )
		{
			System.err.println( e );
			e.printStackTrace( System.err );
		}

		// (3) Draw the cursor!
		g.translate( charWidth * firstVisibleColumn, 0 );
		drawCaret( g, false );
		
		// (4) Blit to screen.
		screenGraphics.drawImage( backBuffer, 0, 0, this );
		toolkit.sync();

		// (5) Change the scrollbars on the basis of what has been rendered.
		updateScrollbars();
	}

	/** This class contains state that tokens need to render themselves.
	 * An instance of it is passed to each token when it is to be drawn.
	 * It contains little more than a Graphics context, but also keeps
	 * track of the position on screen of the drawing cursor.
	 */
	public class RenderContext
	{
		Graphics g;
		Point cursor;	// Coordinates of next char to paint, in block characters.
		int charsDrawn;

		/** Constructs a new RenderContext.
		 *
		 * @param g a graphics context, which is
		 * that of the component creating it, and must not be null.
		 */
		public RenderContext( Graphics g )
		{
			this.g = g;
			cursor = new Point( 0, 0 );
			charsDrawn = 0;
		}

		/** Draws a substring of a token's text on the screen. The substring
		* drawn runs from offset up to the first newline or the end of the
		* string. Tab characters are accounted for. The cursor of the
		* RenderContext is updated to be just after the end of the drawn text.
		*
		* @param token the token to be drawn. This is used to determine
		* the colour in which to draw, by passing it to getColourForToken.
		*
		* @param string the string, a substring of which will be drawn. This
		* will normally be the text of the token.
		*
		* @offset the offset into the string at which to begin drawing.
		*/
		public void drawString( BSEToken token, String string, int offset )
		{
			Color fgColor = getColourForToken( token );
			Color bgColor = getBackgroundForToken( token );

			int nextTabOrNewline = offset - 1;

			while( nextTabOrNewline != string.length() )
			{
				int curOffset = nextTabOrNewline + 1;

				// (1) Work out how much of the string we can print all in a row.
				int nextTab = string.indexOf( '\t', (nextTabOrNewline + 1) );
				int nextNewline = string.indexOf( '\n', (nextTabOrNewline + 1) );
				// If there are no more tabs or newlines, just draw right up to
				// the end of the string.
				nextTabOrNewline = Math.min( ((nextTab == -1) ? string.length() : nextTab),
											 ((nextNewline == -1 ) ? string.length() : nextNewline) );

				if( nextTabOrNewline == -1 )
				{
					// I think this is never executed.
					throw new RuntimeException( "This isn't redundant after all!" );
					//break;
				}

				// (2) Print it and move the cursor along.
				g.setColor( bgColor );
				g.fillRect( cursor.x * charWidth,
							(cursor.y * lineHeight),
							(nextTabOrNewline - curOffset) * charWidth,
							lineHeight );
				g.setColor( fgColor );
				g.drawString( string.substring( curOffset, nextTabOrNewline ),
					cursor.x * charWidth, (cursor.y * lineHeight) + lineMaxAscent );
				cursor.x += (nextTabOrNewline - curOffset);
				charsDrawn += (nextTabOrNewline - curOffset);

				// (3) Act on newline or tab.
				if( nextTabOrNewline == nextTab )
				{
					// Add a tab to the list for the current line.
					((Vector)visibleTabLists.get( cursor.y )).add( new Integer( cursor.x ) );
					
					g.setColor( bgColor );
					g.fillRect( cursor.x * charWidth,
								(cursor.y * lineHeight),
								(nextTabStop( cursor.x ) - cursor.x) * charWidth,
								lineHeight );
					cursor.x = nextTabStop( cursor.x );
					charsDrawn += 1;
				}

				if( nextTabOrNewline == nextNewline )
					return;
			}
		}
	}

	/** Draws the caret in EOR mode.
	 *
	 * @param g the graphics context in which to draw. If this is null, the
	 * graphics context for this component is used. If not, then the origin
	 * of the context must match that of the graphics context of this component,
	 * or the caret will be drawn in the wrong position.
	 */
	private void drawCaret( Graphics g )
	{
		if( g == null )
			g = backBuffer.getGraphics();
		
		Color oldColour = g.getColor();
		g.setColor( Color.black );
		g.setXORMode( curCaretColour );
		
		// selStart may be after selEnd, so need different variables.
		int selTop, selBottom;
		if( selStart <= selEnd )
		{
			selTop = selStart;
			selBottom = selEnd;
		}
		else
		{
			selTop = selEnd;
			selBottom = selStart;
		}

		if( selTop == selBottom )
		{
			g.setColor( Color.black );
			// Just draw a standard caret.

			int caretLine = model.getLineNumberForOffset( selTop );

			if( caretLine == -1 )
				throw new RuntimeException( "Internal error! Caret is outside document bounds." );

			if( (caretLine >= firstVisibleLine + visibleRows) ||
				(caretLine < firstVisibleLine) )
				return;

			int caretCol = getColForChar
				( selTop - model.getOffsetForLineNumber( caretLine ), caretLine );
			g.drawLine( (caretCol - firstVisibleColumn) * charWidth + 1,
						(caretLine - firstVisibleLine) * lineHeight,
						(caretCol - firstVisibleColumn) * charWidth + 1,
						(caretLine - firstVisibleLine + 1) * lineHeight - 1 );
		}
		else
		{
			int selTopLine = model.getLineNumberForOffset( selTop );
			int selBottomLine = model.getLineNumberForOffset( selBottom );

			if( selTopLine == -1 || selBottomLine == -1 )
				throw new RuntimeException( "Internal error! Selection is outside document bounds." );

			if( selTopLine >= firstVisibleLine + visibleRows )
				return;
			if( selBottomLine < firstVisibleLine )
				return;

			// No need to check selection is visible if only one line selected,
			// as previous tests will have returned if not.
			if( selTopLine == selBottomLine )
			{
				int lineStartOffset = model.getOffsetForLineNumber( selTopLine );
				int selTopCol = getColForChar
					( selTop - lineStartOffset, selTopLine );
				int selBottomCol = getColForChar
					( selBottom - lineStartOffset, selBottomLine );
				int selWidthCols = ((firstVisibleColumn + visibleColumns < selBottomCol) ?
					(firstVisibleColumn + visibleColumns) : selBottomCol) - selTopCol;
				g.fillRect( (selTopCol - firstVisibleColumn) * charWidth,
							(selTopLine - firstVisibleLine) * lineHeight,
							selWidthCols * charWidth,
							lineHeight );
			}
			else
			{
				int curLine;

				// (1) Colour the end of the line where the selection starts, if necessary.
				if( selTopLine >= firstVisibleLine )
				{
					int selTopCol = getColForChar
						( selTop - model.getOffsetForLineNumber( selTopLine ), selTopLine );

					if( selTopCol < (firstVisibleColumn + visibleColumns) )
						g.fillRect( (selTopCol - firstVisibleColumn) * charWidth,
									(selTopLine - firstVisibleLine) * lineHeight,
									(firstVisibleColumn + visibleColumns - selTopCol) * charWidth,
									lineHeight );

					curLine = selTopLine + 1;
				}
				else
					curLine = firstVisibleLine;

				// (2) Draw completely selected lines.
				while( (curLine < firstVisibleLine + visibleRows) &&
					   (curLine < selBottomLine) )
				{
					g.fillRect( 0,
								(curLine - firstVisibleLine) * lineHeight,
								visibleColumns * charWidth,
								lineHeight );
					++curLine;
				}

				// (3) Draw start of the line where the selection ends, if necessary.
				if( selBottomLine < firstVisibleLine + visibleRows )
				{
					int selBottomCol = getColForChar
						( selBottom - model.getOffsetForLineNumber( selBottomLine ), selBottomLine );

					if( selBottomCol > firstVisibleColumn )
						g.fillRect( 0,
									(selBottomLine - firstVisibleLine) * lineHeight,
									(selBottomCol - firstVisibleColumn) * charWidth,
									lineHeight );
				}
			}
		}
		
		g.setPaintMode();
		g.setColor( oldColour );
	}
	
	private void drawCaret( Graphics g, boolean swapBuffers )
	{
		drawCaret( g );
		if( swapBuffers )
			getGraphics().drawImage( backBuffer, 0, 0, this );
	}



///////////////
// Listeners //
///////////////

// ComponentListener
	public void componentResized( ComponentEvent ev )
	{
		if( backBuffer != null )
			backBuffer.flush();
		
		backBuffer = createImage( getWidth(), getHeight() );
	}
	
	public void componentShown( ComponentEvent ev ) {}
	public void componentHidden( ComponentEvent ev ) {}
	public void componentMoved( ComponentEvent ev ) {}

// FocusListener
	public void focusGained( FocusEvent ev )
	{
		drawCaret( null, false );
		curCaretColour = caretColourFocus;
		drawCaret( null, true );
	}
	
	public void focusLost( FocusEvent ev )
	{
		drawCaret( null, false );
		curCaretColour = caretColourNoFocus;
		drawCaret( null, true );
	}

// AdjustmentListener (scrollbars)
	public void adjustmentValueChanged( AdjustmentEvent ev )
	{
		if( ev.getSource() == hScrollbar )
			moveViewToPosition( ev.getValue(), firstVisibleLine );
		else if( ev.getSource() == vScrollbar )
			moveViewToPosition( firstVisibleColumn, ev.getValue() );
	}

// KeyListener
	public void keyTyped( KeyEvent ev ) {}

	// TODO: tidy this routine up, maybe with map from keystrokes to actions.
	public void keyPressed( KeyEvent ev )
	{
		boolean consume = true;

		int keyCode = ev.getKeyCode();
		
		int selTop = (selStart > selEnd) ? selEnd : selStart;
		int selBottom = (selStart > selEnd) ? selStart : selEnd;

		if( keyCode == ev.VK_BACK_SPACE )
		{
			if( selTop == selBottom)
			{
				if( selTop > 0 )
				{
					setSelection( selTop - 1, selTop - 1 );
					model.spliceText( selTop - 1, selTop, "" );
				}
			}
			else
			{
				setSelection( selTop, selTop );
				model.spliceText( selTop, selBottom, "" );
			}
		}
		else if( keyCode == ev.VK_DELETE )
		{
			if( selTop == selBottom )
			{
				if( selTop < model.getMaxOffset() )
				{
					model.spliceText( selTop, selTop + 1, "" );
				}
			}
			else
			{
				setSelection( selTop, selTop );
				model.spliceText( selTop, selBottom, "" );
			}
		}
		else if( keyCode == ev.VK_LEFT )
		{
			if( ev.isShiftDown() )
			{
				if( selEnd > 0 )
					setSelection( selStart, selEnd - 1 );
			}
			else
			{
				if( selTop == selBottom )
				{
					if( selTop > 0 )
						setSelection( selTop - 1, selTop - 1 );
				}
				else
					setSelection( selTop, selTop );
			}
		}
		else if( keyCode == ev.VK_RIGHT )
		{
			if( ev.isShiftDown() )
			{
				if( selEnd < model.getMaxOffset() )
					setSelection( selStart, selEnd + 1 );
			}
			else
			{
				if( selTop == selBottom )
				{
					if( selTop < model.getMaxOffset() )
						setSelection( selTop + 1, selTop + 1 );
				}
				else
					setSelection( selBottom, selBottom );
			}
		}
		else if( keyCode == ev.VK_UP )
		{
			int newSelEnd = getBestOffsetOnNewLine( selEnd, -1 );
			setSelection( ev.isShiftDown() ? selStart : newSelEnd, newSelEnd, false );
		}
		else if( keyCode == ev.VK_DOWN )
		{
			int newSelEnd = getBestOffsetOnNewLine( selEnd, 1 );
			setSelection( ev.isShiftDown() ? selStart : newSelEnd, newSelEnd, false );
		}
		else if( keyCode == ev.VK_PAGE_UP )
		{
			int newSelEnd = getBestOffsetOnNewLine( selEnd, -visibleRows );
			setSelection( ev.isShiftDown() ? selStart : newSelEnd, newSelEnd, false, false );
			moveViewToPosition( firstVisibleColumn, firstVisibleLine - visibleRows );
		}
		else if( keyCode == ev.VK_PAGE_DOWN )
		{
			int newSelEnd = getBestOffsetOnNewLine( selEnd, visibleRows );
			setSelection( ev.isShiftDown() ? selStart : newSelEnd, newSelEnd, false, false );
			moveViewToPosition( firstVisibleColumn, firstVisibleLine + visibleRows );
		}
		else
		{
			int keyChar = ev.getKeyChar();
			if( keyChar == ev.CHAR_UNDEFINED )
				return;
				
			if( ev.isControlDown() )
			{
				if( keyCode == ev.VK_V )
				{
					
					Transferable clipboardData =
						toolkit.getSystemClipboard().getContents( this );
					if( clipboardData != null )
						try
						{
							int pasteTop = selTop, pasteBottom = selBottom;
							setSelection( selTop, selTop );

							int charsPasted = model.paste( pasteTop, pasteBottom, clipboardData );
							
							setSelection( selTop + charsPasted, selTop + charsPasted );
						}
						catch( java.io.IOException e ) { log.log( e ); }
						catch( UnsupportedFlavorException e ) { log.log( e ); }
				}
				else if( keyCode == ev.VK_C )
				{
					BSEModel.Selection selection = model.copy( selTop, selBottom );
					toolkit.getSystemClipboard().setContents( selection, selection );
				}
				else if( keyCode == ev.VK_X )
				{
					BSEModel.Selection selection = model.cut( selTop, selBottom );
					toolkit.getSystemClipboard().setContents( selection, selection );
				}
			}
			else
			{
				// Move the caret to the beginning of the selection. This deals
				// with the case where a keypress moves the end of the file into
				// the range of the selection, which would otherwise cause the
				// selection to end up pointing beyond the end of the file.
				int spliceTop = selTop, spliceBottom = selBottom;
				setSelection( selTop, selTop );

				model.spliceText( spliceTop, spliceBottom, String.valueOf( ev.getKeyChar() ) );

				// Move the caret to the char after the one just typed.
				setSelection( selTop + 1, selTop + 1 );
			}
		}

		// Stop Tab from losing focus.
		if( consume )
			ev.consume();
	}

	public void keyReleased( KeyEvent ev ) {}

// MouseListener
	public void mouseClicked( MouseEvent ev )
	{
		if( (ev.getModifiers() & MouseEvent.BUTTON3_MASK) != 0 )
			if( popupMenu != null )
				popupMenu.show( this, ev.getPoint().x, ev.getPoint().y );
	}
	
	public void mouseEntered( MouseEvent ev ) {}
	public void mouseExited( MouseEvent ev ) {}
	public void mousePressed( MouseEvent ev )
	{
		if( this.hasFocus() )
		{
			if( (ev.getModifiers() & MouseEvent.BUTTON1_MASK) != 0 )
			{
				int clickOffset = getOffsetForOnscreenCoordinates( ev.getPoint() );

				if( ev.isShiftDown() )
					setSelection( selStart, clickOffset );
				else
					setSelection( clickOffset, clickOffset );
			}
		}
		else
			requestFocus();
	}

	public void mouseReleased( MouseEvent ev )
	{
	}

// MouseMotionListener
	public void mouseDragged( MouseEvent ev )
	{
		Point p = ev.getPoint();
		if( this.contains( p.x, p.y ) )
			if( (ev.getModifiers() & MouseEvent.BUTTON1_MASK) != 0 )
				setSelection( selStart, getOffsetForOnscreenCoordinates( p ) );

/*		int newX = firstVisibleColumn, newY = firstVisibleLine;
		if( p.x < getHCaretInset() * charWidth )
			newX = firstVisibleColumn + p.x / charWidth;
		else if( p.x > (visibleColumns - getHCaretInset()) * charWidth )
			newX = firstVisibleColumn + p.x / charWidth - visibleColumns;
		if( p.y < getVCaretInset() * lineHeight )
			newY = firstVisibleLine + p.y / lineHeight;
		else if( p.y > (visibleRows - getVCaretInset()) * lineHeight )
			newY = firstVisibleLine + p.y / lineHeight - visibleRows;
		moveViewToPosition( newX, newY );
*/	}

	public void mouseMoved( MouseEvent ev )
	{
	}

// BSEModelListener
	public void bseModelChanged( BSEModelEvent ev )
	{
		repaint();
	}



/////////////////////////////
// Useful internal methods //
/////////////////////////////

	/** Computes a list of columns in which tab characters start.
	* This is useful for converting row numbers into character offsets.
	* If the line has already been drawn on screen, then this method
	* returns the Vector calculated by the repaint() routine. Otherwise
	* it computes the result afresh.
	*
	* @return a list of columns in which tab stops occur. The result
	* may varyif the line is edited or the tab width changed between
	* invocations.
	*/
	private Vector getTabListForLine( int lineNumber )
	{
		if( lineNumber < 0 || lineNumber > model.getLineCount() )
			return null;
		
		// If tab list is already calculated, just return it.
		if( (lineNumber >= firstVisibleLine) &&
			(lineNumber < (firstVisibleLine + visibleTabLists.size())) )
			return (Vector)visibleTabLists.get( lineNumber - firstVisibleLine );
			
		Vector tabs = new Vector();
		
		ParseTreeLocation start = model.getLocationForOffset
			( model.getOffsetForLineNumber( lineNumber ) );
		ParseTreeTokenStream tokStream = (ParseTreeTokenStream)start.getTokenStream( true );
		Token tok;
		int cursor = 0;
		
		while( (tok = tokStream.nextToken()).getType() != Token.EOF_TYPE )
		{
			String string = tok.getText();
			int nextTabOrNewline = -1;
			
			while( nextTabOrNewline != string.length() )
			{
				int curOffset = nextTabOrNewline + 1;
				
				int nextTab = string.indexOf( '\t', (nextTabOrNewline + 1) );
				int nextNewline = string.indexOf( '\n', (nextTabOrNewline + 1) );		
				nextTabOrNewline = Math.min( (nextTab == -1) ? string.length() : nextTab,
											 (nextNewline == -1) ? string.length() : nextNewline );
				
				cursor += (nextTabOrNewline - curOffset);

				if( nextTabOrNewline == nextTab )
				{
					tabs.add( new Integer( cursor ) );
					cursor = nextTabStop( cursor );
				}

				if( nextTabOrNewline == nextNewline )
					return tabs;
				
			}
		}
		
		return tabs;
	}

	/** Magically converts a point in the coordinate system of this
	* view into an offset into the document.
	*
	* @param clickPoint the point to be converted.
	*
	* @return the offset corresponding to the point. If this would
	* be off the end of a line, it is clamped to the end of that line.
	* If it would be off the end of the document, it is clamped to
	* the end of the document.
	*/
	private int getOffsetForOnscreenCoordinates( Point clickPoint )
	{
		Point clickCaretPos = new Point
			( firstVisibleColumn + (clickPoint.x + (charWidth / 2)) / charWidth,
			  firstVisibleLine + clickPoint.y / lineHeight );

		int clickOffset;

		if( clickCaretPos.y > model.getNewlineCount() )
			clickOffset = model.getMaxOffset();
		else
		{
			Vector tabs = getTabListForLine( clickCaretPos.y );
			int clickCaretChar = clickCaretPos.x;
			int curTabPos;
			for( int i = 0; i < tabs.size(); i++ )
			{
				curTabPos = ((Integer)tabs.get( i )).intValue();
				if( curTabPos >= clickCaretPos.x )
					// Don't need to worry about any further tabs.
					break;

				if( nextTabStop( curTabPos ) > clickCaretPos.x )
				{
					// Caret is on a tab; decide which way to fall.
					// If caret falls to the left of the tab, then
					// the character offset is that of the tab.
					// If it falls to the right, it is one greater than that.
					clickCaretChar -= (clickCaretPos.x - curTabPos);
					if( (nextTabStop( curTabPos ) * charWidth - clickPoint.x) <
						(clickPoint.x - curTabPos * charWidth) )
					{
						clickCaretChar += 1;
					}
				}
				else
				{
					// Tab is completely to the left of the caret;
					// adjust to match.
					clickCaretChar -= (nextTabStop( curTabPos ) - curTabPos - 1);
				}
			}

			if( clickCaretChar > model.getLineLength( clickCaretPos.y ) )
				clickCaretChar = model.getLineLength( clickCaretPos.y );

			clickOffset = model.getOffsetForLineNumber( clickCaretPos.y ) + clickCaretChar;
		}

		return clickOffset;
	}
	
	/** Works out which character on a line covers a given column.
	 *
	 * @param colNum the 0-based number of the column to be covered.
	 * @param tabs a Vector containing the Integer numbers of columns at which
	 * tab characters start.
	 *
	 * @return the 0-based index of the character covering the column of the row,
	 */
	private int getCharForCol( int colNum, Vector tabs )
	{
		int charNum = colNum;
		int curTabPos;
		for( int i = 0; i < tabs.size(); i++ )
		{
			curTabPos = ((Integer)tabs.get( i )).intValue();
			if( curTabPos >= colNum )
				break;
			
			if( nextTabStop( curTabPos ) > colNum )
			{
				charNum -= (colNum - curTabPos);
			}
			else
			{
				charNum -= (nextTabStop( curTabPos ) - curTabPos - 1);
			}
		}
		
		return charNum;
	}
	
	/** Works out which character on a line covers a given column.
	 *
	 * @param colNum the 0-based number of the column to be covered.
	 * @param lineNum the 0-based number of the line of the document on which the
	 * calculation is performed.
	 *
	 * @return the 0-based index of the character covering the column of the row,
	 * or -1 if lineNum is negative or greater than the number of lines in the
	 * document, or the length of the row if colNum is greater than that length.
	 */
	private int getCharForCol( int colNum, int lineNum )
	{
		// Row might not be on screen.
		if( lineNum >= model.getLineCount() || lineNum < 0 )
			return -1;
		
		Vector tabs = getTabListForLine( lineNum );
		int charNum = getCharForCol( colNum, tabs );
		
		if( charNum > model.getLineLength( lineNum ) )
			charNum = model.getLineLength( lineNum );
		
		return charNum;
	}

	/** Works out the first on-screen column that is occupied by a given
	 * character on a line.
	 *
	 * @param charNum the 0-based index of the character on the line.
	 * @param tabs a Vector containing the Integer numbers of columns at which
	 * tab characters start.
	 *
	 * @return the 0-based index of the first column that the character
	 * occupies.
	 */
	private int getColForChar( int charNum, Vector tabs )
	{
		int colNum = charNum;
		
		for( int i = 0; i < tabs.size(); i++ )
		{
			int tabCol = ((Integer)tabs.get( i )).intValue();
			if( tabCol >= colNum )
				break;

			colNum += (nextTabStop( tabCol ) - tabCol - 1);
		}

		return colNum;
	}
	
	/** Works out the first on-screen column that is occupied by a given
	 * character on a line.
	 *
	 * @param charNum the 0-based index of the character on the line.
	 * @param lineNum the 0-based index of the line in the document.
	 *
	 * @return the 0-based index of the first column that the character
	 * occupies, or -1 if lineNum is greater than the number of lines in
	 * the model, 
	 */
	private int getColForChar( int charNum, int lineNum )
	{
		if( lineNum < 0 || lineNum >= model.getLineCount() )
			return -1;

		Vector tabs = getTabListForLine( lineNum );
		return getColForChar( charNum, tabs );
	}
	
	/** Sets the start and end offsets of the selection, and optionally
	* updates the magic caret to match the end of the selection. If the
	* offsets are the same, the caret is positioned (the caret is effectively
	* a selection of length zero). No checks are made as to the validity
	* of the offsets. If necessary, the selection is erased and redrawn at
	* its new position.
	*
	* @param newSelStart the start of the selection.
	* @param newSelEnd the end of the selection.
	*
	* @param setMagicCaret if true, the magic caret is updated to lie in
	* the column corresponding to the end of the selection.
	*
	* @param forceEndOnScreen if true, the view will be moved as necessary
	* to ensure that the end of the selection is kept on the screen.
	*/
	private void setSelection( int newSelStart, int newSelEnd,
		boolean setMagicCaret, boolean forceEndOnScreen )
	{
		if( newSelStart != selStart ||
			newSelEnd != selEnd )
		{
			drawCaret( null, false );

			selStart = newSelStart;
			selEnd = newSelEnd;
			drawCaret( null, true );
		}
		
		if( setMagicCaret )
		{
			int newLine = model.getLineNumberForOffset( newSelEnd );
			magicCaretCol = getColForChar( newSelEnd - model.getOffsetForLineNumber
				( newLine ), newLine );
		}
		
		if( forceEndOnScreen )
			ensureOffsetIsVisible( newSelEnd );
	}
	
	private void setSelection( int newSelStart, int newSelEnd,
		boolean setMagicCaret )
	{
		setSelection( newSelStart, newSelEnd, setMagicCaret, true );
	}

	/** Calculates the best new offset for the cursor given its current position,
	 * and how many lines up or down it is about to move. If the magic caret is
	 * enabled, this will be the offset corresponding to the column that is closest
	 * to the caret's current column. If not, it will be the offset corresponding
	 * to the column closest to the column of the current offset.
	 *
	 * @param curOffset the current offset, from which a new offset will be
	 * calculated if the magic caret is not enabled.
	 *
	 * @param linesToSkip how many lines up or down the caret is about to move
	 * relative to the line it is currently on.
	 *
	 * @return the most suitable new offset, which is guaranteed to be on the
	 * requested line, unless the calculated new line number is too great or too small,
	 * in which case the greatest or smallest possible offset is returned instead.
	 */
	private int getBestOffsetOnNewLine( int curOffset, int linesToSkip )
	{
		int offset;

		int curLine = model.getLineNumberForOffset( curOffset );
		int curCol = useMagicCaret ? magicCaretCol :
			getColForChar( selEnd - model.getOffsetForLineNumber( curLine ), curLine );
		int newLine = curLine + linesToSkip;

		if( newLine < 0 )
			offset = 0;
		else if( newLine >= model.getLineCount() )
			offset = model.getMaxOffset();
		else
		{
			Vector tabs = getTabListForLine( newLine );
			int newChar = getCharForCol( curCol, tabs );

			int newCol = getColForChar( newChar, tabs );
			int newCol2 = getColForChar( newChar + 1, tabs );
			if( (curCol - newCol) > (newCol2 - curCol) )
				++newChar;

			if( newChar > model.getLineLength( newLine ) )
				newChar = model.getLineLength( newLine );

			offset = newChar + model.getOffsetForLineNumber( newLine );
		}
		
		return offset;
	}

	/** Returns the column to which the caret would
	 * move if a tab was inserted at a given column.
	 */
	private int nextTabStop( int col )
	{
		return ((col / tabWidth) + 1) * tabWidth;
	}
	
	/** Returns the number of rows by which
	* the caret or selection end should be inset from
	* the edge of the view, if it is not too close to
	* the top.
	*/
	private int getVCaretInset()
	{
		return ( visibleRows >= 4 ) ? 2 : 0;
	}
		
	/** Returns the number of columns by which
	* the caret or selection end should be inset from
	* the edge of the view, if it is not too close to
	* the left.
	*/
	private int getHCaretInset()
	{
		return (visibleColumns >= 4 ) ? 2 : 0;
	}

////////////////////////////////
// Scrollbar utility routines //
////////////////////////////////

	private void updateScrollbars()
	{
		if( hScrollbar != null )
		{
			if( getMaxHScroll() <= 0 )
				hScrollbar.setEnabled( false );
			else
			{
				hScrollbar.setEnabled( true );
				hScrollbar.setValues( firstVisibleColumn, visibleColumns,
					0, visibleColumns + getMaxHScroll() );
				hScrollbar.setUnitIncrement( 1 );
				hScrollbar.setBlockIncrement( visibleColumns );
			}
		}

		if( vScrollbar != null )
		{
			if( getMaxVScroll() <= 0 )
				vScrollbar.setEnabled( false );
			else
			{
				vScrollbar.setEnabled( true );
				vScrollbar.setValues( firstVisibleLine, visibleRows,
					0, visibleRows + getMaxVScroll() );
				vScrollbar.setUnitIncrement( 1 );
				vScrollbar.setBlockIncrement( visibleRows );
			}
		}
	}
}