package gbmvdd.bse;

import java.io.*;
import antlr.*;
import java.awt.datatransfer.*;
import java.util.Vector;

import gbmvdd.util.Log;

public class BSEModel
{
	public class Selection implements Transferable, ClipboardOwner
	{
		private String beginning;
		public ParseTree middle;
		public String end;
		public Language language;
		
		public Object getTransferData( DataFlavor flavour )
			throws UnsupportedFlavorException, IOException
		{
			if( flavour.equals( BSEModel.bseDataFlavour ) )
				return this;
			else if( flavour.equals( DataFlavor.stringFlavor ) )
				return beginning;
			else if( flavour.equals( DataFlavor.plainTextFlavor ) )
				return new StringReader( beginning );
			else
				throw new UnsupportedFlavorException( flavour );
		}
		
		public DataFlavor[] getTransferDataFlavors()
		{
			return BSEModel.supportedDataFlavours;
		}
		
		public boolean isDataFlavorSupported( DataFlavor flavour )
		{
			for( int i = 0; i < BSEModel.supportedDataFlavours.length; i++ )
				if( supportedDataFlavours[i].equals( flavour ) )
					return true;
			return false;
		}
		
		public void lostOwnership( Clipboard clipboard, Transferable contents ) {}
	}
	
	public static final DataFlavor bseDataFlavour = new DataFlavor
		( Selection.class, "BSE parse tree fragment" );
	
	public static final DataFlavor[] supportedDataFlavours =
		{ bseDataFlavour,
		  DataFlavor.stringFlavor,
		  DataFlavor.plainTextFlavor };


	private Language language = null;
	private Object languageExtraData = null;
	private ParseTree parseTree = null;
	private String filename = null;
	private boolean modified = false;
	private boolean isOnDisk = false;

	private Vector listeners = new Vector();

	public BSEModel( Reader r, Language lang )
	{
		load( r, lang );
	}

	public BSEModel( Language lang )
	{
		this( new StringReader( "" ), lang );
	}
	
	public void load( Reader r, Language lang )
	{
		Reader in = new NewlineConvertingReader
			( r, NewlineConvertingReader.MODE_CRLF_TO_LF );

		language = lang;
		languageExtraData = language.createLanguageExtraData();

		WhitespaceCollapsingTokenFilter wsFilter =
			new WhitespaceCollapsingTokenFilter( language.createLexer( in ) );
		BSEParser p = language.createParser( wsFilter );
		wsFilter.hide( p.getIgnoredTokens() );
		
		parseTree = p.parseEntireStream();
		
		fireParseTreeChange();
	}
	
	public void load( Reader r )
	{
		load( r, language );
	}
	
	public void save( Writer w ) throws IOException
	{
		TokenStream tStream = parseTree.getTokenStream( true );
		try
		{
			Token tok;
			while( (tok = tStream.nextToken()).getType() != Token.EOF_TYPE )
				w.write( tok.getText() );
		}
		catch( TokenStreamException e )
		{
			throw new IOException( e.toString() );
		}
	}
	
	public void setFilename( String name )
	{
		filename = name;
	}
	
	public String getFilename()
	{
		return filename;
	}
	
	public boolean isModified() { return modified; }
	public void setModified( boolean m ) { modified = m; }
	public boolean existsOnDisk() { return isOnDisk; }
	public void setExistsOnDisk( boolean e ) { isOnDisk = e; }
	
	
	protected void fireParseTreeChange( /* TODO: add arguments */ )
	{
		BSEModelEvent event = new BSEModelEvent( this /* TODO: add arguments */ );
		for( int i = 0; i < listeners.size(); i++ )
			((BSEModelListener)listeners.get( i )).bseModelChanged( event );
	}
	

	public void addBSEModelListener( BSEModelListener l )
	{
		listeners.addElement( l );
	}

	public boolean removeBSEModelListener( BSEModelListener l )
	{
		return listeners.removeElement( l );
	}
	
	
	
	public void spliceText( int spliceStartOffset,
		int spliceEndOffset, String textToSplice )
	{
		// Really dumb reparse-complete-document update operation.
		load( new SplicingReader( new TokenStreamReader
			( parseTree.getTokenStream( true ) ),
			  spliceStartOffset, spliceEndOffset, textToSplice ) );
		
		modified = true;
		
		// TODO: replace the above with a more efficient implementation.
	}

	public Selection cut( int cutStartOffset, int cutEndOffset )
	{
		Selection result = copy( cutStartOffset, cutEndOffset );
		spliceText( cutStartOffset, cutEndOffset, "" );
		return result;
	}
	
	public Selection copy( int copyStartOffset, int copyEndOffset )
	{
		try
		{
			StringBuffer text = new StringBuffer();
			TokenStreamReader r = new TokenStreamReader( parseTree.getLocationForOffset
				( copyStartOffset ).getTokenStream( true ) );
			for( int i = copyStartOffset; i < copyEndOffset; i++ )
				text.append( (char)r.read() );
			Selection result = new Selection();
			result.beginning = text.toString();

			return result;
		}
		catch( IOException e )
		{
			System.err.println( "Failed to read from TokenStream: " + e.getMessage() );
			return null;
		}
	
		// TODO: replace the above with a more efficient implementation.
	}
	
	public int paste( int pasteStartOffset,
		int pasteEndOffset,Transferable dataToPaste )
		throws IOException, UnsupportedFlavorException
	{
		if( dataToPaste.isDataFlavorSupported( bseDataFlavour ) )
		{
			// TODO: This will need doing properly when cut & copy are
			// fully written.
			Selection selection = (Selection)
				dataToPaste.getTransferData( bseDataFlavour );
			spliceText( pasteStartOffset,
				pasteEndOffset, selection.beginning );
			return selection.beginning.length();
		}
		else if( dataToPaste.isDataFlavorSupported( DataFlavor.stringFlavor ) )
		{
			String text = (String)
				dataToPaste.getTransferData( DataFlavor.stringFlavor );
			spliceText( pasteStartOffset,
				pasteEndOffset, text );
			return text.length();
		}
		else
			throw new UnsupportedFlavorException
				( dataToPaste.getTransferDataFlavors()[0] );
	}


	public int getCharCount()
	{
		return parseTree.getCharCount();
	}
	
	public int getNewlineCount()
	{
		return parseTree.getNewlineCount();
	}
	
	public int getOffsetForLineNumber( int lineNumber )
	{
		return parseTree.getOffsetForLineNumber( lineNumber );
	}
	
	public int getLineNumberForOffset( int offset )
	{
		return parseTree.getLineNumberForOffset( offset );
	}
	
	public ParseTreeLocation getLocationForOffset( int offset )
	{
		return parseTree.getLocationForOffset( offset );
	}

	public ParseTreeLocation getLocationForLineNumber( int lineNumber )
	{
		return getLocationForOffset( getOffsetForLineNumber( lineNumber ) );
	}
	
	public int getLineLength( int lineNumber )
	{
		int curLineOffset = getOffsetForLineNumber( lineNumber );
		if( curLineOffset == -1 )
			return -1;
			
		int nextLineOffset = getOffsetForLineNumber( lineNumber + 1 );
		if( nextLineOffset == -1 )
			nextLineOffset = getCharCount();
		
		return nextLineOffset - curLineOffset - 1;
	}
	
	

	public Reader getReader()
	{
		return new TokenStreamReader( parseTree.getTokenStream( true ) );
	}

	public Reader getReader( ParseTreeLocation l )
	{
		// TODO
		return null;
	}
	
	public ParseTree getParseTree()
	{
		return parseTree;
	}
	
	public Language getLanguage()
	{
		return language;
	}
	
	public Object getLanguageExtraData()
	{
		return languageExtraData;
	}
	
	// There's always one more line than there are newlines.
	public int getLineCount()
	{
		return getNewlineCount() + 1;
	}
	
	// getCharCount() includes one virtual character for the
	// EOF character. Since the location after EOF is invalid,
	// the maximum offset is one less than that.
	public int getMaxOffset()
	{
		return getCharCount() - 1;
	}
}
