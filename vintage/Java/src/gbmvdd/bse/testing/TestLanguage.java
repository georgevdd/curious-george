package gbmvdd.bse.testing;

import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import antlr.*;

import gbmvdd.bse.*;
import gbmvdd.util.*;

public class TestLanguage implements ActionListener, Log
{
	Frame frame;

	TextArea programTextArea;
	TextArea outputTextArea;
	Button compileButton, cutButton, pasteButton;
	JTree outputTree;

	Language language = null;

	ParseTree parseTree = null;

	/**
	* This method returns 0-based coordinates; that is, the earliest position that
	* the caret can occupy returns coordinates (0, 0).
	*/
	private Point getCursorCoordsFromStringIndex( String str, int index )
	{
		int ycoord = 1;
		int lastNewlineIndex = -1;
		int nextNewlineIndex;
		while( ((nextNewlineIndex = str.indexOf( '\n', lastNewlineIndex + 1 )) < index)
			&& nextNewlineIndex != -1 )
		{
			lastNewlineIndex = nextNewlineIndex;
			ycoord++;
		}
		int xcoord = index - lastNewlineIndex;
		return new Point( xcoord - 1, ycoord - 1 );
	}

	public TestLanguage( String languageClassName ) throws Exception
	{
		Class languageClass;
		Object languageObject;

		try
		{
			languageClass = Class.forName( languageClassName );
		}
		catch( ClassNotFoundException e )
		{
			throw new Exception( "The class \"" + languageClassName + "\" could not be found." );
		}

		try
		{
			languageObject = languageClass.newInstance();
		}
		catch( Exception e )
		{
			throw new Exception( "The class \"" + languageClassName + "\" could not be instantiated." );
		}

		if( !(languageObject instanceof Language) )
			throw new Exception( "The class \"" + languageClassName + "\" is not a Language." );

		language = (Language)languageObject;


		frame = new Frame();
		frame.addWindowListener( new WindowAdapter()
			{
				public void windowClosing( WindowEvent ev )
				{
					System.exit( 0 );
				}
			} );
		frame.setSize( 600, 600 );
		frame.setLocation( 200, 50 );

		Font font = Font.getFont( "Courier New", null );

		programTextArea = new TextArea();
		programTextArea.setText( "  int main()\n{\n\tint value;\n\t// A comment\n\treturn value;\n}\n" );
		outputTextArea = new TextArea();
		outputTextArea.setEditable( false );
		if( font != null )
		{
			programTextArea.setFont( font );
			outputTextArea.setFont( font );
		}

		compileButton = new Button( "Compile" );
		cutButton = new Button( "Cut" );
		pasteButton = new Button( "Paste" );

		outputTree = new BSETreeView();

		Container buttonContainer = new Container();
		buttonContainer.setLayout( new FlowLayout() );
		buttonContainer.add( compileButton );
		buttonContainer.add( cutButton );
		buttonContainer.add( pasteButton );

		Container topHalf = new Container();
		topHalf.setLayout( new BorderLayout() );
		topHalf.add( buttonContainer, BorderLayout.SOUTH );
		topHalf.add( programTextArea );

		compileButton.addActionListener( this );
		cutButton.addActionListener( this );
		pasteButton.addActionListener( this );

		Container rightHalf = new Container();
		rightHalf.setLayout( new GridLayout( 2, 1 ) );
		rightHalf.add( topHalf );
		rightHalf.add( outputTextArea );

		frame.setLayout( new GridLayout( 1, 2 ) );
		frame.add( outputTree );
		frame.add( rightHalf );
		frame.setVisible( true );
	}

	public void actionPerformed( ActionEvent ev )
	{
		try
		{
			if( ev.getSource() == compileButton )
			{
				outputTextArea.setText( "" );

				int cursorIndex = programTextArea.getCaretPosition();
				Point cursorPos = getCursorCoordsFromStringIndex( programTextArea.getText(), cursorIndex );

				outputTextArea.append( "Caret at position " + cursorIndex + "\n" );
				outputTextArea.append( "Caret coordinates: " + cursorPos + "\n" );


				BSELexer lexer = language.createLexer( new StringReader( programTextArea.getText() ) );
				LogTokenStream logStream = new LogTokenStream( lexer );

				WhitespaceCollapsingTokenFilter wsFilter = new WhitespaceCollapsingTokenFilter( logStream );
				BSEParser parser = language.createParser( wsFilter );
				parser.setLog( this );
				wsFilter.hide( parser.getIgnoredTokens() );
				parseTree = parser.parseEntireStream();

				int lineOffset = parseTree.getOffsetForLineNumber( cursorPos.y );
				outputTextArea.append( "Line " + cursorPos.y + " starts at offset " + lineOffset + ".\n" );
				int cursorOffset = parseTree.getOffsetForCoordinates( cursorPos.x, cursorPos.y );
				outputTextArea.append( "Coordinates (" + cursorPos.x + ", " + cursorPos.y +
					") are at offset " + cursorOffset + ".\n" );
				ParseTreeLocation cursorLocation = parseTree.getLocationForOffset( cursorIndex );
				outputTextArea.append( cursorLocation.toString() + "\n\n" );

				outputTextArea.append( "ParseTreeWhitespaceTokenStream:\n##" );
				dumpParseTree( outputTextArea, true );
				outputTextArea.append( "##\n" );

				outputTextArea.append( "ParseTreeTokenStream:\n##" );
				dumpParseTree( outputTextArea, false );
				outputTextArea.append( "##\n" );

				TokenStream fromCursor = cursorLocation.getTokenStream( true );
				outputTextArea.append("From cursor:\n##" );
				Token tok;
				while( (tok = fromCursor.nextToken()).getType() != Token.EOF_TYPE )
					outputTextArea.append( tok.getText() );
				outputTextArea.append("##\n" );

				fromCursor = cursorLocation.getTokenStream( false );
				outputTextArea.append("From cursor (no whitespace):\n##" );
				while( (tok = fromCursor.nextToken()).getType() != Token.EOF_TYPE )
					outputTextArea.append( tok.getText() );
				outputTextArea.append("##\n" );
			}
			else if( ev.getSource() == cutButton )
			{
				int selStartIndex = programTextArea.getSelectionStart();
				int selEndIndex = programTextArea.getSelectionEnd();

				Point selStartPos = getCursorCoordsFromStringIndex( programTextArea.getText(), selStartIndex );
				Point selEndPos = getCursorCoordsFromStringIndex( programTextArea.getText(), selEndIndex );

				int selStartOffset = parseTree.getOffsetForCoordinates( selStartPos.x, selStartPos.y );
				int selEndOffset = parseTree.getOffsetForCoordinates( selEndPos.x, selEndPos.y );

				spliceText( selStartOffset, selEndOffset, "" );
			}
			else if( ev.getSource() == pasteButton )
			{
				int cursorIndex = programTextArea.getCaretPosition();
				Point cursorPos = getCursorCoordsFromStringIndex( programTextArea.getText(), cursorIndex );

				int cursorOffset = parseTree.getOffsetForCoordinates( cursorPos.x, cursorPos.y );

				outputTextArea.append( "Caret at position: " + cursorIndex + "\n" );
				outputTextArea.append( "Caret coordinates: " + cursorPos + "\n" );
				outputTextArea.append( "Offset in parse tree: " + cursorOffset + "\n" );

				spliceText( cursorOffset, cursorOffset, "Hello" );
			}
		}
		catch( Exception e )
		{
			log( e );
		}
	}

	private void spliceText( int startOffset, int endOffset, String replacement )
	{
		TokenStream astOut = parseTree.getTokenStream( true );
		Reader newText = new SplicingReader
			( new TokenStreamReader( astOut ), startOffset, endOffset, replacement );

		BSELexer lexer = language.createLexer( newText );
		lexer.setLog( this );

		WhitespaceCollapsingTokenFilter wsFilter = new WhitespaceCollapsingTokenFilter( lexer );
		BSEParser parser = language.createParser( wsFilter );
		parser.setLog( this );
		wsFilter.hide( parser.getIgnoredTokens() );
		parseTree = parser.parseEntireStream();

		programTextArea.setText( "" );
		dumpParseTree( programTextArea );
	}

	private void dumpParseTree( TextArea target, boolean withWhitespace )
	{
		// Update the tree view.
		outputTree.setModel( new ParseTreeModel( parseTree ) );
		for( int row = 0; row < outputTree.getRowCount(); row++ )
			outputTree.expandRow( row );

		// Turn the parse tree back into characters.
		TokenStream astOut = parseTree.getTokenStream( withWhitespace );
		Reader astReader = new TokenStreamReader( astOut );

		StringBuffer s = new StringBuffer();
		int nextChar;

		try
		{
			while( (nextChar = astReader.read()) != -1 )
				s.append( (char)nextChar );
		}
		catch( IOException e )
		{
			log( e );
		}
		target.append( s.toString() );
	}

	private void dumpParseTree( TextArea target )
	{
		dumpParseTree( target, true );
	}

	public void log( Object o )
	{
		if( o instanceof Exception )
		{
			log( o.toString() );
			((Exception)o).printStackTrace( System.err );
		}
		else
			outputTextArea.append( "+++ " + o + "\n" );
	}

	public static void main( String[] args )
	{
		System.out.println( "TestLanguage" );

		if( args.length != 1 )
		{
			System.err.println( "Syntax: TestLanguage <languageClassName>" );
			System.exit( 1 );
		}

		try
		{
			new TestLanguage( args[0] );
		}
		catch( Exception e )
		{
			System.err.println(" Error: " + e );
			System.exit( 2 );
		}
	}
}
