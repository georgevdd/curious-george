package gbmvdd.bse.testing;

import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import antlr.*;

import gbmvdd.bse.*;
import gbmvdd.util.*;

public class TestTokenBuffer extends Log implements ActionListener
{
	Frame frame;

	TextArea programTextArea;
	TextArea outputTextArea;
	TextField numberField;
	Button goButton, ltButton, consumeButton;
	JTree outputTree;
	
	Language language = null;

	ParseTree parseTree = null;
	ParseTreeTokenBuffer tokBuf = null;

	public TestTokenBuffer( String languageClassName ) throws Exception
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

		Font font = new Font( "Monospaced", Font.PLAIN, 12 );

		programTextArea = new TextArea();
		programTextArea.setText( "  int main()\n{\n\tint value;\n\t// A comment\n\treturn value;\n}\n" );
		outputTextArea = new TextArea();
		outputTextArea.setEditable( false );
		if( font != null )
		{
			programTextArea.setFont( font );
			outputTextArea.setFont( font );
		}

		goButton = new Button( "Build" );
		ltButton = new Button( "LT" );
		consumeButton = new Button( "Consume" );
		numberField = new TextField( "1" );

		outputTree = new BSETreeView();

		Container buttonContainer = new Container();
		buttonContainer.setLayout( new FlowLayout() );
		buttonContainer.add( goButton );
		buttonContainer.add( numberField );
		buttonContainer.add( ltButton );
		buttonContainer.add( consumeButton );

		Container topHalf = new Container();
		topHalf.setLayout( new BorderLayout() );
		topHalf.add( buttonContainer, BorderLayout.SOUTH );
		topHalf.add( programTextArea );

		goButton.addActionListener( this );
		ltButton.addActionListener( this );
		consumeButton.addActionListener( this );

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
			if( ev.getSource() == goButton )
			{
				outputTextArea.setText( "" );

			// 1) Build the parse tree.
				BSELexer lexer = language.createLexer( new StringReader( programTextArea.getText() ) );
				LogTokenStream logStream = new LogTokenStream( lexer );

				WhitespaceCollapsingTokenFilter wsFilter = new WhitespaceCollapsingTokenFilter( logStream );
				BSEParser parser = language.createParser( wsFilter );
				parser.setLog( this );
				wsFilter.hide( parser.getIgnoredTokens() );
				parseTree = parser.parseEntireStream();

			// 2) Output parse tree and update view.
				dumpParseTree( outputTextArea );

			// 3) Reset buffers
				tokBuf = new ParseTreeTokenBuffer( parseTree );
				outputTextArea.append( tokBuf.toString() );
			}
			else if( ev.getSource() == ltButton )
			{
				int i = getNumber();
				log( "LT(" + i + ")" );
				Token tok = tokBuf.LT( i );
				outputTextArea.append( tokBuf.toString() + "\n"
				   +"LT(" + i + ") = " + tok.toString() );
			}
			else if( ev.getSource() == consumeButton )
			{
				log( "Consume" );
				tokBuf.consume();
				outputTextArea.append( tokBuf.toString() );
			}
		}
		catch( Exception e )
		{
			log( e );
		}
	}

	private int getNumber() throws NumberFormatException
	{
		return Integer.parseInt( numberField.getText() );
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

	private void updateTreeView()
	{
		outputTree.setModel( new ParseTreeModel( parseTree ) );
		for( int row = 0; row < outputTree.getRowCount(); row++ )
			outputTree.expandRow( row );
	}

	private void dumpParseTree( TextArea target, boolean withWhitespace )
	{
		updateTreeView();

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
		outputTextArea.append( "+++ " + o + "\n" );
	}

	public static void main( String[] args )
	{
		System.out.println( "TestTokenBuffer" );

/*		String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
		for( int i = 0; i < fonts.length; i++ )
			 System.out.println( fonts[i] );
*/
		if( args.length != 1 )
		{
			System.err.println( "Syntax: TestTokenBuffer <languageClassName>" );
			System.exit( 1 );
		}

		try
		{
			new TestTokenBuffer( args[0] );
		}
		catch( Exception e )
		{
			System.err.println(" Error: " + e );
			System.exit( 2 );
		}
	}
}
