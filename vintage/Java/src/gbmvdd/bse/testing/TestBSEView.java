package gbmvdd.bse.testing;

import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import antlr.*;

import gbmvdd.bse.*;
import gbmvdd.util.*;

public class TestBSEView implements ActionListener, Log, BSEModelListener
{
	Frame frame;

	TextArea programTextArea;
	TextArea outputTextArea;
	Button goButton;
	JTree outputTree;
	BSEView bseView;

	public TestBSEView( String[] args ) throws Exception
	{
		String languageClassName = args[0];
		String filename = args[1];
	// (1) Set up window.
		frame = new Frame();
		frame.addWindowListener( new WindowAdapter()
			{
				public void windowClosing( WindowEvent ev )
				{
					System.exit( 0 );
				}
			} );
		frame.setSize( 600, 600 );
		frame.setLocation( 0, 0 );


	// (2) Create a view onto a document.
		Language language = Util.instantiateLanguage( languageClassName );
		File file = new File( filename );
		Reader fileReader = new InputStreamReader( new FileInputStream( file ) );
		Reader inReader = new BufferedReader( fileReader );
		bseView = new BSEView( new BSEModel( inReader, language ) );
		bseView.setLog( this );
		bseView.getModel().addBSEModelListener( this );

		Scrollbar hScroll = new Scrollbar( Scrollbar.HORIZONTAL );
		Scrollbar vScroll = new Scrollbar( Scrollbar.VERTICAL );
		
		bseView.setHAdjustable( hScroll );
		bseView.setVAdjustable( vScroll );
		
		Container viewContainer = new Container();
		viewContainer.setLayout( new BorderLayout() );
		viewContainer.add( hScroll, BorderLayout.SOUTH );
		viewContainer.add( vScroll, BorderLayout.EAST );
		viewContainer.add( bseView );


	// (3) Create input/output text areas.
		programTextArea = new TextArea();

		StringWriter sw = new StringWriter();
		Reader r = bseView.getModel().getReader();
		int ch;
		while( (ch = r.read()) != -1 )
			sw.write( ch );
		programTextArea.setText( sw.toString() );
		
		outputTextArea = new TextArea();
		outputTextArea.setEditable( false );
		
		Font font = bseView.getFont();
		if( font != null )
		{
			programTextArea.setFont( font );
			outputTextArea.setFont( font );
		}

	// (4) Create a button.
		goButton = new Button( "Go" );
		goButton.addActionListener( this );


	// (5) Create tree view.
		outputTree = new BSETreeView( bseView.getModel().getParseTree() );

		Container buttonContainer = new Container();
		buttonContainer.setLayout( new FlowLayout() );
		buttonContainer.add( goButton );

		Container topHalf = new Container();
		topHalf.setLayout( new BorderLayout() );
		topHalf.add( buttonContainer, BorderLayout.SOUTH );
		topHalf.add( programTextArea );

		Container rightHalf = new Container();
		rightHalf.setLayout( new GridLayout( 2, 1 ) );
		rightHalf.add( topHalf );
		rightHalf.add( outputTextArea );
		
		frame.setLayout( new GridLayout( 1, 3 ) );
		frame.add( outputTree );
		frame.add( rightHalf );
		frame.add( viewContainer );
		
		frame.setVisible( true );
	}

	public void actionPerformed( ActionEvent ev )
	{
		try
		{
			if( ev.getSource() == goButton )
			{
				outputTextArea.setText( "" );
			
				bseView.getModel().load( new StringReader( programTextArea.getText() ) );
			}
		}
		catch( Exception e )
		{
			log( e );
		}
	}
	
	public void bseModelChanged( BSEModelEvent e )
	{
		updateTreeView();
	}


	private void updateTreeView()
	{
		ParseTree parseTree = bseView.getModel().getParseTree();
		
		outputTree.setModel( new ParseTreeModel( parseTree ) );
		for( int row = 0; row < outputTree.getRowCount(); row++ )
			outputTree.expandRow( row );
	}

	private void dumpParseTree( TextArea target, boolean withWhitespace )
	{
		ParseTree parseTree = bseView.getModel().getParseTree();
		
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
		gbmvdd.util.Log.defaultLog.log( o );
	}

	public static void main( String[] args )
	{
		System.out.println( "TestBSEView" );

		if( args.length != 2 )
		{
			System.err.println( "Syntax: TestBSEView <languageClassName> <filename>" );
			System.exit( 1 );
		}

		try
		{
			new TestBSEView( args );
		}
		catch( Exception e )
		{
			System.err.println(" Error: " + e );
			e.printStackTrace( System.err );
			System.exit( 2 );
		}
	}
}
