package gbmvdd.bse;

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import gbmvdd.util.*;
import gbmvdd.awt.Action;

public class BSE extends Applet
{
	boolean isApplet;
	String[] args = {};
	
	Frame frame;
	BSEView view;
	BSETreeView parseTreeView;
	JSplitPane splitPane;
	Scrollbar hScrollbar, vScrollbar;
	Dialog aboutBox;
	int docIndex = 0;

	public static String defaultLangClassName = "gbmvdd.bse.java.Language";
	Properties languageMap = new Properties();

	Log log = Log.defaultLog;
	
	public BSE( String[] args )
	{
		this.args = args;
		isApplet = false;
		init();
		start();
	}
	
	public BSE()
	{
		try
		{
			String sysLAF = UIManager.getSystemLookAndFeelClassName();
			UIManager.setLookAndFeel( sysLAF );
		}
		catch( Exception e ) { System.err.println( e ); }
		isApplet = true;
	}
	
	public void init()
	{
		if( isApplet )
		{
			String logAppletName = getParameter( "logapplet" );
			if( logAppletName != null )
			{
				Applet logApplet = getAppletContext().getApplet( logAppletName );
				if( logApplet != null )
					log = Log.defaultLog = ((gbmvdd.util.AppletLog)logApplet).log;
			}
			
			log.log( "BSE $Revision: 1.9 $" );
		}
		
		String filename = null;
		
		if( args != null )
		{
			if( args.length > 0 )
				filename = args[0];
		}
			
		try
		{
			languageMap.load( getResourceAsStream
				( "/gbmvdd/bse/defaultlanguagemap.properties" ) );
		}
		catch( Exception e )
		{
			log.log( "BSE: couldn't find default language properties: " + e );
		}
		

		
		try
		{
			if( filename == null )
			{
				view = new BSEView( new BSEModel
					( new StringReader( "// BSE $Revision: 1.9 $" ),
					  Util.instantiateLanguage( defaultLangClassName ) ) );
				view.getModel().setFilename( "Document" + ++docIndex );
			}
			else
			{
				File file = new File( filename );
				String langClassName = getLangClassForFilename( filename );
				Language lang = Util.instantiateLanguage
					( langClassName, defaultLangClassName );

				if( !file.exists() )
					view = new BSEView( new BSEModel
						( new StringReader( "// BSE $Revision: 1.9 $" ), lang ) );
				else
				{
					try
					{
						Reader r = new BufferedReader
							( new InputStreamReader( new FileInputStream( file ) ) );
						view = new BSEView( new BSEModel( r, lang ) );
					}
					catch( IOException e )
					{
						throw new RuntimeException( "It was not possible to read from the file \"" + 
							file.getAbsolutePath() + ": " + e.getMessage() );
					}
					view.getModel().setExistsOnDisk( true );
				}
				view.getModel().setFilename( file.getAbsolutePath() );
			}
		}
		catch( Exception e )
		{
			throw new RuntimeException( "Exception instantiating default language class (\"" +
				defaultLangClassName + "\"): " + e.getMessage() );
		}
		
		hScrollbar = new Scrollbar( Scrollbar.HORIZONTAL );
		vScrollbar = new Scrollbar( Scrollbar.VERTICAL );
		view.setHScrollbar( hScrollbar );
		view.setVScrollbar( vScrollbar );
		
		parseTreeView = new BSETreeView( view.getModel() );
		parseTreeView.setMinimumSize( new Dimension( 0, 100 ) );
		JScrollPane scrollPane = new JScrollPane( parseTreeView );
		
		Container viewContainer = new Container();
		viewContainer.setLayout( new BorderLayout() );
		viewContainer.add( view );
		viewContainer.add( hScrollbar, BorderLayout.SOUTH );
		viewContainer.add( vScrollbar, BorderLayout.EAST );
		
		setLayout( new BorderLayout() );
		splitPane = new JSplitPane();
//		splitPane.setOneTouchExpandable( true );
		splitPane.setLeftComponent( scrollPane );
		splitPane.setRightComponent( viewContainer );
		add( splitPane );
		
				
		if( isApplet )
		{
			// Parent for dialogs.
			frame = new Frame( "" );

			PopupMenu popupMenu = new PopupMenu();
			Iterator topLevelMenus = loadMenus().iterator();
			while( topLevelMenus.hasNext() )
				popupMenu.add( (Menu)topLevelMenus.next() );
			view.setPopupMenu( popupMenu );
		}
		else
		{
			frame = new Frame( view.getModel().getFilename() );
			frame.addWindowListener( new WindowAdapter()
				{
					public void windowClosing( WindowEvent e )
					{ System.exit( 0 ); }
				} );
		
			
			MenuBar menuBar = new MenuBar();
			Iterator topLevelMenus = loadMenus().iterator();
			while( topLevelMenus.hasNext() )
				menuBar.add( (Menu)topLevelMenus.next() );

			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			frame.setLocation( screenSize.width / 8, screenSize.height / 8 );
			frame.setSize( screenSize.width * 3/4, screenSize.height * 3/4 );
			frame.setTitle( view.getModel().getFilename() );
			frame.setMenuBar( menuBar );
			frame.add( this );
			frame.setVisible( true );
		}
		
		aboutBox = new BSEAboutBox( frame );
	}
	
	public void start()
	{
		view.requestFocus();
	}
	
	private boolean doSave()
	{
		BSEModel model = view.getModel();
		String filename = model.getFilename();
		File file = new File( filename );

		Writer w = null;
		
		try
		{
			if( file.isDirectory() )
				throw new Exception( "it is a directory." );
			if( file.exists() && !file.canWrite() )
				throw new Exception( "no write access." );
			w = new BufferedWriter
				( new OutputStreamWriter
				( new FileOutputStream( file ) ) );
			model.save( w );
			model.setModified( false );
			model.setExistsOnDisk( true );
			log.log( "Saved to: " + filename );
			return true;
		}
		catch( Exception e )
		{
			log.log( "Unable to save \"" + filename + "\": " + e.getMessage() );
			return false;
		}
		finally
		{
			if( w != null )
				try
				{
					w.close();
				}
				catch( IOException e )
				{
					log.log( e );
				}
		}
	}
	
	private boolean ignoreModifiedFile()
	{
		BSEModel model = view.getModel();
		
		if( !model.isModified() )
			return true;

		switch( JOptionPane.showConfirmDialog( null,
			"Save changes to \"" +
			model.getFilename() + "\"?", "BSE",
			JOptionPane.YES_NO_CANCEL_OPTION ) )
		{
			case JOptionPane.CANCEL_OPTION:
				return false;
			case JOptionPane.YES_OPTION:
			{
				if( view.getModel().existsOnDisk() )
					return doSave();
				else
					return doSaveDialog();
			}
			case JOptionPane.NO_OPTION:
				return true;
		}
		
		return true;
	}
	
	private boolean doSaveDialog()
	{
		BSEModel model = view.getModel();
		FileDialog fd = new FileDialog
			( frame,  "Save File", FileDialog.SAVE );
		fd.setLocation( view.getX() + 20, view.getY() + 20 );
		File file = new File( model.getFilename() );
		fd.setFile( file.getName() );
		fd.setDirectory( file.getParent() );
		fd.setVisible( true );
		
		if( fd.getFile() == null )
		{
			log.log( "Save cancelled." );
			return false;
		}
			
		String filename = model.getFilename();
		file = new File( fd.getDirectory(), fd.getFile() );
		
		try
		{
			file = new File( fd.getDirectory(), fd.getFile() );
			filename = file.getCanonicalPath();
			model.setFilename( filename );
		}
		catch( Exception e )
		{
			log.log( "Unable to save \"" + filename + "\": " + e.getMessage() );
			return false;
		}

		if( doSave() )
		{
			if( !isApplet )
				frame.setTitle( view.getModel().getFilename() );
			return true;
		}
		else return false;
	}
	
	private boolean doLoadDialog()
	{
		if( !ignoreModifiedFile() )
		{
			log.log( "Open cancelled." );
			return false;
		}

		String filename = "";
		Reader r = null;
		
		try
		{
			FileDialog fd = new FileDialog
				( frame, "Open File", FileDialog.LOAD );
			fd.setLocation( view.getX() + 20, view.getY() + 20 );
			fd.setVisible( true );
			if( fd.getFile() == null )
				return false;
			File file = new File( fd.getDirectory(), fd.getFile() );
			filename = file.getCanonicalPath();
			if( !file.exists() )
				throw new Exception( "file does not exist." );
			if( file.isDirectory() )
				throw new Exception( "it is a directory." );
			if( !file.canRead() )
				throw new Exception( "no read access.") ;

			String langClassName = getLangClassForFilename( filename );
			Language lang = Util.instantiateLanguage
				( langClassName, defaultLangClassName );
			r = new BufferedReader
				( new InputStreamReader( new FileInputStream( file ) ) );
			view.setModel( new BSEModel( r, lang ) );
			parseTreeView.setModel( new ParseTreeModel( view.getModel() ) );
			
			BSEModel model = view.getModel();
			model.setFilename( filename );
			model.setExistsOnDisk( true );
			if( !isApplet )
				frame.setTitle( model.getFilename() );
				
			return true;
		}
		catch( Exception e )
		{
			log.log( "Unable to open \"" + filename + "\": " + e.getMessage() );
			return false;
		}
		finally
		{
			if( r != null )
				try
				{
					r.close();
				}
				catch( IOException e )
				{
					log.log( e );
				}
		}
	}
	
	public Action fileSave = new Action()
	{
		public String getCommand() { return "fileSave"; }
		public String getLabel() { return "Save"; }
		
		public void actionPerformed( ActionEvent ev )
		{
			if( view.getModel().existsOnDisk() )
				doSave();
			else
				doSaveDialog();
		}
	};
	
	public Action fileOpen = new Action()
	{
		public String getCommand() { return "fileOpen"; }
		public String getLabel() { return "Open"; }
		
		public void actionPerformed( ActionEvent ev )
		{
			doLoadDialog();
		}
	};
	
	public Action fileExit = new Action()
	{
		public String getCommand() { return "fileExit"; }
		public String getLabel() { return "Exit"; }
		
		public void actionPerformed( ActionEvent ev )
		{
			if( ignoreModifiedFile() )
				System.exit( 0 );
		}
	};
	
	public Action viewParseTree = new Action()
	{
		public String getCommand() { return "viewParseTree"; }
		public String getLabel() { return "Parse Tree"; }
		
		public void actionPerformed( ActionEvent ev )
		{
			
		}
	};
	
	public Action helpAbout = new Action()
	{
		public String getCommand() { return "helpAbout"; }
		public String getLabel() { return "About BSE..."; }
		
		public void actionPerformed( ActionEvent ev )
		{
			aboutBox.setLocation( view.getX() + 20, view.getY() + 20 );
			aboutBox.setVisible( true );
		}
	};
	
	
	
	public BSEView getView() { return view; }
	
	public String getLangClassForFilename( String filename )
	{
		// TODO: write this.
		int lastSlash = filename.lastIndexOf( File.separatorChar );
		int lastDot = filename.lastIndexOf( '.' );
		
		// The <= covers the case in which neither
		// slashes nor dots occur.
		if( lastDot <= lastSlash )
			return defaultLangClassName;
		else
		{
			String extension = filename.substring( lastDot + 1 );
			return getLangClassForExtension( extension );
		}
	}
	
	public String getLangClassForExtension( String extension )
	{
		return languageMap.getProperty ( languageMap.getProperty
			( extension.toLowerCase(), "Java" ), defaultLangClassName );
	}
	
	public java.util.List loadMenus()
	{
		try
		{
			return gbmvdd.util.MenuLoader.load( new InputStreamReader
				( getResourceAsStream( "/gbmvdd/bse/menus.txt" ) ),
				this );
		}
		catch( Exception e )
		{
			log.log( "Couldn't load menu hierarchy: " + e );
			return new LinkedList();
		}
	}
	
	public static InputStream getResourceAsStream( String filename )
	{
		return gbmvdd.bse.BSE.class.getResourceAsStream( filename );
	}

	public static void main( String[] args )
	{
		new BSE( args );
	}
}