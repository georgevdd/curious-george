package gbmvdd.bse;

import java.awt.*;
import java.awt.event.*;

class BSEAboutBox extends Dialog
{
	Label[] labels =
	{
		new Label( "BSE $Revision: 1.1 $" ),
		new Label( "\u00A92002 George van den Driessche" ),
		new Label()
	};
	
	public BSEAboutBox( Frame parent )
	{
		super( (parent == null) ? new Frame( "" ) : parent, "About BSE" );
		
		setSize( 300, 200 );
		setResizable( false );
		setLayout( new GridLayout( labels.length, 1 ) );
		
		for( int i = 0; i < labels.length; i++ )
		{
			labels[i].setBackground( SystemColor.control );
			labels[i].setAlignment( Label.CENTER );
			add( labels[i] );
		}

		addWindowListener( new WindowAdapter()
			{
				public void windowClosing( WindowEvent ev )
				{
					setVisible( false );
				}
			} );
	}
}