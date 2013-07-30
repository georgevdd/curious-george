package gbmvdd.awt;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class Action implements ActionListener
{
	public class MenuItem extends java.awt.MenuItem
	{
		public MenuItem()
		{
			menuItems.add( this );
			addActionListener( Action.this );
			updateUI();
		}
		
		public boolean isEnabled() { return Action.this.enabled; }

/*		{
			setLabel( Action.this.getLabel() );
			this.setEnabled( Action.this.isEnabled() );
			setActionCommand( Action.this.getCommand() );
			addActionListener( Action.this );
		}
*/

		private void updateUI()
		{		
			setLabel( Action.this.getLabel() );
			this.setEnabled( Action.this.isEnabled() );
			setActionCommand( Action.this.getCommand() );
		}
	}
	
	private boolean enabled = true;
	private java.util.List menuItems = new LinkedList();

	public String getLabel() { return "<unnamed action>"; }
	public String getCommand() { return "UnnamedAction"; }
	
	public void actionPerformed( ActionEvent ev )
	{
		setEnabled( !enabled );
		System.out.println( "aP: " + getCommand() );
	}
	
	public boolean isEnabled() { return enabled; }
	
	public void setEnabled( boolean enabled )
	{
		this.enabled = enabled;
		updateUIs();
	}
	
	public java.util.List getMenuItems() { return menuItems; }
	
/*	public static void main( String[] args )
	{
		Action a = new Action();
		
		Frame f = new Frame( "Action" );
		MenuBar mb = new MenuBar();
		Menu m = new Menu( "Test" );
		m.add( a.getMenuItem() );
		m.add( a.getMenuItem() );
		mb.add( m );
		f.setMenuBar( mb );
		
//		f.addActionListener( new javax.swing.BasicDesktopPaneUI.CloseAction() );
		f.setVisible( true );
	}
*/

	private void updateUIs()
	{
		Iterator i = menuItems.listIterator();
		while( i.hasNext() )
		{
			((MenuItem)(i.next())).updateUI();
		}
	}


}
