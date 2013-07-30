package gbmvdd.raycast;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import javax.swing.*;

public class Raycast extends JPanel implements KeyListener
{
	private int[] pix;

	private double xFOV, yFOV;
	private double angleIncrement;
	private double fovIncrement;
	private double[] viewPos = new double[3];
		// Pitch, roll, yaw.
	private double[] viewDir = new double[3];

	private class MyMouseMotionListener extends MouseAdapter implements MouseMotionListener
	{
		private Point dragStart;
		private double[] viewDirStart;

		public void mousePressed( MouseEvent e )
		{
			requestFocus();
			dragStart = e.getPoint();
			viewDirStart = (double[])viewDir.clone();
		}

		public void mouseMoved( MouseEvent e ) {}

		public void mouseDragged( MouseEvent e )
		{
			e.translatePoint( -dragStart.x, -dragStart.y );
			double xAngleOffset = xFOV * ((double)e.getX() / getWidth() );
			double yAngleOffset = yFOV * ((double)e.getY() / getHeight() );
			setViewDir( viewDirStart[0] + yAngleOffset, viewDirStart[1] - xAngleOffset, viewDirStart[2] );
		}
	}

	public Raycast( int xSize, int ySize )
	{
		super( true );
		setPreferredSize( new Dimension( xSize, ySize ) );
		setSize( xSize, ySize );
		MyMouseMotionListener mmml = new MyMouseMotionListener();
		addMouseListener( mmml );
		addMouseMotionListener( mmml );
		addKeyListener( this );

		pix = new int[xSize * ySize];
		xFOV = yFOV = Math.PI / 2;
		angleIncrement = (Math.PI / 2) / 12 ;
		fovIncrement = (Math.PI / 2) / 12 ;
	}

	public boolean isFocusTraversable() { return true; }

	public void keyPressed( KeyEvent ev )
	{
//		System.out.println( ev );

		switch( ev.getKeyCode() )
		{
			case ev.VK_NUMPAD4:
				adjustViewDir( 0, -angleIncrement, 0 );
				break;
			case ev.VK_NUMPAD6:
				adjustViewDir( 0, angleIncrement, 0 );
				break;
			case ev.VK_NUMPAD2:
				adjustViewDir( -angleIncrement, 0, 0 );
				break;
			case ev.VK_NUMPAD8:
				adjustViewDir( angleIncrement, 0, 0 );
				break;
			case ev.VK_NUMPAD7:
				adjustViewDir( 0, 0, angleIncrement );
				break;
			case ev.VK_NUMPAD9:
				adjustViewDir( 0, 0, -angleIncrement );
				break;
			case ev.VK_NUMPAD5:
				setViewDir( 0, 0, 0 );
				break;

			case ev.VK_ADD:
				if( ev.isShiftDown() )
					setYFOV( getYFOV() + fovIncrement );
				else
					setXFOV( getXFOV() + fovIncrement );
				break;
			case ev.VK_SUBTRACT:
				if( ev.isShiftDown() )
					setYFOV( getYFOV() - fovIncrement );
				else
					setXFOV( getXFOV() - fovIncrement );
				break;
		}
	}

	public void keyReleased( KeyEvent ev ) {}

	public void keyTyped( KeyEvent ev ) {}


	public void paint( Graphics g )
	{
		Rectangle rect = g.getClipBounds();

		System.out.println( rect );

		BufferedImage offScreen =
			(BufferedImage)RepaintManager.currentManager( this ).
				getOffscreenBuffer( this, rect.width, rect.height );

		System.out.println( offScreen.getWidth() + "x" + offScreen.getHeight() );

		final int w = getWidth(), h = getHeight();

		if( pix.length != w * h )
			pix = new int[w * h];

		double rayPitch, rayYaw;
			// Cartesian ray in view space.
		double vRayX, vRayY, vRayZ;
		double rolledX, rolledY, rolledZ;
		double rolledPitchedX, rolledPitchedY, rolledPitchedZ;
			// Ray in world space.
		double rayX, rayY, rayZ;

		for( int y = rect.y; y < rect.y + rect.height; y++ )
		{
			for( int x = rect.x; x < rect.width; x++ )
			{
					// Calculate the ray vector in view space.
				rayPitch = (yFOV * (0.5 - ((double)y/h)));
				rayYaw = (xFOV * (((double)x/w) - 0.5));
				vRayX = Math.sin( rayYaw ) * Math.cos( rayPitch );
				vRayY = Math.sin( rayPitch );
				vRayZ = Math.cos( rayYaw ) * Math.cos( rayPitch );

					// Now rotate it back into world space.
					// Roll, then pitch, then yaw.
					// Roll rotation (about Z):
				rolledX = Math.cos( viewDir[2] ) * vRayX - Math.sin( viewDir[2] ) * vRayY;
				rolledY = Math.sin( viewDir[2] ) * vRayX + Math.cos( viewDir[2] ) * vRayY;
				rolledZ = vRayZ;
					// Pitch rotation (about X):
				rolledPitchedX = rolledX;
				rolledPitchedY = Math.sin( viewDir[0] ) * rolledZ + Math.cos( viewDir[0] ) * rolledY;
				rolledPitchedZ = Math.cos( viewDir[0] ) * rolledZ - Math.sin( viewDir[0] ) * rolledY;
					// Yaw rotation (about Y):
				rayX = Math.cos( viewDir[1] ) * rolledPitchedX + Math.sin( viewDir[1] ) * rolledPitchedZ;
				rayY = rolledPitchedY;
				rayZ = Math.cos( viewDir[1] ) * rolledPitchedZ - Math.sin( viewDir[1] ) * rolledPitchedX;

				pix[x + (y * w)] = getRayColour( rayX, rayY, rayZ );
			}
		}

		offScreen.setRGB( 0, 0, rect.width, rect.height, pix, (rect.x + rect.y * w), w );
	}

	public int getRayColour( double x, double y, double z )
	{
		int colour = 0;
		if( x > 0 )
			colour |= 0x0000ff;
		if( y > 0 )
			colour |= 0xff0000;
		if( z > 0 )
			colour |= 0x00ff00;
		return colour;
	}



	public void setViewDir( double pitch, double yaw, double roll )
	{
		viewDir[0] = pitch;
		viewDir[1] = yaw;
		viewDir[2] = roll;
		System.out.println( "Pitch/yaw/roll: "
			+ viewDir[0] * (180/Math.PI) + "/"
			+ viewDir[1] * (180/Math.PI) + "/"
			+ viewDir[2] * (180/Math.PI) );
		repaint();
	}

	public void adjustViewDir( double pitch, double yaw, double roll )
	{
		double[] f = getViewDir();
		setViewDir( f[0] + pitch, f[1] + yaw, f[2] + roll );
	}

	public double[] getViewDir() { return (double[])viewDir.clone(); }



	public void setViewPos( double x, double y, double z )
	{
		viewPos[0] = x;
		viewPos[1] = y;
		viewPos[2] = z;
		repaint();
	}

	public void adjustViewPos( double leftRight, double upDown, double forwardBackward )
	{
		throw new RuntimeException( "Not implemented." );
	}

	public double[] getViewPos() { return (double[])viewPos.clone(); }

	public double getXFOV() { return xFOV; }
	public double getYFOV() { return yFOV; }

	public void setXFOV( double newFOV )
	{
		if( newFOV < 0.0 ) xFOV = 0.0;
		else if( newFOV > Math.PI * 2 ) xFOV = Math.PI * 2;
		else xFOV = newFOV;

		System.out.println( "xFOV: " + xFOV * (180/Math.PI) );
		repaint();
	}

	public void setYFOV( double newFOV )
	{
		if( newFOV < 0.0 ) yFOV = 0.0;
		else if( newFOV > Math.PI * 2 ) yFOV = Math.PI * 2;
		else yFOV = newFOV;

		System.out.println( "yFOV: " + yFOV * (180/Math.PI) );
		repaint();
	}



	public static void main( String[] args )
	{
		Raycast r = new Raycast( 320, 256 );
		JFrame f = new JFrame( "Raycaster" );
		f.addWindowListener( new WindowAdapter()
			{
				public void windowClosing( WindowEvent e )
				{
					System.exit( 0 );
				}
			} );
		f.getContentPane().add( r );
		f.pack();
		f.setVisible( true );
	}
}
