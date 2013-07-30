package gbmvdd.bezier;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

public class Bezier
{
	public static void render( Graphics g, float[] pts )
	{
		final float toleranceSquared = 1.0f;

		float ADx = pts[6] - pts[0], ADy = pts[7] - pts[1];
		float ABx = pts[2] - pts[0], ABy = pts[3] - pts[1];
		float ABdotAD = ABx * ADx + ABy * ADy;
		if( ABdotAD >= 0 )
		{
			float Abx = ABx * ABdotAD, Aby = ABy * ABdotAD;
			if( (ABx - Abx)*(ABx - Abx) + (ABy - Aby)*(ABy - Aby) <= toleranceSquared )
			{
				float CDx = pts[6] - pts[4], CDy = pts[7] - pts[5];
				float CDdotAD = CDx * ADx + CDy * ADy;
				if( CDdotAD >= 0 )
				{
					float cDx = ADx * CDdotAD, cDy = ADy * CDdotAD;
					if( (CDx - cDx)*(CDx - cDx) + (CDy - cDy)*(CDy - cDy) <= toleranceSquared )
					{
						g.drawLine( (int)pts[0], (int)pts[1], (int)pts[6], (int)pts[7] );
						return;
					}
				}
			}
		}

		float Mx = (pts[0] + pts[6])*0.125f + (pts[2] + pts[4])*0.375f,
			  My = (pts[1] + pts[7])*0.125f + (pts[3] + pts[5])*0.375f;


		float[] temp =
		{
			pts[0],
			pts[1],
			(pts[0] + pts[2])*0.5f,
			(pts[1] + pts[3])*0.5f,
			(pts[0] + pts[4])*0.25f + pts[2]*0.5f,
			(pts[1] + pts[5])*0.25f + pts[3]*0.5f,
			Mx,
			My
		};
		render( g, temp );

		float[] temp2 =
		{
			Mx,
			My,
			(pts[2] + pts[6])*0.25f + pts[4]*0.5f,
			(pts[3] + pts[7])*0.25f + pts[5]*0.5f,
			(pts[4] + pts[6])*0.5f,
			(pts[5] + pts[7])*0.5f,
			pts[6],
			pts[7]
		};
		render( g, temp2 );
	}

	public static void main( String[] args )
	{
		int xSize = 600, ySize = 400;

		Frame f = new Frame( "Bezier" );
		f.setSize( xSize, ySize );
		f.setResizable( false );
		f.setVisible( true );
		f.addWindowListener( new WindowAdapter()
			{
				public void windowClosing( WindowEvent ev )
				{
					System.exit( 0 );
				}
			} );

		float[] pts = new float[8];
		boolean[] dirs = new boolean[pts.length];
		for( int i = 0; i < pts.length; i++ )
			pts[i] = (float)Math.random() * ((i%2>0) ? ySize : xSize);
		for( int i = 0; i < dirs.length; i++ )
			dirs[i] = (Math.random() < 0.5);

		for(;;)
		{
			Graphics g = f.getGraphics();
			g.clearRect( 0, 0, xSize, ySize );
			for( int i = 0; i < pts.length - 2; i+=2 )
				g.drawLine( (int)pts[i], (int)pts[i+1],
							(int)pts[i+2], (int)pts[i+3] );
			render( g, pts );
//			f.repaint();

			for( int i = 0; i < pts.length; i++ )
			{
				pts[i] += dirs[i] ? 1 : -1;
				if( pts[i] < 0 || pts[i] >
					((i%2>0) ? ySize : xSize) )
					dirs[i] = !dirs[i];
			}
		}
	}
}