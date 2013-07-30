package gbmvdd.test;

public class Outer
{
	int p;
	
	public Outer( int p )
	{
		this.p = p;
	}
	
	class Inner
	{
		int p()
		{
			return p;
		}
	}
	
	public static void main( String[] args )
	{
		Outer o = new Outer( 6 );
		Inner i = o.new Inner();
		System.out.println( i.p() );
	}
}