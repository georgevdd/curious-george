package gbmvdd.bse;

public class Util
{
	public static Language instantiateLanguage( String languageClassName ) throws Exception
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

		return (Language)languageObject;
	}
	
	public static Language instantiateLanguage
		( String languageClassName, String defaultClassName ) throws Exception
	{
		try
		{
			return instantiateLanguage( languageClassName );
		}
		catch( Exception e )
		{
			return instantiateLanguage( defaultClassName );
		}
	}
}