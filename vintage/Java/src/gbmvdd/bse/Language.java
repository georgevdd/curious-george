package gbmvdd.bse;

import java.io.Reader;
import antlr.TokenStream;
import gbmvdd.bse.BSELexer;
import gbmvdd.bse.BSEParser;

public interface Language
{
	public BSELexer createLexer( Reader in );
	public BSEParser createParser( TokenStream tStream );
	public Object createLanguageExtraData();
	public java.awt.Color getTokenColour
		( BSEToken token, Object languageExtraData );
	public java.awt.Color getBackgroundColour
		( BSEToken token, Object languageExtraData );
}