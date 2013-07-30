/*
 * Make sure to run antlr.Tool on the lexer.g file first!
 */

header
{
package gbmvdd.bse.tinyc;
}


options
{
	mangleLiteralPrefix = "TK_";
	language = "BSEJava";
}


class TinyCLexer extends Lexer;
options
{
	k=2;
	exportVocab=TinyC;
	charVocabulary = '\3'..'\377';
}

tokens
{
	"int"; "char"; "if"; "else"; "while";
}

{
	/**
	* TODO: These members should eventually be added to the code
	* generator as it will be common to every lexer. Or even
	* it could be added to an intermediate class between
	* CharScanner and the lexer class being generated, to
	* reuse code.
	*/
	private int currentTokenNewlineCount;
//	private int currentTokenCharCount;

	public void newline()
	{
		super.newline();
		currentTokenNewlineCount++;
	}

/*	public void consume() throws CharStreamException
	{
		super.consume();
		currentTokenCharCount++;
	}
*/

	protected Token makeToken( int type )
	{
		gbmvdd.bse.BSEToken result =
			(gbmvdd.bse.BSEToken)super.makeToken( type );
		return result;
	}

	public void resetText()
	{
		super.resetText();
		currentTokenNewlineCount = 0;
	}
}

WS	:	(' '
	|	'\t'
	|	'\n'	{ newline(); }
	|	'\r')+
	;

SL_COMMENT :
	"//"
	(~'\n')*
	;

ML_COMMENT
	:	"/*"
		(	{ LA(2)!='/' }? '*'
		|	'\n' { newline(); }
		|	~('*'|'\n')
		)*
		"*/"
	;

LPAREN
options {
	paraphrase="'('";
}
	:	'('
	;

RPAREN
options {
	paraphrase="')'";
}
	:	')'
	;

LCURLY:	'{'
	;

RCURLY:	'}'
	;

STAR:	'*'
	;

PLUS:	'+'
	;

ASSIGN
	:	'='
	;

SEMI:	';'
	;

COMMA
	:	','
	;

CHAR_LITERAL
	:	'\'' (ESC|~'\'') '\''
	;

STRING_LITERAL
	:	'"' (ESC|~('"'|'\\'|'\n'))* '"'
	;

protected
ESC	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	'0'..'3'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	DIGIT
				(
					options {
						warnWhenFollowAmbig = false;
					}
				:	DIGIT
				)?
			)?
		|	'4'..'7'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	DIGIT
			)?
		)
	;

protected
DIGIT
	:	'0'..'9'
	;

INT	:	(DIGIT)+
	;

ID
options
{
	testLiterals = true;
	paraphrase = "an identifier";
}
	:	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
	;


