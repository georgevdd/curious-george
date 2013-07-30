package gbmvdd.bse;

import javax.swing.JTree;

public class BSETreeView extends JTree
{
	public BSETreeView()
	{
		super( new ParseTreeModel( new ParseTree( new BSEToken( "Wah." ) ) ) );
	}
	
	public BSETreeView( ParseTree parseTree )
	{
		super( new ParseTreeModel( parseTree ) );
	}
	
	public BSETreeView( BSEModel bseModel )
	{
		super( new ParseTreeModel( bseModel ) );
	}

	public String convertValueToText( Object value, boolean selected, boolean expanded,
									  boolean leaf, int row, boolean hasFocus )
	{
		ParseTreeChild parseTreeChild = (ParseTreeChild)value;
		if( parseTreeChild != null ) return parseTreeChild.toString() +
			" [" + parseTreeChild.getNewlineCount() + "/" + parseTreeChild.getCharCount() + "]";
		return "";
	}
}
