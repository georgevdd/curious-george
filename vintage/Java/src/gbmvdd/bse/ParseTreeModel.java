package gbmvdd.bse;

import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.Vector;

public class ParseTreeModel implements TreeModel, BSEModelListener
{
	ParseTree root;
	BSEModel bseModel;
	Vector listeners = new Vector();

	public ParseTreeModel( ParseTree root )
	{
		this.root = root;
		bseModel = null;
	}
	
	public ParseTreeModel( BSEModel model )
	{
		bseModel = model;
		root = bseModel.getParseTree();
		bseModel.addBSEModelListener( this );
	}
	
	public void finalize()
	{
		if( bseModel != null )
			bseModel.removeBSEModelListener( this );
	}

	public void addTreeModelListener( TreeModelListener l )
	{
		if( l != null )
			listeners.add( l );
	}

	public void bseModelChanged( BSEModelEvent ev )
	{
		root = bseModel.getParseTree();
		
		if( listeners.size() > 0 )
		{
			TreeModelEvent tmev = new TreeModelEvent
				( this, new TreePath( root ),
				null, null );
			for( int i = 0; i < listeners.size(); i++ )
			{
				TreeModelListener l = (TreeModelListener)listeners.get( i );
				l.treeStructureChanged( tmev );
			}
		}
	}

	public Object getChild( Object parent, int index )
	{
		if( index < 0 )
			return null;

		ParseTree p = (ParseTree)parent;
		ParseTreeChild child = p.getFirstParseTreeChild();

		while( index > 0 && child != null )
		{
			child = child.getNextSiblingParseTreeChild();
			index--;
		}

		return child;
	}

	public int getChildCount( Object parent )
	{
		ParseTree p = (ParseTree)parent;
		ParseTreeChild child = p.getFirstParseTreeChild();
		int childCount = 0;

		while( child != null )
		{
			child = child.getNextSiblingParseTreeChild();
			childCount++;

			// Take care of the case of circular lists.
			if( child == p.getFirstParseTreeChild() )
				break;
		}

		return childCount;
	}

	public int getIndexOfChild( Object parent, Object child )
	{
		ParseTree p = (ParseTree)parent;
		ParseTreeChild curChild = p.getFirstParseTreeChild();
		int childIndex = 0;

		while( curChild != null )
		{
			if( curChild == child )
				return childIndex;
			curChild = curChild.getNextSiblingParseTreeChild();
			childIndex++;
		}

		return -1;
	}

	public Object getRoot()
	{
		return root;
	}

	public boolean isLeaf( Object node )
	{
		return( !(node instanceof ParseTree) );
	}

	public void removeTreeModelListener( TreeModelListener l )
	{
		listeners.remove( l );
	}

	public void valueForPathChanged( TreePath path, Object newValue )
	{
		throw new RuntimeException( "You what?" );
	}
}