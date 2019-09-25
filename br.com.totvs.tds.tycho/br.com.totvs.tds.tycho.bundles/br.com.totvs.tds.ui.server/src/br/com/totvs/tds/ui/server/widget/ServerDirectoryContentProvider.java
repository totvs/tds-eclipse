package br.com.totvs.tds.ui.server.widget;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;

/**
 * ServerDirectoryContentProvider.
 *
 * @author Leo Watanabe
 *
 */
public class ServerDirectoryContentProvider implements ITreeContentProvider {

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof IServerDirectoryItemNode) {
			return ((IServerDirectoryItemNode) parentElement).getChildren().toArray();
		}
		return null;
	}

	@Override
	public Object[] getElements(final Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public Object getParent(final Object element) {
		if (element instanceof IServerDirectoryItemNode) {
			return ((IServerDirectoryItemNode) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		if (element instanceof IServerDirectoryItemNode) {
			return ((IServerDirectoryItemNode) element).hasChildren();
		}
		return false;
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		// Do nothing
	}

}
