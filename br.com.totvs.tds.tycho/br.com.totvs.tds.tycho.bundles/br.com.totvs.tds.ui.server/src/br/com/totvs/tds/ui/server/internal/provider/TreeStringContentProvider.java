package br.com.totvs.tds.ui.server.internal.provider;

import java.util.Collection;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Classe TreeStringContentProvider.
 *
 * @author leo.watanabe
 *
 */
public class TreeStringContentProvider implements ITreeContentProvider {

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public Object[] getChildren(final Object parentElement) {
		// Do nothing because this tree must not have children
		return null;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Object[] getElements(final Object inputElement) {
		if (inputElement instanceof Object[]) {
			return (Object[]) inputElement;
		}
		if (inputElement instanceof Collection) {
			return ((Collection) inputElement).toArray();
		}
		return new Object[0];
	}

	@Override
	public Object getParent(final Object element) {
		// Do nothing
		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		return false;
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		// Do nothing
	}

}
