package br.com.totvs.tds.ui.server.internal.provider;

import java.util.Vector;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class ServerIniContentProvider implements ITreeContentProvider {

	@Override
	public void dispose() {
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		return new Object[0];
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof Vector) {
			return ((Vector) inputElement).toArray();
		}
		return new Object[0];
	}

	@Override
	public Object getParent(Object element) {
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return false;
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

}
