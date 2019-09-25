package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class SpecialKeysContentProvider implements IStructuredContentProvider {

	@Override
	public void dispose() {
	}

	@Override
	public Object[] getElements(Object inputElement) {
//		if (inputElement instanceof List<?>) {
//			return ((List<SimpleEntry<String, String>>) inputElement).toArray();
//		}
		return new Object[0];
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

}
