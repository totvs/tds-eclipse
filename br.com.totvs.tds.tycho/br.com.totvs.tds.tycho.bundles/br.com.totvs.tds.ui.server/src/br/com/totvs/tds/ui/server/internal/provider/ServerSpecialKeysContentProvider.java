package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class ServerSpecialKeysContentProvider implements IStructuredContentProvider {

	@Override
	public void dispose() {
	}

	@Override
	public Object[] getElements(final Object inputElement) {
//		if (inputElement instanceof List<?>) {
//			return ((List<IServerIniUnknow>) inputElement).toArray();
//		}
		return new Object[0];
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
	}

}
