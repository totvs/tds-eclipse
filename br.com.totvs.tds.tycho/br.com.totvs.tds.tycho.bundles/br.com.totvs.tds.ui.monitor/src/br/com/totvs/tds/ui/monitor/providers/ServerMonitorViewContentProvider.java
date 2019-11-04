package br.com.totvs.tds.ui.monitor.providers;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

/**
 * Content provider da view "Servidor Monitores".
 *
 * @author Matheus.Sales
 *
 */
public class ServerMonitorViewContentProvider implements ITreeContentProvider {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String VIEW_ID = "br.com.totvs.tds.ui.monitor.serverMonitorView"; //$NON-NLS-1$

	private static final Object[] EMPTY_ARRAY = new Object[0];

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object[] getElements(final Object inputElement) {

		return ((Map<String, IServerMonitor>) inputElement).values().toArray();
	}

	@Override
	public Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof IServerMonitor) {
			final IServerMonitor itemMonitor = (IServerMonitor) parentElement;
			final ArrayList<IUserMonitor> children = new ArrayList<>();

			for (final IUserMonitor users : itemMonitor.getChildren()) {
				children.add(users);
			}

			return children.toArray();
		}

		return EMPTY_ARRAY;
	}

	@Override
	public Object getParent(final Object element) {
		if (element instanceof IUserMonitor) {
			return ((IUserMonitor) element).getParent();
		}

		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		if (element instanceof IServerMonitor) {
			final IServerMonitor itemMonitor = (IServerMonitor) element;

			return !itemMonitor.getChildren().isEmpty();
		}

		return false;
	}

}
