package br.com.totvs.tds.ui.server.providers;

import java.util.Collection;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;

public class ApplyPatchContentProvider implements IStructuredContentProvider {

	/**
	 * This implementation does nothing.
	 */
	@Override
	public void dispose() {
		// do nothing.
	}

	/**
	 * Returns the elements in the input, which must be either an array or a
	 * <code>Collection</code>.
	 */
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

	/**
	 * This implementation does nothing.
	 */
	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		TableViewer tableViewer = (TableViewer) viewer;
		Table table = tableViewer.getTable();
		if (table != null && table.getChildren() != null) {
			for (Control item : table.getChildren()) {
				List<?> objectList = (List<?>) newInput;
				Object data = item.getData("inputObject"); //$NON-NLS-1$
				if (item != null && !objectList.contains(data) && !item.isDisposed()) {
					item.dispose();
				}
			}
		}
	}

}
