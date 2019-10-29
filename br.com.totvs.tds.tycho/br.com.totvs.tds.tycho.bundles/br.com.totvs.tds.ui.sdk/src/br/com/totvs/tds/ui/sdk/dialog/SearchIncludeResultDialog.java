package br.com.totvs.tds.ui.sdk.dialog;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;

/**
 * Shows a list of items to the user with a text entry field for a string
 * pattern used to filter the list of items.
 */
public class SearchIncludeResultDialog extends FilteredItemsSelectionDialog {

	/**
	 * Filters resources using pattern and showDerived flag. It overrides
	 * ItemsFilter.
	 */
	protected class ElementFilter extends ItemsFilter {

		/**
		 * Creates new instance
		 *
		 * @param container
		 * @param showDerived flag which determine showing derived elements
		 * @param typeMask
		 */
		public ElementFilter() {
			super();
		}

		/**
		 * Checks whether the provided filter is equal to the current filter. The
		 * default implementation checks if <code>SearchPattern</code> from current
		 * filter is equal to the one from provided filter.
		 *
		 * @param filter filter to be checked, or <code>null</code>
		 * @return <code>true</code> if the given filter is equal to current filter,
		 *         <code>false</code> if given filter isn't equal to current one or if
		 *         it is <code>null</code>
		 *
		 * @see org.eclipse.ui.dialogs.SearchPattern#equalsPattern(org.eclipse.ui.dialogs.SearchPattern)
		 */
		@Override
		public boolean equalsFilter(final ItemsFilter filter) {
			final boolean result = super.equalsFilter(filter);

			return result;
		}

		@Override
		public boolean isConsistentItem(final Object item) {

			return (item instanceof String);
		}

		/**
		 * @param item Must be instance of IResource, otherwise <code>false</code> will
		 *             be returned.
		 * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#matchItem(java.lang.Object)
		 */
		@Override
		public boolean matchItem(final Object item) {
			boolean result = false;

			if (isConsistentItem(item)) {
				result = matches((String) item);
				// item.setVisible(result);
			}

			return result;
		}

	}

	private List<String> elements = Collections.emptyList();

	/**
	 * Creates a new instance of the class.
	 *
	 * @wbp.parser.constructor
	 */
	public SearchIncludeResultDialog() {
		super(null, true);

		setListLabelProvider(new LabelProvider());
		setInitialPattern("?");

	}

	@Override
	protected ItemsFilter createFilter() {

		return new ElementFilter();
	}

	@Override
	protected void fillContentProvider(final AbstractContentProvider contentProvider, final ItemsFilter itemsFilter,
			final IProgressMonitor progressMonitor) throws CoreException {
		progressMonitor.beginTask("Localizando", elements.size()); //$NON-NLS-1$

		for (final String element : elements) {
			contentProvider.add(element, itemsFilter);
			progressMonitor.worked(1);
		}

		progressMonitor.done();
	}

	@Override
	public String getElementName(final Object item) {

		return item.toString();
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Comparator getItemsComparator() {
		return (arg0, arg1) -> arg0.toString().compareToIgnoreCase(arg1.toString());
	}

	/**
	 * Trata item selecionado.
	 *
	 * @param selection
	 */
	@Override
	protected void handleSelected(final StructuredSelection selection) {
//		final ArrayList<IRpoFunction> functions = new ArrayList<IRpoFunction>();
//
//		for (final Object element : selection.toList()) {
//			if (element instanceof IRpoSource) {
//				functions.addAll(((IRpoSource) element).getFunctionList());
//			}
//		}

		super.handleSelected(selection);
	}

	@Override
	protected void restoreDialog(final IDialogSettings settings) {
		if (settings != null) {
			super.restoreDialog(settings);
		}

	}

	@Override
	protected void storeDialog(final IDialogSettings settings) {

		super.storeDialog(settings);
	}

	@Override
	protected IStatus validateItem(final Object item) {

		return Status.OK_STATUS;
	}

	@Override
	protected Control createExtendedContentArea(final Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected IDialogSettings getDialogSettings() {

		return SdkUIActivator.getDefault().getDialogSettings();
	}

	public void setElements(final List<String> resultSearch) {
		elements = resultSearch;
	}

}
