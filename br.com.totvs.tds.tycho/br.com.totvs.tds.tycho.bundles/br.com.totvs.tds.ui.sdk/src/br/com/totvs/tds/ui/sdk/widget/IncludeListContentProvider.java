package br.com.totvs.tds.ui.sdk.widget;

import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import br.com.totvs.tds.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

/**
 * Provimento de conte�do para a lista de definições.
 * 
 * @author acandido
 */
public class IncludeListContentProvider implements ITreeContentProvider {

	private static final Object[] EMPTY_ARRAY = new Object[0];

	@Override
	public void inputChanged(final Viewer v, final Object oldInput, final Object newInput) {
	}

	@Override
	public Object[] getElements(final Object inputElement) {
		if (inputElement instanceof List) {
			return ((List<?>) inputElement).toArray();
		}

		return EMPTY_ARRAY;
	}

	@Override
	public Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof IncludeDataModel) {
			String value = ((IncludeDataModel) parentElement).getFolder();
			if (value.equals(IncludeDataModel.GLOBAL)) {
				IPreferenceStore ps = SdkUIActivator.getDefault().getPreferenceStore();
				String[] globalIncludes = ps.getString(ISDKPreferenceKeys.GLOBAL_INCLUDE).split(IWrapperManager.INCLUDES_SEPARATOR); //$NON-NLS-1$
				IncludeDataModel[] data = new IncludeDataModel[globalIncludes.length];

				for (int i = 0; i < globalIncludes.length; i++) {
					String folder = globalIncludes[i];
					data[i] = new IncludeDataModel(folder);
				}

				return data;
			}
		}

		return null;
	}

	@Override
	public Object getParent(final Object element) {

		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		if (element instanceof IncludeDataModel) {
			String value = ((IncludeDataModel) element).getFolder();
			if (value.equals(IncludeDataModel.GLOBAL)) {
				return true;
			}
		}

		return false;
	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}
}
