package br.com.totvs.tds.ui.server.internal.filter;

import java.util.Map.Entry;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import br.com.totvs.tds.server.model.SourceInformation;

/**
 * A class to filter information regarding patches.
 * 
 * @author daniel.yampolschi
 *
 */
public class PatchTableFilter extends ViewerFilter {

	@Override
	public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
		if (element instanceof Entry) {
			Entry<?, ?> entry = (Entry<?, ?>) element;
			String programName = (String) entry.getKey();
			boolean show = !isSignatureFile(programName);
			return show;
		} else if (element instanceof SourceInformation) {
			SourceInformation info = (SourceInformation) element;
			boolean show = !isSignatureFile(info.getName());
			return show;
		}
		return true;
	}

	private boolean isSignatureFile(String name) {
		boolean isSignature = name.toUpperCase().endsWith(".SIGNATURE"); //$NON-NLS-1$
		return isSignature;
	}

}
