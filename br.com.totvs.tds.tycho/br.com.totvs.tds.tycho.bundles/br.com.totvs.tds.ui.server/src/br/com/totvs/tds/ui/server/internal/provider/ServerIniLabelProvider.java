package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.forms.editor.IFormPage;

public class ServerIniLabelProvider implements ILabelProvider {

	@Override
	public void addListener(ILabelProviderListener listener) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof IFormPage) {
			return ((IFormPage) element).getTitleImage();
		} else if (element instanceof IEditorPart) {
			return ((IEditorPart) element).getTitleImage();
		}
		return null;
	}

	@Override
	public String getText(Object element) {
		if (element instanceof IFormPage) {
			return ((IFormPage) element).getTitle();
		} else if (element instanceof IEditorPart) {
			return ((IEditorPart) element).getTitle();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
	}

}
