package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.model.MSRpoFunction;
import br.com.totvs.tds.server.model.MSRpoObject;
import br.com.totvs.tds.ui.server.ServerUIIcons;

public class InspectorLabelProvider implements ILabelProvider {

	Image imageFunctionInspector = ServerUIIcons.getFunctionInspector();
	Image imageObjectInspector = ServerUIIcons.getObjectInspector();

	@Override
	public void addListener(final ILabelProviderListener listener) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public Image getImage(final Object element) {
		if (element instanceof MSRpoObject) {
			return imageObjectInspector;
		} else if (element instanceof MSRpoFunction) {
			return imageFunctionInspector;
		}
		return null;
	}

	@Override
	public String getText(final Object element) {
		if (element instanceof MSRpoObject) {
			MSRpoObject program = (MSRpoObject) element;
			StringBuilder builder = new StringBuilder();
			builder.append(program.getName());
			builder.append(" - "); //$NON-NLS-1$
			builder.append(program.getType());
			builder.append(" - "); //$NON-NLS-1$
			builder.append(program.getDateAsString());
			return builder.toString();
		}
		if (element instanceof MSRpoFunction) {
			MSRpoFunction function = (MSRpoFunction) element;
			return String.format("%s (%s)", function.getName(), function.getLineNumber()); //$NON-NLS-1$
		}
		if (element != null) {
			return element.toString();
		}
		return null;
	}

	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		return false;
	}

	@Override
	public void removeListener(final ILabelProviderListener listener) {
	}

}
