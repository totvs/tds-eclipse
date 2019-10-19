package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IRpoFunction;
import br.com.totvs.tds.server.interfaces.IRpoResource;
import br.com.totvs.tds.server.interfaces.IRpoSource;
import br.com.totvs.tds.ui.server.ServerUIIcons;

public class RpoObjectLabelProvider extends LabelProvider {
	protected static final Image SOURCE_ICON = ServerUIIcons.getSource().createImage();
	protected static final Image RESOURCE_ICON = ServerUIIcons.getResource().createImage();
	protected static final Image FUNCTION_ICON = ServerUIIcons.getFunction().createImage();

	/**
	 * Construtor.
	 *
	 * @param showDetails
	 *
	 * @param detail
	 * @param functions
	 */
	public RpoObjectLabelProvider() {

	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof IRpoElement) {
			if (element instanceof IRpoSource) {
				return SOURCE_ICON;
			}
			if (element instanceof IRpoResource) {
				return RESOURCE_ICON;
			}
			if (element instanceof IRpoFunction) {
				return FUNCTION_ICON;
			}
		}

		return super.getImage(element);
	}

	@Override
	public String getText(Object element) {
		if (element instanceof IRpoElement) {
			IRpoElement rpoElement = (IRpoElement) element;
			return String.format("%s", rpoElement.getName());
		}

		return super.getText(element);
	}

}
