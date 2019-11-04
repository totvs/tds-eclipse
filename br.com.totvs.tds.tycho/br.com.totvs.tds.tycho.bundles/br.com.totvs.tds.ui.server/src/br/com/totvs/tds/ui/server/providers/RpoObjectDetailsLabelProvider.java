package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IRpoFunction;
import br.com.totvs.tds.server.interfaces.IRpoResource;
import br.com.totvs.tds.server.interfaces.IRpoSource;
import br.com.totvs.tds.ui.server.ServerUIIcons;

public class RpoObjectDetailsLabelProvider extends LabelProvider {
	protected static final Image SOURCE_ICON = ServerUIIcons.getSource().createImage(true);
	protected static final Image RESOURCE_ICON = ServerUIIcons.getResource().createImage(true);
	protected static final Image FUNCTION_ICON = ServerUIIcons.getFunction().createImage(true);

	/**
	 * Construtor.
	 *
	 * @param showDetails
	 *
	 * @param detail
	 * @param functions
	 */
	public RpoObjectDetailsLabelProvider() {

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
			if (element instanceof IRpoFunction) {
				IRpoFunction rpoFunction = (IRpoFunction) element;
				return String.format("%s [%s:%s (%s)]", rpoFunction.getName(), rpoFunction.getProgram(),
						rpoFunction.getLineNumber(), rpoFunction.getDate());
			} else if (element instanceof IRpoSource) {
				IRpoSource rpoSource = (IRpoSource) element;
				if (rpoSource.getFunctionList().size() > 0) {
					return String.format("%s (%s) Funções: %03d", rpoSource.getName(), rpoSource.getDate().toString(),
							rpoSource.getFunctionList().size());
				}
				return String.format("%s (%s)", rpoSource.getName(), rpoSource.getDate());
			} else {
				IRpoElement rpoElement = (IRpoElement) element;
				return String.format("%s (%s)", rpoElement.getName(), rpoElement.getDate());
			}
		}

		return super.getText(element);
	}

}
