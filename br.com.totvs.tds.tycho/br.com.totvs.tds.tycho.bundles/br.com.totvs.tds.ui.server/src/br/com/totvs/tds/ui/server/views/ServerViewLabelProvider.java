package br.com.totvs.tds.ui.server.views;

import java.util.HashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Provimento de texto e imagem, para identificação de elementos.
 * 
 * @author acandido
 */
public class ServerViewLabelProvider extends LabelProvider {

	private final HashMap<String, Image> resources = new HashMap<String, Image>();

	@Override
	public Image getImage(final Object obj) {
		IItemInfo item = ((IItemInfo) obj);
		String iconName = item.getIconName();
		
		if (!resources.containsKey(iconName)) {
			ImageDescriptor imageDescriptor = ServerUIActivator.getDefault()
					.getImageDescriptor(String.format("icons/%s.png", item.getIconName())); //$NON-NLS-1$
			resources.put(iconName, imageDescriptor.createImage());
		}

		return resources.get(iconName);
	}

	@Override
	public String getText(final Object obj) {

		return ((IItemInfo) obj).getName();
	}

}