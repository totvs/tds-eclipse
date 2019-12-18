package br.com.totvs.tds.ui.server.internal.provider;

import java.util.HashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.ServerUIIcons;

public class ImportServerDecoratingLabelProvider extends LabelProvider implements ILightweightLabelDecorator {

	private final HashMap<Class<?>, Image> resources = new HashMap<Class<?>, Image>();

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if (element instanceof IItemInfo) {
			IItemInfo item = (IItemInfo) element;
//			Boolean isDuplicated = (Boolean) item.getProperty(ImportTools.IS_DUPLICATED);
//			if (isDuplicated != null && isDuplicated) {
//				decoration.addOverlay(ServerUIIcons.getWarningBigDecorator(), IDecoration.TOP_RIGHT);
//			}
		}
	}

	@Override
	public Image getImage(final Object obj) {
		IItemInfo item = ((IItemInfo) obj);
		if (!resources.containsKey(item.getClass())) {
			ImageDescriptor imageDescriptor = ServerUIIcons
					.getImageDescriptor(String.format("icons/%s.png", item.getClass().getSimpleName())); //$NON-NLS-1$
			resources.put(item.getClass(), imageDescriptor.createImage(true));
		}
		return resources.get(item.getClass());
	}

	@Override
	public String getText(final Object obj) {
		return ((IItemInfo) obj).getName();
	}

}
