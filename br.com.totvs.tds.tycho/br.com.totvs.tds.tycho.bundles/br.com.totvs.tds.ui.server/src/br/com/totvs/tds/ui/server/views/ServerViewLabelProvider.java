package br.com.totvs.tds.ui.server.views;

import java.util.HashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Provimento de texto e imagem, para identificação de elementos.
 *
 * @author acandido
 */
public class ServerViewLabelProvider extends LabelProvider implements IStyledLabelProvider {

	private static final Styler stylerRunning = new Styler() {
		@Override
		public void applyStyles(final TextStyle textStyle) {
			textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
			textStyle.background = Display.getCurrent().getSystemColor(SWT.COLOR_GRAY);
		}
	};

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
	public StyledString getStyledText(Object element) {
		IAppServerInfo appServerInfo = (IAppServerInfo) element;

		StyledString ss = new StyledString((appServerInfo).getName());
		if (appServerInfo.isRunning()) {
			ss.append(" [executando] ", stylerRunning);
		} else if (appServerInfo.isLocalServer()) {
			ss.append(" [local] ", stylerRunning);
		}

		return ss;
	}

	@Override
	public String getText(final Object obj) {
		if (obj instanceof IAppServerInfo) {
			return getStyledText(obj).getString();
		}

		return ((IItemInfo) obj).getName();
	}

}