package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class ServerVirtualHostLabelProvider extends LabelProvider implements ITableLabelProvider {

	public Image getColumnImage(final Object obj, final int index) {
		// if (obj instanceof TypeOne) {
		// return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
		// }
		// if (obj instanceof TypeTwo) {
		// return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FILE);
		// }
		return null;
	}

	public String getColumnText(final Object obj, final int index) {
//		if (obj instanceof IServerIniHostVirtual) {
//			return ((IServerIniHostVirtual) obj).getName();
//		}
		return ""; //$NON-NLS-1$
	}

}
