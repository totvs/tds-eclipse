package br.com.totvs.tds.ui.server.widget;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryDirNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryFileNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;

/**
 * ServerDirectoryLabelProvider .
 *
 * @author Leo Watanabe
 *
 */
public class ServerDirectoryLabelProvider extends LabelProvider {

	@Override
	public Image getImage(final Object obj) {
		Image image = null;
		//
		if (obj instanceof IServerDirectoryServerNode) {
			image = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_HOME_NAV);
		} else if (obj instanceof IServerDirectoryDirNode) {
			image = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER);
		} else if (obj instanceof IServerDirectoryFileNode) {
			image = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FILE);
		}
		//
		return image;
	}

	@Override
	public String getText(final Object obj) {
		if (obj instanceof IServerDirectoryItemNode) {
			return ((IServerDirectoryItemNode) obj).getName();
		}
		return obj.toString();
	}

}
