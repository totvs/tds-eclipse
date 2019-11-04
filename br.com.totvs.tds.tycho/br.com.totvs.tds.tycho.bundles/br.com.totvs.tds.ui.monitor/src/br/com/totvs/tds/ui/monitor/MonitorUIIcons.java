package br.com.totvs.tds.ui.monitor;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;

/**
 * Ícones utilizados pelo adicional.
 *
 * @author acandido
 */
public final class MonitorUIIcons {

	private MonitorUIIcons() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path.
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return ResourceLocator.imageDescriptorFromBundle(MonitorUIActivator.PLUGIN_ID, path).get();
	}

	public static ImageDescriptor getServer() {

		return getImageDescriptor("icons/server.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getBlockedServer() {

		return getImageDescriptor("icons/blocked.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getUser() {

		return getImageDescriptor("icons/user.png"); //$NON-NLS-1$
	}

}
