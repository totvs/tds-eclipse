package br.com.totvs.tds.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;

/**
 * Icons to be used from all UI plugins.
 *
 * @author acandido
 * @author daniel.yampolschi
 *
 */
public final class TDSUIIcons {

	/**
	 * Construtor.
	 */
	private TDSUIIcons() {
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path.
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return ResourceLocator.imageDescriptorFromBundle(TDSUIActivator.PLUGIN_ID, path).get();
	}

	public static ImageDescriptor getIconTDS() {
		return getImageDescriptor("icons/tds.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteOut() {
		return getImageDescriptor("icons/writeout.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteErr() {
		return getImageDescriptor("icons/writeerr.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCancel() {
		return getImageDescriptor("icons/cancel.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOk() {
		return getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getRight() {
		return getImageDescriptor("icons/right.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getUp() {
		return getImageDescriptor("icons/up.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getDoubleLeft() {
		return getImageDescriptor("icons/double_left.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getDown() {
		return getImageDescriptor("icons/down.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLeft() {
		return getImageDescriptor("icons/left.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getDoubleDirection() {
		return getImageDescriptor("icons/double_direction.gif"); //$NON-NLS-1$
	}

}
