package br.com.totvs.tds.ui.sdk;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Classe utilitária de construção de ícones.
 */
public final class SdkUIIcons {

	/**
	 * Construtor.
	 */
	private SdkUIIcons() {
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path.
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return ResourceLocator.imageDescriptorFromBundle(SdkUIActivator.PLUGIN_ID, path).get();
	}

	/**
	 * @return the item
	 */
	public static ImageDescriptor getFolder() {
		return getImageDescriptor("icons/folder.png"); //$NON-NLS-1$
	}

	/**
	 * @return the up
	 */
	public static ImageDescriptor getUp() {
		return getImageDescriptor("icons/up.png"); //$NON-NLS-1$
	}

	/**
	 * @return the down
	 */
	public static ImageDescriptor getDown() {
		return getImageDescriptor("icons/down.png"); //$NON-NLS-1$
	}

	/**
	 * @return the import
	 */
	public static ImageDescriptor getImport() {
		return getImageDescriptor("icons/import.png"); //$NON-NLS-1$
	}

	/**
	 * @return the workspace
	 */
	public static ImageDescriptor getWorkspace() {
		return getImageDescriptor("icons/workspace.png"); //$NON-NLS-1$
	}

	/**
	 * @return the global
	 */
	public static ImageDescriptor getGlobal() {
		return getImageDescriptor("icons/global.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getAdd() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_ADD);
	}

	public static ImageDescriptor getDel() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_DELETE);
	}

	public static ImageDescriptor getSearch() {
		return getImageDescriptor("icons/search.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogoTotvs() {
		return getImageDescriptor("icons/totvs.png"); //$NON-NLS-1$
	}

}
