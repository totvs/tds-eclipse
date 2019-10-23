package br.com.totvs.tds.ui.sdk;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Class utilit�ria de construção de �cones.
 */
public final class SdkUIIcons {

	/**
	 * Construtor.
	 */
	private SdkUIIcons() {
	}

	/**
	 * @return the item
	 */
	public static ImageDescriptor getFolder() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/folder.png"); //$NON-NLS-1$
	}

	/**
	 * @return the up
	 */
	public static ImageDescriptor getUp() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/up.png"); //$NON-NLS-1$
	}

	/**
	 * @return the down
	 */
	public static ImageDescriptor getDown() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/down.png"); //$NON-NLS-1$
	}

	/**
	 * @return the import
	 */
	public static ImageDescriptor getImport() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/import.png"); //$NON-NLS-1$
	}

	/**
	 * @return the workspace
	 */
	public static ImageDescriptor getWorkspace() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/workspace.png"); //$NON-NLS-1$
	}

	/**
	 * @return the global
	 */
	public static ImageDescriptor getGlobal() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/global.png"); //$NON-NLS-1$
	}

	/**
	 * @return the add
	 */
	public static ImageDescriptor getAdd() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_ADD);
	}

	/**
	 * @return the del
	 */
	public static ImageDescriptor getDel() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_DELETE);
	}

	public static ImageDescriptor getSearch() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/search.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogoTotvs() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/totvs.png"); //$NON-NLS-1$
	}

}
