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
	 * @return the environment
	 */
	private static ImageDescriptor getEnvironment() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/project/import.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the ignoreCompile
	 */
	private static ImageDescriptor getIgnoreCompile() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/folder/ignoreCompile.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the ok
	 */
	private static ImageDescriptor getOk() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
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

	/**
	 * 
	 * @return the cut
	 */
	private static ImageDescriptor getEdit() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_CUT);
	}

	/**
	 * @return the new
	 */
	private static ImageDescriptor getBrAmarelo() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_amarelo.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrAzul() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_azul.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrBranco() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_branco.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrCinza() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_cinza.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrLaranja() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_laranja.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrMarrom() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_marrom.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrVerde() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_verde.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrVermelho() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_vermelho.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrPink() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_pink.png"); //$NON-NLS-1$
	}

	private static ImageDescriptor getBrPreto() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/legends/br_preto.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSearch() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/search.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogoTotvs() {
		return SdkUIActivator.getDefault().getImageDescriptor("icons/totvs.png"); //$NON-NLS-1$
	}

}
