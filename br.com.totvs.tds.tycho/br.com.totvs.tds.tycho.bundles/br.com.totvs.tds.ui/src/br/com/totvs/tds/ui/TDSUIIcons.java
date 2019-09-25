package br.com.totvs.tds.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Icons to be used from all UI plugins.
 * 
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
	 * @return the iconAdd
	 */
	private static ImageDescriptor getIconAdd() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_ADD);
	}

	/**
	 * @return the iconDelete
	 */
	private static ImageDescriptor getIconDelete() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_DELETE);
	}

	/**
	 * @return the iconAcCadatro
	 */
	private static ImageDescriptor getIconAxCadastro() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_DEF_VIEW);
	}

	/**
	 * @return the iconFileObj
	 */
	private static ImageDescriptor getIconFileObj() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_FILE);
	}

	/**
	 * @return the iconTotvsLogo
	 */
	private static ImageDescriptor getIconTotvsLogo() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/totvs.png"); //$NON-NLS-1$
	}

	/**
	 * @return the icon template.gif
	 */
	private static ImageDescriptor getIconTemplate() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/template.gif"); //$NON-NLS-1$
	}

	private static ImageDescriptor getIconDeleteAll() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL);
	}

	private static ImageDescriptor getIconDown() {

		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL);
	}

	private static ImageDescriptor getIconUp() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL);
	}

	public static ImageDescriptor getIconTDS() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/tds.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteOut() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/writeout.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteErr() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/writeerr.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCancel() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/cancel.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOk() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
	}

}
