package br.com.totvs.tds.ui.server;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Ícones utilizados pelo adicional.
 *
 * @author acandido
 */
public final class ServerUIIcons {

	/**
	 * @return the add
	 */
	public static ImageDescriptor getAdd() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_ADD);
	}

	/**
	 * @return the bLOCKED
	 */
	public static ImageDescriptor getBlocked() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/bloqueado.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getBuildPatch() {

		return ServerUIActivator.getDefault().getImageDescriptor("icons/patch_build_wizard.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCompileKeyLock() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/compile_key_lock.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCompileKeyUnlock() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/compile_key_unlock.png"); //$NON-NLS-1$
	}

	/**
	 * @return the cONNECTED
	 */
	public static ImageDescriptor getConnected() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/connected.png"); //$NON-NLS-1$
	}

	/**
	 * @return Date Icon
	 */
	public static ImageDescriptor getDate() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/date_mode.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the del
	 */
	public static ImageDescriptor getDel() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_DELETE);
	}

	/**
	 * @return the cut
	 */
	public static ImageDescriptor getEdit() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_CUT);
	}

	public static ImageDescriptor getError() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_ERROR_TSK);
	}

	public static ImageDescriptor getFunction() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/function.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getHelp() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/help.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getInfo() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK);
	}

	public static ImageDescriptor getLinkIcon() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/linkDarkGray16x16.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogix() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/logix.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getMultiEnvironment() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/check.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getObject() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/object.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOk() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOrganizations() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/organizations.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Correction
	 */
	public static ImageDescriptor getPatchCorrection() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/patch_correction.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Package
	 */
	public static ImageDescriptor getPatchPackage() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/patch_package.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Update
	 */
	public static ImageDescriptor getPatchUpdate() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/patch_update.png"); //$NON-NLS-1$
	}

	/**
	 * @return the pgmAdvpl
	 */
	public static ImageDescriptor getPgmAdvpl() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/pgm_advpl.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getProtheus() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/protheus.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getResource() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/resource.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getRunning() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/running.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSelectedEnvironment() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/environment_current.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getServer() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/server.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSlave() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/slave.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSource() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/source.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getStopped() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/stopped.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getTemplate() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @return the user icon
	 */
	public static ImageDescriptor getUser() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/user.png"); //$NON-NLS-1$
	}

	/**
	 * @return the user login wizard icon
	 */
	public static ImageDescriptor getUser64() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/user_64.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getWarning() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_WARN_TSK);
	}

	public static ImageDescriptor getWarningBigDecorator() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/warning_bigdecorator.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the wizardGroup
	 */
	public static ImageDescriptor getWizardGroup() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/group_wizard.png"); //$NON-NLS-1$
	}

	/**
	 * @return the wizardServer
	 */
	public static ImageDescriptor getWizardServer() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/server_wizard.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getZip() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/zip.png"); //$NON-NLS-1$
	}

	private ServerUIIcons() {
		// TODO Auto-generated constructor stub
	}

}
