package br.com.totvs.tds.ui.server;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * �cones utilizados pelo adicional.
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

	private static ImageDescriptor getCamera() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/camera.png"); //$NON-NLS-1$
	}

	/**
	 * @return the clear
	 */
	private static ImageDescriptor getClear() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ETOOL_CLEAR);
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

	private static ImageDescriptor getFileObj() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/file_obj.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the form banner
	 */
	private static ImageDescriptor getFormBanner() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/form_banner.gif"); //$NON-NLS-1$
	}

	public static Image getFunctionInspector() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/function_rpo.gif").createImage(); //$NON-NLS-1$
	}

	private static ImageDescriptor getGears() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/gears.gif"); //$NON-NLS-1$
	}

	private static ImageDescriptor getIndex() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/index.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the log icon
	 */
	private static ImageDescriptor getLog() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/log.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogix() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/logix.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getMultiEnvironment() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/check.png"); //$NON-NLS-1$
	}

	/**
	 * @return the newKey
	 */
	private static ImageDescriptor getNewKey() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/newkey.gif"); //$NON-NLS-1$
	}

	public static Image getObjectInspector() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/object_rpo.png").createImage(); //$NON-NLS-1$
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
	 * @return the play
	 */
	private static ImageDescriptor getPlay() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/play.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getProtheus() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/protheus.png"); //$NON-NLS-1$
	}

	/**
	 * @return the refreshMessage
	 */
	private static ImageDescriptor getRefreshMessage() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/refresh_message.gif"); //$NON-NLS-1$
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

	/**
	 * @return the stop
	 */
	private static ImageDescriptor getStop() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_STOP);
	}

	public static ImageDescriptor getTemplate() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @return the update05Seconds
	 */
	private static ImageDescriptor getUpdate05Seconds() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/clock03-16x16.png"); //$NON-NLS-1$
	}

	/**
	 * @return the update15Seconds
	 */
	private static ImageDescriptor getUpdate15Seconds() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/clock02-16x16.png"); //$NON-NLS-1$
	}

	/**
	 * @return the update30Seconds
	 */
	private static ImageDescriptor getUpdate30Seconds() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/clock01-16x16.png"); //$NON-NLS-1$
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

	private static ImageDescriptor getWarning() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/warning.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getWarningBigDecorator() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/warning_bigdecorator.gif"); //$NON-NLS-1$
	}

	private static ImageDescriptor getWarningDecorator() {
		return ServerUIActivator.getDefault().getImageDescriptor("icons/warning_decorator.gif"); //$NON-NLS-1$
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

	private ServerUIIcons() {
		// TODO Auto-generated constructor stub
	}

}
