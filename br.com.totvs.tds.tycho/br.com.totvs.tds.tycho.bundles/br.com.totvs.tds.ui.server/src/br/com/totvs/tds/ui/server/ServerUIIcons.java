package br.com.totvs.tds.ui.server;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;
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
		return getImageDescriptor("icons/bloqueado.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getBuildPatch() {

		return getImageDescriptor("icons/patch_build_wizard.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCompileKeyLock() {
		return getImageDescriptor("icons/compile_key_lock.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCompileKeyUnlock() {
		return getImageDescriptor("icons/compile_key_unlock.png"); //$NON-NLS-1$
	}

	/**
	 * @return the cONNECTED
	 */
	public static ImageDescriptor getConnected() {
		return getImageDescriptor("icons/connected.png"); //$NON-NLS-1$
	}

	/**
	 * @return Date Icon
	 */
	public static ImageDescriptor getDate() {
		return getImageDescriptor("icons/date_mode.gif"); //$NON-NLS-1$
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
		return getImageDescriptor("icons/function.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getHelp() {
		return getImageDescriptor("icons/help.png"); //$NON-NLS-1$
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path.
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return ResourceLocator.imageDescriptorFromBundle(ServerUIActivator.PLUGIN_ID, path).get();
	}

	public static ImageDescriptor getInfo() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK);
	}

	public static ImageDescriptor getLinkIcon() {
		return getImageDescriptor("icons/linkDarkGray16x16.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getLogix() {
		return getImageDescriptor("icons/logix.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getMultiEnvironment() {
		return getImageDescriptor("icons/check.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getObject() {
		return getImageDescriptor("icons/object.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOk() {
		return getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOrganizations() {
		return getImageDescriptor("icons/organizations.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Correction
	 */
	public static ImageDescriptor getPatchCorrection() {
		return getImageDescriptor("icons/patch_correction.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Package
	 */
	public static ImageDescriptor getPatchPackage() {
		return getImageDescriptor("icons/patch_package.png"); //$NON-NLS-1$
	}

	/**
	 * @return Patch Update
	 */
	public static ImageDescriptor getPatchUpdate() {
		return getImageDescriptor("icons/patch_update.png"); //$NON-NLS-1$
	}

	/**
	 * @return the pgmAdvpl
	 */
	public static ImageDescriptor getPgmAdvpl() {
		return getImageDescriptor("icons/pgm_advpl.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getProtheus() {
		return getImageDescriptor("icons/protheus.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getResource() {
		return getImageDescriptor("icons/resource.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getRunning() {
		return getImageDescriptor("icons/running.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSelectedEnvironment() {
		return getImageDescriptor("icons/environment_current.gif"); //$NON-NLS-1$
	}

	public static ImageDescriptor getServer() {
		return getImageDescriptor("icons/server.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSlave() {
		return getImageDescriptor("icons/slave.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getSource() {
		return getImageDescriptor("icons/source.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getStopped() {
		return getImageDescriptor("icons/stopped.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getTemplate() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @return the user icon
	 */
	public static ImageDescriptor getUser() {
		return getImageDescriptor("icons/user.png"); //$NON-NLS-1$
	}

	/**
	 * @return the user login wizard icon
	 */
	public static ImageDescriptor getUser64() {
		return getImageDescriptor("icons/user_64.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getWarning() {
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_WARN_TSK);
	}

	public static ImageDescriptor getWarningBigDecorator() {
		return getImageDescriptor("icons/warning_bigdecorator.gif"); //$NON-NLS-1$
	}

	/**
	 * @return the wizardGroup
	 */
	public static ImageDescriptor getWizardGroup() {
		return getImageDescriptor("icons/group_wizard.png"); //$NON-NLS-1$
	}

	/**
	 * @return the wizardServer
	 */
	public static ImageDescriptor getWizardServer() {
		return getImageDescriptor("icons/server_wizard.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getZip() {
		return getImageDescriptor("icons/zip.png"); //$NON-NLS-1$
	}

	private ServerUIIcons() {
		// TODO Auto-generated constructor stub
	}

}
