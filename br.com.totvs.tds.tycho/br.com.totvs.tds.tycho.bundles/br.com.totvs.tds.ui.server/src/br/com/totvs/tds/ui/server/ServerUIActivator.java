package br.com.totvs.tds.ui.server;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.services.IServiceLocator;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.TDSMessageHandler;

/**
 * The activator class controls the plug-in life cycle.
 */
public final class ServerUIActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "br.com.totvs.tds.ui.server"; //$NON-NLS-1$

	// The shared instance
	private static ServerUIActivator plugin;

	/**
	 * Returns the shared instance.
	 *
	 * @return the shared instance
	 */
	public static ServerUIActivator getDefault() {

		return plugin;
	}

	public static IStatus logStatus(int level, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, message, args);
		TDSMessageHandler.logMessage(status);

		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(IStatus status) {
		TDSMessageHandler.showMessage(status);

		getDefault().getLog().log(status);

		return status;
	}

	/**
	 * Utility method to create status.
	 *
	 * @param level
	 * @param message
	 * @param thr
	 * @return status
	 */
	public static IStatus showStatus(int level, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, message, args);
		TDSMessageHandler.logMessage(status);

		getDefault().getLog().log(status);

		return status;
	}

	/**
	 * The constructor.
	 */
	public ServerUIActivator() {

	}

	/**
	 * Inicializa a sess�o de armazenamento de configurações de diálogos.
	 *
	 * @param name
	 * @return
	 */
	public IDialogSettings getDialogSettings(final Class<? extends Dialog> class1) {

		return getDialogSettings(class1.getName());
	}

	public IDialogSettings getDialogSettings(final String className) {
		IDialogSettings settings = super.getDialogSettings();
		IDialogSettings section = settings.getSection(className);

		if (section == null) {
			section = settings.addNewSection(className);
		}

		return section;
	}

	/**
	 * Return Server Manager service.
	 *
	 * @return
	 */
	public IServerManager getServerManager() {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();

		return serviceLocator.getService(IServerManager.class);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.
	 * BundleContext)
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.
	 * BundleContext)
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;

		super.stop(context);
	}

}
