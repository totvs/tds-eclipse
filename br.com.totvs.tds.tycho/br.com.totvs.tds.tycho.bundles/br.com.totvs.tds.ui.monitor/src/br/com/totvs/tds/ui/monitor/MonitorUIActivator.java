package br.com.totvs.tds.ui.monitor;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.ui.TDSMessageHandler;

/**
 * The activator class controls the plug-in life cycle
 */
public class MonitorUIActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "br.com.totvs.tds.ui.monitor"; //$NON-NLS-1$

	// The shared instance
	private static MonitorUIActivator plugin;

	/**
	 * The constructor
	 */
	public MonitorUIActivator() {
	}

	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static MonitorUIActivator getDefault() {
		return plugin;
	}

	public static IStatus logStatus(final int level, final String message, final Object... args) {
		final IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, message, args);
		TDSMessageHandler.logMessage(status);

		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(final IStatus status) {
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
	public static IStatus showStatus(final int level, final String message, final Object... args) {
		final IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, message, args);
		TDSMessageHandler.logMessage(status);

		getDefault().getLog().log(status);

		return status;
	}

	/**
	 * Inicializa a sessão de armazenamento de configurações de diálogos.
	 *
	 * @param name
	 * @return
	 */
	public IDialogSettings getDialogSettings(final Class<? extends Dialog> class1) {

		return getDialogSettings(class1.getName());
	}

	public IDialogSettings getDialogSettings(final String className) {
		final IDialogSettings settings = super.getDialogSettings();
		IDialogSettings section = settings.getSection(className);

		if (section == null) {
			section = settings.addNewSection(className);
		}

		return section;
	}
}
