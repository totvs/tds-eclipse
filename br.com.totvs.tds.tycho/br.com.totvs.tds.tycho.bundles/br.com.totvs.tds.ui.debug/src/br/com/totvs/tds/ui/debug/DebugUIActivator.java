package br.com.totvs.tds.ui.debug;

import br.com.totvs.tds.ui.TDSMessageHandler;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class DebugUIActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "br.com.totvs.tds.ui.debug"; //$NON-NLS-1$

	// The shared instance
	private static DebugUIActivator plugin;

	/**
	 * The constructor
	 */
	public DebugUIActivator() {
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
	public static DebugUIActivator getDefault() {
		return plugin;
	}

	/**
	 * Utility method to create status.
	 *
	 * @param level
	 * @param message
	 * @param thr
	 *
	 * @return status
	 */

	public static IStatus showStatus(final int level, final String title, final String message) {
		return showStatus(level, title, message, TDSMessageHandler._EMPTY_ARGS);
	}

	public static IStatus showStatus(final int level, final String title, final String message, final Object... args) {
		final IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.showMessage(title, status);

		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(final int level, final String title, final String message) {
		return logStatus(level, title, message, TDSMessageHandler._EMPTY_ARGS);
	}

	public static IStatus logStatus(final int level, final String title, final String message, final Object... args) {
		final IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.logMessage(title, status);

		getDefault().getLog().log(status);

		return status;
	}

}
