package br.com.totvs.tds.ui.editor;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.ui.TDSMessageHandler;

/**
 * The activator class controls the plug-in life cycle
 */
public class EditorActivator extends AbstractUIPlugin {

	// The plug-in ID
	private static final String PLUGIN_ID = "br.com.totvs.tds.ui.editor.editor.advpl"; //$NON-NLS-1$

	// The shared instance
	private static EditorActivator plugin;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);

		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;

		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static EditorActivator getDefault() {
		return plugin;
	}

	/**
	 * Utility method to create status.
	 * 
	 * @param level
	 * @param message
	 * @param thr
	 * @return status
	 */
	public static IStatus showStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.showMessage(title, status);
		
		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.logMessage(title, status);
		
		getDefault().getLog().log(status);

		return status;
	}
	
	
}
