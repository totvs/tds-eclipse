package br.com.totvs.tds.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.ui.console.MainConsole;

/**
 * The activator class controls the plug-in life cycle.
 */
public final class TDSUIActivator extends AbstractUIPlugin  {

	// The plug-in ID
	public static final String PLUGIN_ID = "br.com.totvs.tds.ui"; //$NON-NLS-1$

	// The shared instance
	private static TDSUIActivator plugin;

	private BundleContext context;

	/**
	 * The constructor.
	 */
	public TDSUIActivator() {

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
		this.context = context;

		MainConsole.getDefault().getMainConsole(); // inicia o console TDS
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 * 
	 * @return the shared instance
	 */
	public static TDSUIActivator getDefault() {
		return plugin;
	}

	public ImageDescriptor getImageDescriptor(final String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	public BundleContext getContext() {
		return context;
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
