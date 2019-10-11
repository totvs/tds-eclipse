package br.com.totvs.tds.sdk;

import org.eclipse.core.runtime.IStatus;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class SdkActivator implements BundleActivator {

	private static final String PLUGIN_ID = "br.com.totvs.tds.sdk";
	private static SdkActivator plugin;

	@Override
	public void start(final BundleContext context) throws Exception {
		plugin = this;
	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static SdkActivator getDefault() {
		return plugin;
	}

	public static IStatus logStatus(final int level, final String message, final Object... args) {
//		final IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, message, args);
//		TDSMessageHandler.logMessage(status);
//
//		getDefault().getLog().log(status);
		System.out.println("SdkActivator.logStatus()");
		System.out.println(message);

		return null; // status;
	}

}
