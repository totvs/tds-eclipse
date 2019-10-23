package br.com.totvs.tds.sdk;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

public class SdkActivator extends Plugin {

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
		final IStatus status = new Status(level, PLUGIN_ID, String.format(message, args));
//
		getDefault().getLog().log(status);

		return status;
	}

}
