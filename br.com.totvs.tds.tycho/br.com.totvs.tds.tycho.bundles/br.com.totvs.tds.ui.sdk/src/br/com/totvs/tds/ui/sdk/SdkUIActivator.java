package br.com.totvs.tds.ui.sdk;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.ui.TDSMessageHandler;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

/**
 * The activator class controls the plug-in life cycle
 */
public class SdkUIActivator extends AbstractUIPlugin implements IPropertyChangeListener {

	// The plug-in ID
	private static final String PLUGIN_ID = "br.com.totvs.tds.ui.sdk"; //$NON-NLS-1$

	// The shared instance
	private static SdkUIActivator plugin;

	/**
	 * The constructor
	 */
	public SdkUIActivator() {

	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path.
	 * 
	 * @param path the path
	 * @return the image descriptor
	 */
	public ImageDescriptor getImageDescriptor(final String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		
		getPreferenceStore().addPropertyChangeListener(this);
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
	public static SdkUIActivator getDefault() {
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

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (event.getProperty().equals(ISDKPreferenceKeys.RESULT_SEARCH)) {
			System.out.println("SdkUIActivator.propertyChange()"); //$NON-NLS-1$
			System.out.println("RESULT_SEARCH"); //$NON-NLS-1$
		}
		
	}

}
