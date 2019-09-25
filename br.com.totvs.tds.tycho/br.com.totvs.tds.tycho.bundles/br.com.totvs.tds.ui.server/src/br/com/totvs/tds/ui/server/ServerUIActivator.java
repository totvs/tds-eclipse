package br.com.totvs.tds.ui.server;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.lsp4e.LanguageServerPlugin;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.json.JSONObject;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.ui.TDSMessageHandler;

/**
 * The activator class controls the plug-in life cycle.
 */
@SuppressWarnings("restriction")
public final class ServerUIActivator extends AbstractUIPlugin implements ILogListener {

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

	public static IStatus logStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.logMessage(title, status);

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
	public static IStatus showStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.showMessage(title, status);

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
	public void logging(final IStatus status, final String plugin) {
		if (plugin.equals(LanguageServerPlugin.PLUGIN_ID)) {
			String msg = status.getMessage();
			int nPos = msg.indexOf("\n");
			if (nPos > -1) {
				JSONObject json = new JSONObject(msg.substring(nPos + 1));
				if (json.opt("result") != null) { // ignorar mensages do tipo
					return;
				}
				String method = "";

				try {
					method = (String) json.get("method");
				} catch (Exception e) {
					System.out.println("ServerUIActivator.logging()");
					System.out.println(method);
				}
				if (method.isEmpty()) {
					try {
						JSONObject error = (JSONObject) json.get("error");
						int code = error.getInt("code");
						String message = error.getString("message");
						logStatus(IStatus.ERROR, "LS", "[%d] %s", code, message.replace("\n", "\n\t"));
					} catch (Exception e) {
						System.out.println("ServerUIActivator.logging()");
						System.out.println(method);
					}
				} else if (method.equals("window/logMessage")) {
					JSONObject params = (JSONObject) json.get("params");
					msg = params.getString("message");

					String regex = "\\[(.*)\\](.*)";
					Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
					Matcher matcher = pattern.matcher(msg);
					int type = IStatus.INFO;
					int i = 1;
					String finalMsg = "";

					while (matcher.find()) {
						String group = matcher.group(i);
						if (i == 1) {
							if (group.startsWith("Log")) {
								type = IStatus.INFO;
							}
						} else {
							finalMsg += group;
						}
						i++;
					}

					logStatus(type, "LS", finalMsg.isEmpty() ? msg : finalMsg);
				}
			}
		} else {
			TDSMessageHandler.logMessage(plugin, status);
		}
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
		LanguageServerPlugin.getDefault().getLog().addLogListener(this);
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
