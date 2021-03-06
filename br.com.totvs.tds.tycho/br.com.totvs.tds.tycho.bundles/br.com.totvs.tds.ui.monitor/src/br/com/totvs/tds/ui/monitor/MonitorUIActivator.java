package br.com.totvs.tds.ui.monitor;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.services.IServiceLocator;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.TDSMessageHandler;
import br.com.totvs.tds.ui.monitor.views.ServerMonitorView;

/**
 * The activator class controls the plug-in life cycle
 */
public class MonitorUIActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "br.com.totvs.tds.ui.monitor"; //$NON-NLS-1$

	// The shared instance
	private static MonitorUIActivator plugin;

	private IServerManager serverManager;

	/**
	 * The constructor
	 */
	public MonitorUIActivator() {
	}

	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;

		final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		final IPartService partService = window.getPartService();

		final IPartListener listener = new IPartListener() {

			@Override
			public void partOpened(final IWorkbenchPart part) {
				if (part.getSite().getId().equals(ServerMonitorView.VIEW_ID)) {
					final ServerMonitorView serverMonitorView = (ServerMonitorView) part;
					serverMonitorView.startMonitorJob();
				}

			}

			@Override
			public void partDeactivated(final IWorkbenchPart part) {
			}

			@Override
			public void partClosed(final IWorkbenchPart part) {
				if (part.getSite().getId().equals(ServerMonitorView.VIEW_ID)) {
					final ServerMonitorView serverMonitorView = (ServerMonitorView) part;
					serverMonitorView.stopMonitorJob();
				}
			}

			@Override
			public void partBroughtToTop(final IWorkbenchPart part) {
			}

			@Override
			public void partActivated(final IWorkbenchPart part) {
			}
		};

		partService.addPartListener(listener);
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
	 * Inicializa a sess�o de armazenamento de configura��es de di�logos.
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

	public IServerManager getServerManager() {
		if (serverManager == null) {
			final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			serverManager = serviceLocator.getService(IServerManager.class);
		}
		return serverManager;
	}
}
