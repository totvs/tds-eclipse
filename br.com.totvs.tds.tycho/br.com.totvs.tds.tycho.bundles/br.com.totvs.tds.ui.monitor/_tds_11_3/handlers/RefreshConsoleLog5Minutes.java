package br.com.totvs.tds.server.ui.log.console.commands;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

import br.com.totvs.osgi.notificationcenter.INotification;
import br.com.totvs.osgi.notificationcenter.INotificationService;
import br.com.totvs.osgi.notificationcenter.NotificationHandler;
import br.com.totvs.tds.ui.monitor.handlers.ServerMonitorHandler;

public class RefreshConsoleLog5Minutes extends ServerMonitorHandler {

	private boolean isEnabled = true;
	private RefreshServerConsoleLogJob job;

	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		sendNotification();
		hookNotification();
		if (job != null) {
			job.stop();
		}
		job = new RefreshServerConsoleLogJob("Refresh 5 minutes", 300000); //$NON-NLS-1$
		job.schedule();
		isEnabled = false;
		return null;
	}

	private void sendNotification() {
		BundleContext bundleContext = FrameworkUtil.getBundle(INotificationService.class).getBundleContext();
		ServiceReference<?> sr = bundleContext.getServiceReference(INotificationService.class.getName());
		INotificationService notificationService = (INotificationService) bundleContext.getService(sr);

		INotification notification = notificationService.createNotification(
				INotification.EventType.CONSOLE_REFRESH_5_MIN, this);
		notificationService.send(notification);
	}

	private void hookNotification() {
		BundleContext bundleContext = FrameworkUtil.getBundle(INotificationService.class).getBundleContext();
		ServiceReference<?> sr = bundleContext.getServiceReference(INotificationService.class.getName());
		INotificationService notificationService = (INotificationService) bundleContext.getService(sr);

		NotificationHandler eventHandlerStateConnection = new NotificationHandler() {
			@Override
			public void receive(final INotification notification) {
				if (notification.getEventType() == INotification.EventType.CONSOLE_DEL_ITEM
						|| notification.getEventType() == INotification.EventType.CONSOLE_REFRESH_10_MIN
						|| notification.getEventType() == INotification.EventType.CONSOLE_REFRESH_2_MIN) {
					job.stop();
					isEnabled = true;
				}
			}
		};

		notificationService.subscribe(INotification.EventType.CONSOLE_DEL_ITEM,
				eventHandlerStateConnection);
		notificationService.subscribe(INotification.EventType.CONSOLE_REFRESH_10_MIN,
				eventHandlerStateConnection);
		notificationService.subscribe(INotification.EventType.CONSOLE_REFRESH_2_MIN,
				eventHandlerStateConnection);
	}

	@Override
	public final boolean isEnabled() {
		return isEnabled;
	}

}
