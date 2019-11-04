package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

import br.com.totvs.osgi.notificationcenter.INotification;
import br.com.totvs.osgi.notificationcenter.INotificationService;

public class RecordMonitorView extends ServerMonitorHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		Command command = event.getCommand();
		boolean oldValue = HandlerUtil.toggleCommandState(command);
		// use the old value and perform the operation

		if (event.getTrigger() instanceof Event) {
			Event we = (Event) event.getTrigger();
			MenuItem mi = (MenuItem) we.widget;
			mi.setSelection(!oldValue);

			BundleContext bundleContext = FrameworkUtil.getBundle(INotificationService.class).getBundleContext();
			ServiceReference<?> sr = bundleContext.getServiceReference(INotificationService.class.getName());
			INotificationService notificationService = (INotificationService) bundleContext.getService(sr);

			INotification notification = notificationService.createNotification(INotification.EventType.MONITOR_REFRESH,
			        this);
			notification.setData("recordLog", !oldValue);
			notificationService.send(notification);
		}
		
		return null;
	}

	@Override
	public boolean isEnabled() {

		return true;
	}

	@Override
	public boolean isHandled() {

		return true;
	}

	@Override
	public void removeHandlerListener(IHandlerListener handlerListener) {

	}

}
