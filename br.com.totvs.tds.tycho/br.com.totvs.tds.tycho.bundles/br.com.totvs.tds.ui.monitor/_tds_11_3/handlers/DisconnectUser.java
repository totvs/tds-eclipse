package br.com.totvs.tds.ui.monitor.handlers;

import java.util.ArrayList;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

import br.com.totvs.osgi.notificationcenter.INotification;
import br.com.totvs.osgi.notificationcenter.INotificationService;
import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.connector.IServerConnector;
import br.com.totvs.tds.server.connector.IServerMonitorConnector;
import br.com.totvs.tds.server.dba.ini.IDbaServerInfo;
import br.com.totvs.tds.server.dba.internal.DbaServerInfo;
import br.com.totvs.tds.server.factory.ServerConnectorFactory;
import br.com.totvs.tds.server.internal.LogixServerInfo;
import br.com.totvs.tds.server.internal.ProtheusServerInfo;
import br.com.totvs.tds.server.ui.Messages;
import br.com.totvs.tds.server.ui.ServerUIActivator;
import br.com.totvs.tds.server.ui.ServerUIUtil;
import br.com.totvs.tds.server.ui.internal.monitors.views.DisconnectUserDialogMessage;
import br.com.totvs.tds.server.ui.internal.monitors.views.DisconnectUserDialogMessage.TypeDisconnect;
import br.com.totvs.tds.server.ui.internal.monitors.views.ItemMonitor;
import br.com.totvs.tds.server.ui.internal.monitors.views.ItemMonitor.MonitorType;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;
import br.com.totvs.tds.startup.log.TdsLogging;

public class DisconnectUser extends ServerMonitorHandler {

	ItemMonitor[] itemsMonitor;

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		try {
			itemsMonitor = getSelections();

			ArrayList<ItemMonitor> servers = new ArrayList<>();

			for (ItemMonitor s : itemsMonitor) {
				if (s.getMonitorType().equals(MonitorType.SERVER)) {
					servers.add(s);
				}
			}

			if (!servers.isEmpty()) {
				itemsMonitor = servers.toArray(new ItemMonitor[servers.size()]);
			}

			String question = getConfirmText(itemsMonitor);

			DisconnectUserDialogMessage dialog = new DisconnectUserDialogMessage(new Shell(), question);
			int open = dialog.open();
			if (open == Window.OK) {
				TypeDisconnect type = dialog.getResult();
				okPressed(type);
			}

			sendRefreshNotification();
		} catch (Exception e) {
			e.printStackTrace();
			ServerUIUtil.createStatus(e);
		}

		return null;
	}

	private void sendRefreshNotification() {
		BundleContext bundleContext = FrameworkUtil.getBundle(INotificationService.class).getBundleContext();
		ServiceReference<?> sr = bundleContext.getServiceReference(INotificationService.class.getName());
		INotificationService notificationService = (INotificationService) bundleContext.getService(sr);

		INotification notification = notificationService.createNotification(INotification.EventType.MONITOR_REFRESH,
				this);
		notificationService.send(notification);
	}

	private void okPressed(final TypeDisconnect type) {
		try {
			for (IItemMonitor item : itemsMonitor) {
				IServerConnector serverConnector = ServerConnectorFactory.getConnector(item.getServerInfo());
				IServerMonitorConnector monitorConnector = null;

				if (serverConnector instanceof IServerMonitorConnector) {
					monitorConnector = (IServerMonitorConnector) serverConnector;
				}

				if (item.getMonitorType().equals(MonitorType.SERVER)) {
					for (IItemMonitor children : item.getChildren()) {
						Integer threadID = Integer.valueOf(children.getThreadId());
						if (type == TypeDisconnect.POSSIBLE) {
							if (!children.getObservations().contains("TOTVS Developer Studio")) { //$NON-NLS-1$
								monitorConnector.disconnectUser(children.getUser(), children.getMachine(), threadID,
										children.getUserServer());
							}
						} else if (type == TypeDisconnect.IMMEDIATELY) {
							if (!children.getObservations().contains("TOTVS Developer Studio")) { //$NON-NLS-1$
								monitorConnector.disconnectUserImmediately(children.getUser(), children.getMachine(),
										threadID, children.getUserServer());
							}
						}
					}
				} else {
					// A desconex�o de usu�rios TDS foi bloqueada, s� � poss�vel desconectar usu�rios que n�o sejam o
					// TDS.
					if (item.getObservations().contains("TOTVS Developer Studio")) { //$NON-NLS-1$
						TdsLogging.getDefault().showError(ServerUIActivator.PLUGIN_ID,
								Messages.DisconnectUser_3);
						return;
					}

					Integer threadID = (int) (Long.valueOf(item.getThreadId()) & 0xFFFFFFFFL);
					if (type == TypeDisconnect.POSSIBLE) {
						monitorConnector.disconnectUser(item.getUser(), item.getMachine(), threadID,
								item.getUserServer());
					} else if (type == TypeDisconnect.IMMEDIATELY) {
						monitorConnector.disconnectUserImmediately(item.getUser(), item.getMachine(), threadID,
								item.getUserServer());
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			ServerUIUtil.createStatus(e);
		}

	}

	private String getConfirmText(final IItemMonitor[] itemsMonitor) {
		boolean isSingular = true;
		boolean isUser = true;

		String names = ""; //$NON-NLS-1$
		String text = ""; //$NON-NLS-1$

		for (IItemMonitor itemMonitor : itemsMonitor) {
			if (itemMonitor.getMonitorType().equals(MonitorType.USER)) {
				if (itemsMonitor.length <= 1) {
					names = itemMonitor.getUser();
					isSingular = true;
				} else {
					isSingular = false;
					names += itemMonitor.getUser() + ", "; //$NON-NLS-1$
				}
			} else {
				if (itemsMonitor.length <= 1) {
					isUser = false;
					isSingular = true;
					names = itemMonitor.getServerName();
				} else {
					isUser = false;
					isSingular = false;
					names += itemMonitor.getServerName() + ", "; //$NON-NLS-1$
				}
			}
		}

		if (names.endsWith(", ")) { //$NON-NLS-1$
			names = names.substring(0, names.length() - 2);
		}

		if (isSingular) {
			if (isUser) {
				text = String.format(Messages.DisconnectUser_9, names);
			} else {
				text = String.format(Messages.DisconnectUser_10, names);
			}
		} else {
			if (isUser) {
				text = String.format(Messages.DisconnectUser_11, names);
			} else {
				text = String.format(Messages.DisconnectUser_12, names);
			}
		}

		return text;
	}

	@Override
	public boolean isEnabled() {
		IItemMonitor selection = getSelection();
		if (selection == null) {
			return false;
		}
		IServerInfo serverInfo = selection.getServerInfo();
		boolean isEnabled = false;
		if (serverInfo.isConnected()) {
			if (serverInfo instanceof IDbaServerInfo) {
				DbaServerInfo dbaInfo = null;
				dbaInfo = (DbaServerInfo) serverInfo;
				boolean isKillUser = dbaInfo.isKillGuestUser();
				isEnabled = (selection.isAdmin() || isKillUser);
			} else if (serverInfo instanceof ProtheusServerInfo) {
				isEnabled = isEnabledProtheus(serverInfo);
			} else if (serverInfo instanceof LogixServerInfo) {
				isEnabled = isEnabledLogix(serverInfo);
			}
		}
		return isEnabled;
	}

	private boolean isEnabledLogix(final IServerInfo serverInfo) {
		LogixServerInfo logixInfo = (LogixServerInfo) serverInfo;
		return ServerUIUtil.isLocalhost(serverInfo) || logixInfo.isDisconnectUserEnabled();
	}

	private boolean isEnabledProtheus(final IServerInfo serverInfo) {
		ProtheusServerInfo protheusInfo = (ProtheusServerInfo) serverInfo;
		String userType = protheusInfo.getPersistentProperty("UserTypeServer"); //$NON-NLS-1$
		return ServerUIUtil.isLocalhost(serverInfo) || (userType.equals("0") && protheusInfo.isDisconnectUserEnabled()); //$NON-NLS-1$
	}

}
