package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.connector.IAppServerConnector;
import br.com.totvs.tds.server.factory.ServerConnectorFactory;
import br.com.totvs.tds.server.internal.LogixServerInfo;
import br.com.totvs.tds.server.internal.ProtheusServerInfo;
import br.com.totvs.tds.server.ui.Messages;
import br.com.totvs.tds.server.ui.ServerUIActivator;
import br.com.totvs.tds.server.ui.ServerUIUtil;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;
import br.com.totvs.tds.startup.log.TdsLogging;

public class MonitorStopServerCommand extends ServerMonitorHandler implements IHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IItemMonitor selection = getSelection();
		if (selection != null) {
			IServerInfo serverInfo = selection.getServerInfo();
			try {
				boolean isConfirmed = MessageDialog
						.openConfirm(
								new Shell(Display.getDefault()),
								Messages.MonitorStopServerCommand_0,
								Messages.MonitorStopServerCommand_1);
				if (isConfirmed) {
					IAppServerConnector connector = (IAppServerConnector) ServerConnectorFactory
							.getConnector(serverInfo);
					connector.stopServer();
					serverInfo.setConnected(false);
					TdsLogging.getDefault().logInformation(
							String.format(
									Messages.MonitorStopServerCommand_2,
									serverInfo.getName()));
				}
			} catch (Exception e) {
				TdsLogging.getDefault().showError(ServerUIActivator.PLUGIN_ID,
						Messages.MonitorStopServerCommand_3 + e.getMessage());
				e.printStackTrace();
			}
		}
		return null;
	}

	@Override
	public boolean isEnabled() {
		IItemMonitor selection = getSelection();
		if (selection == null) {
			return false;
		}
		IServerInfo serverInfo = selection.getServerInfo();
		boolean enable = false;
		if (serverInfo.isConnected()) {
			if (serverInfo instanceof ProtheusServerInfo) {
				enable = isEnabledProtheus(serverInfo);
			} else if (serverInfo instanceof LogixServerInfo) {
				enable = isEnabledLogix(serverInfo);
			}
		}
		return enable;
	}

	private boolean isEnabledLogix(final IServerInfo serverInfo) {
		LogixServerInfo logixInfo = (LogixServerInfo) serverInfo;
		return ServerUIUtil.isLocalhost(serverInfo) || logixInfo.isStopServerEnabled();
	}

	private boolean isEnabledProtheus(final IServerInfo serverInfo) {
		ProtheusServerInfo protheusInfo = (ProtheusServerInfo) serverInfo;
		String userType = protheusInfo.getPersistentProperty("UserTypeServer"); //$NON-NLS-1$
		return ServerUIUtil.isLocalhost(serverInfo) || (userType.equals("0") && protheusInfo.isStopServerEnabled()); //$NON-NLS-1$
	}

}
