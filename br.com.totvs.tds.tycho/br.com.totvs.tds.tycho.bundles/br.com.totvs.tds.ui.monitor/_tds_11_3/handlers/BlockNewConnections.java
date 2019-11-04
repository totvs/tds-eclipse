package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import br.com.totvs.tds.server.ConnType;
import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.connector.IServerConnector;
import br.com.totvs.tds.server.connector.version.ConnectionServer;
import br.com.totvs.tds.server.connector.version.ConnectionServer.ConnectionState;
import br.com.totvs.tds.server.dba.ini.IDbaServerInfo;
import br.com.totvs.tds.server.factory.ServerConnectorFactory;
import br.com.totvs.tds.server.internal.LogixServerInfo;
import br.com.totvs.tds.server.internal.ProtheusServerInfo;
import br.com.totvs.tds.server.ui.ServerUIUtil;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;

public class BlockNewConnections extends ServerMonitorHandler {

	@ConnectionServer(state = ConnectionState.REQUIRED)
	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		IItemMonitor itemMonitor = getSelection();
		IServerConnector connectorMonitor = null;

		try {
			connectorMonitor = ServerConnectorFactory.getConnector(itemMonitor.getServerInfo(), ConnType.MONITOR);
			connectorMonitor.connect(ConnType.MONITOR);
			if (connectorMonitor.isEnableConnection()) {
				connectorMonitor.setEnableConnection(false);
			} else {
				connectorMonitor.setEnableConnection(true);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		IItemMonitor selection = getSelection();
		if (selection == null) {
			return false;
		}
		boolean isEnabled = false;
		IServerInfo serverInfo = selection.getServerInfo();
		if (serverInfo.isConnected()) {
			if (serverInfo instanceof IDbaServerInfo) {
				IDbaServerInfo dbaInfo = (IDbaServerInfo) serverInfo;
				isEnabled = dbaInfo.isAdmin();
			} else if (serverInfo instanceof ProtheusServerInfo) {
				isEnabled = isEnabledProtheus(serverInfo);
			} else if (serverInfo instanceof LogixServerInfo) {
				isEnabled = isEnabledLogix(serverInfo);
			}
		}
		return isEnabled;
	}

	private boolean isEnabledLogix(IServerInfo serverInfo) {
		LogixServerInfo logixInfo = (LogixServerInfo) serverInfo;
		return ServerUIUtil.isLocalhost(serverInfo) || logixInfo.isBlockNewConnectionsEnabled();
	}

	private boolean isEnabledProtheus(final IServerInfo serverInfo) {
		ProtheusServerInfo protheusInfo = (ProtheusServerInfo) serverInfo;
		String userType = protheusInfo.getPersistentProperty("UserTypeServer"); //$NON-NLS-1$
		return ServerUIUtil.isLocalhost(serverInfo)
				|| (userType.equals("0") && protheusInfo.isBlockNewConnectionsEnabled()); //$NON-NLS-1$
	}

}
