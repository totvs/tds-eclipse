package br.com.totvs.tds.ui.monitor.handlers;

import java.util.ArrayList;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.internal.LogixServerInfo;
import br.com.totvs.tds.server.internal.ProtheusServerInfo;
import br.com.totvs.tds.server.ui.ServerUIUtil;
import br.com.totvs.tds.server.ui.internal.monitors.views.ItemMonitor;
import br.com.totvs.tds.server.ui.internal.monitors.views.ItemMonitor.MonitorType;
import br.com.totvs.tds.server.ui.internal.monitors.views.SendMessageDialog;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;

public class SendMessageUser extends ServerMonitorHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		ItemMonitor[] select = getSelections();

		ArrayList<ItemMonitor> servers = new ArrayList<>();

		for (ItemMonitor s : select) {
			if (s.getMonitorType().equals(MonitorType.SERVER)) {
				servers.add(s);
			}
		}

		if (!servers.isEmpty()) {
			select = servers.toArray(new ItemMonitor[servers.size()]);
		}

		SendMessageDialog dialog = new SendMessageDialog(new Shell(), select);
		dialog.open();

		return null;
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
			isEnabled = selection != null && !(serverInfo.getServerType().contains("dba")); //$NON-NLS-1$
			if (serverInfo instanceof LogixServerInfo) {
				isEnabled = isEnabledLogix(serverInfo);
			} else if (serverInfo instanceof ProtheusServerInfo) {
				isEnabled = isEnabledProtheus(serverInfo);
			}
		}
		return isEnabled;
	}

	private boolean isEnabledLogix(final IServerInfo serverInfo) {
		LogixServerInfo logixInfo = (LogixServerInfo) serverInfo;
		return ServerUIUtil.isLocalhost(serverInfo) || logixInfo.isSendMessageEnabled();
	}

	private boolean isEnabledProtheus(final IServerInfo serverInfo) {
		ProtheusServerInfo protheusInfo = (ProtheusServerInfo) serverInfo;
		String userType = protheusInfo.getPersistentProperty("UserTypeServer"); //$NON-NLS-1$
		return ServerUIUtil.isLocalhost(serverInfo) || (userType.equals("0") && protheusInfo.isSendMessageEnabled()); //$NON-NLS-1$
	}

}
