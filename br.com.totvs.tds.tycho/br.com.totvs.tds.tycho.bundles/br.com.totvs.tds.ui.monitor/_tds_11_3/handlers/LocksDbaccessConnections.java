package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.ui.internal.monitors.views.DBAccessLocksDialog;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;

public class LocksDbaccessConnections extends ServerMonitorHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		DBAccessLocksDialog dialog = new DBAccessLocksDialog(new Shell());
		dialog.open();
		return null;
	}

	@Override
	public boolean isEnabled() {
		IItemMonitor select = getSelection();
		if (select == null) {
			return false;
		}
		IServerInfo serverInfo = select.getServerInfo();
		boolean isEnabled = false;
		if (serverInfo.isConnected()) {
			// boolean isUser = select.getMonitorType().equals(MonitorType.USER);
			isEnabled = serverInfo.getServerType().contains("dba"); //$NON-NLS-1$
		}
		return isEnabled;
	}

}
