package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAppServerSlaveInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

public class SlaveRegisterHandler extends ServerHandler {

	@Override
	public Object execute(ExecutionEvent event) {
		IItemInfo selection = getSelection();

		if (selection instanceof IAppServerSlaveInfo) {
			IAppServerSlaveInfo slave = (IAppServerSlaveInfo) selection;
			IAppServerInfo master = slave.getMaster();
			IGroupInfo parent = (IGroupInfo) master.getParent();

			IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			IAppServerInfo server = serverManager
					.newAppServer(String.format("%s_%s", master.getName(), slave.getName())); //$NON-NLS-1$
			server.setAddress(slave.getAddress());
			server.setServerType(slave.getServerType());
			server.setVersion(master.getVersion());
			server.setSmartClientPath(master.getSmartClientPath());
			server.setLocalServer(master.isLocalServer());
			server.setAppServerPath(master.getAppServerPath());

			parent.addChild(server);
		} else {
			ServerUIActivator.logStatus(IStatus.WARNING, Messages.SlaveRegisterHandler_item_not_slave,
					selection.getName());
		}

		return null;
	}

}
