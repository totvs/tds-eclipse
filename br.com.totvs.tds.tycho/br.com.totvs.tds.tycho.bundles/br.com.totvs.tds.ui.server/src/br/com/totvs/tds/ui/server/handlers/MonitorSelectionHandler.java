package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;

public class MonitorSelectionHandler extends ServerHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

//		Command command = event.getCommand();
//		boolean oldValue = HandlerUtil.toggleCommandState(command);
//
//		// use the old value and perform the operation
//		Object trigger = event.getTrigger();
//		if (trigger instanceof Event) {
//			Object widget = ((Event) trigger).item;
//			System.out.println(widget);
//		}

//		if (actionSort.isChecked()) {
//			viewer.setComparator(new ServerViewerComparator());
//		} else {
//			viewer.setComparator(null);
//		}
//		viewer.refresh();

		return null;
	}

	@Override
	public boolean isEnabled() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IAppServerInfo currentServer = serverManager.getCurrentServer();
		IAppServerInfo serverSelection = (IAppServerInfo) getSelection();

		return super.isEnabled() && serverSelection.equals(currentServer);
	}

}
