package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.monitor.views.ServerMonitorView;
import br.com.totvs.tds.ui.server.handlers.ServerHandler;

public class ToggleServerMonitorHandler extends ServerHandler {

	private ServerMonitorView serverMonitorView;

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final Command command = event.getCommand();
		final boolean oldValue = HandlerUtil.toggleCommandState(command);

		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		serverMonitorView = (ServerMonitorView) page.findView(ServerMonitorView.VIEW_ID);

		if (serverMonitorView == null) {
			try {
				serverMonitorView = (ServerMonitorView) page.showView(ServerMonitorView.VIEW_ID);
			} catch (final PartInitException e) {
				throw new ExecutionException(e.getMessage(), e);
			}
		}

		final IItemInfo selection = getSelection();
		if (selection != null) {
			toggleServerMonitor(!oldValue, selection);
		}

		return !oldValue;
	}

	private void toggleServerMonitor(final boolean value, final IItemInfo selection) {
		if (selection instanceof IGroupInfo) {
			((IGroupInfo) selection).getChildren().forEach((final IItemInfo element) -> {
				toggleServerMonitor(value, element);
			});
		} else if (selection instanceof IAppServerInfo) {
			if (value) {
				serverMonitorView.addServer((IAppServerInfo) selection);
			} else {
				serverMonitorView.removeServer((IAppServerInfo) selection);
			}
		}
	}
}