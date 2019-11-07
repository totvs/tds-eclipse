package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.monitor.views.ServerMonitorView;
import br.com.totvs.tds.ui.server.handlers.ServerHandler;

public class AddServerMonitorHandler extends ServerHandler {

	private ServerMonitorView serverMonitorView;

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
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
			addServerMonitor(selection);
		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		final IItemInfo selection = getSelection();
		IAppServerInfo server = null;

		if (selection instanceof IAppServerInfo) {
			server = (IAppServerInfo) selection;
		}

		return server == null ? true : server.isConnected() && !server.isPinnedMonitor();
	}

	private void addServerMonitor(final IItemInfo selection) {
		if (selection instanceof IGroupInfo) {
			((IGroupInfo) selection).getChildren().forEach((final IItemInfo element) -> {
				addServerMonitor(element);
			});
		} else if (selection instanceof IAppServerInfo) {
			serverMonitorView.addServer((IAppServerInfo) selection);
		}
	}
}