package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.handlers.HandlerUtil;

public class MonitorSelectionHandler extends ServerHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		Command command = event.getCommand();
		boolean oldValue = HandlerUtil.toggleCommandState(command);

		IWorkbenchPart wp = HandlerUtil.getActivePart(event);
		TreeViewer viewer = wp.getAdapter(TreeViewer.class);

		if (viewer != null) {
			viewer.refresh();
		}

		return !oldValue;
	}

}
