package br.com.totvs.tds.ui.server.views;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.State;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;

public class ServerViewerComparator extends ViewerComparator implements IExecutionListener {

	private boolean alphabeticalOrder;
	private final int down = 1;
	private final int equals = 0;

	private final int up = -1;
	private TreeViewer viewer;

	/**
	 * Construtor base.
	 */
	public ServerViewerComparator(TreeViewer viewer) {
		super();
		this.viewer = viewer;

		IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		ICommandService commandService = serviceLocator.getService(ICommandService.class);
		Command command = commandService.getCommand("br.com.totvs.tds.ui.server.commands.sortTreeCommand"); //$NON-NLS-1$
		command.addExecutionListener(this);

		State state = command.getState("org.eclipse.ui.commands.toggleState"); //$NON-NLS-1$
		if (state == null) {
			this.alphabeticalOrder = true;
		} else {
			Boolean value = (Boolean) state.getValue();
			this.alphabeticalOrder = value;
		}
	}

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) {
		if (hasNull(e1, e2)) {
			return equals;
		}

		IItemInfo node1 = (IItemInfo) e1;
		IItemInfo node2 = (IItemInfo) e2;

		if (!((node1 instanceof IGroupInfo) && (node2 instanceof IGroupInfo))) {
			if (node1 instanceof IGroupInfo)
				return up;
			if (node2 instanceof IGroupInfo)
				return down;
		}

		if (alphabeticalOrder) {
			String name1 = node1.getName().toLowerCase();
			String name2 = node2.getName().toLowerCase();

			return name1.compareTo(name2);
		}

		return node1.getId().compareTo(node2.getId());
	}

	private boolean hasNull(final Object e1, final Object e2) {
		return e1 == null || e2 == null;
	}

	@Override
	public void notHandled(String commandId, NotHandledException exception) {
		System.out.println("ServerViewerComparator.notHandled()"); //$NON-NLS-1$

	}

	@Override
	public void postExecuteFailure(String commandId, ExecutionException exception) {
		System.out.println(exception);
		this.viewer.refresh();
	}

	@Override
	public void postExecuteSuccess(String commandId, Object returnValue) {
		this.alphabeticalOrder = (boolean) returnValue;
		this.viewer.refresh();
	}

	@Override
	public void preExecute(String commandId, ExecutionEvent event) {
		// TODO Auto-generated method stub
		System.out.println("ServerViewerComparator.preExecute()"); //$NON-NLS-1$
	}

}