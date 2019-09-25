package br.com.totvs.tds.ui.server.handlers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Aciona a Edição de item selecionado.
 *
 * @author acandido
 */
public final class RemoveItemHandler extends ServerHandler {

	private void disconectServer(IServerInfo server) {
		IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		ICommandService commandService = serviceLocator.getService(ICommandService.class);

		Command command = commandService.getCommand("br.com.totvs.tds.ui.server.commands.disconectCommand"); //$NON-NLS-1$

		Map<String, Object> parameters = new HashMap<String, Object>();
		parameters.put("server", server.getName()); //$NON-NLS-1$

		ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, parameters);

		IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);
		try {
			handlerService.executeCommand(pc, null);
		} catch (Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, Messages.RemoveItemHandler_server_view, e.getMessage(), e);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.
	 * ExecutionEvent)
	 */
	@Override
	public Object execute(final ExecutionEvent event) {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IItemInfo[] elements = getSelections();

		Shell shell = HandlerUtil.getActiveShell(event);
		String message = String.format(Messages.RemoveItemHandler_exclusion_confirm_one, elements[0].getName());

		if (elements.length > 1) {
			message = String.format(Messages.RemoveItemHandler_exclusion_confirm_more_one, elements.length);
		}

		MessageDialog dialog = new MessageDialog(shell, Messages.RemoveItemHandler_delete, null, message,
				MessageDialog.QUESTION, new String[] { Messages.RemoveItemHandler_yes, Messages.RemoveItemHandler_no },
				1);
		int result = dialog.open();
		int groupOptionRemove = 0;

		if (result == 0) {
			if ((elements != null) && (elements.length > 0)) {
				for (int i = 0; i < elements.length; i++) {
					IItemInfo itemInfo = elements[i];

					if (itemInfo instanceof IEnvironmentInfo) {
						IAppServerInfo server = (IAppServerInfo) itemInfo.getParent();
						server.removeEnvironment((IEnvironmentInfo) itemInfo);
						serverManager.refresh(server);
						ServerActivator.logStatus(IStatus.ERROR, Messages.RemoveItemHandler_server_view,
								Messages.RemoveItemHandler_environment_removed, itemInfo.getName(), server.getName());
						continue;
					}

					if (itemInfo instanceof IGroupInfo) {
						IGroupInfo group = (IGroupInfo) itemInfo;
						if (group.hasChildren()) {
							if (groupOptionRemove != 2) {
								dialog = new MessageDialog(shell, Messages.RemoveItemHandler_delete, null,
										String.format(Messages.RemoveItemHandler_group_removed, group.getName()),
										MessageDialog.QUESTION, new String[] { Messages.RemoveItemHandler_yes,
												Messages.RemoveItemHandler_no, Messages.RemoveItemHandler_no_for_all },
										1);
								groupOptionRemove = dialog.open();
							}

							if (groupOptionRemove != 0) {
								continue;
							}
						}
					} else if (itemInfo instanceof IServerInfo) {
						IServerInfo server = (IServerInfo) itemInfo;
						if (server.isConnected()) {
							ServerActivator.logStatus(IStatus.WARNING, Messages.RemoveItemHandler_server_view,
									Messages.RemoveItemHandler_desconnect_process, server.getName());
							disconectServer(server);
						}
					}

					serverManager.remove(itemInfo);
					ServerActivator.logStatus(IStatus.WARNING, Messages.RemoveItemHandler_server_view,
							Messages.RemoveItemHandler_item_removed, itemInfo.getName());
				}
			}
		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		boolean ret = super.isEnabled();

		if (ret) {
			IItemInfo selection = getSelection();
			if ((selection == null) || (selection.getParent() == null)) { // não permite remover o n� raiz
				ret = false;
			}
		}

		return ret;
	}

}
