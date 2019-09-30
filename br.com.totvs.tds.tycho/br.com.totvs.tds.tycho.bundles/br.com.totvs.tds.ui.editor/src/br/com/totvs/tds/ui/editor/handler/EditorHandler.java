package br.com.totvs.tds.ui.editor.handler;

import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.editor.EditorActivator;

public abstract class EditorHandler extends AbstractHandler {

	/**
	 * @return seleção corrente corrente.
	 */
	protected ISelection getCurrentSelection() {
		final ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
				.getSelectionService();
		final ISelection selection = selectionService.getSelection();

		return selection;
	}

	/**
	 * @return seleção corrente, se esta for do tipo estruturada.
	 */
	protected IStructuredSelection getCurrentStructuredSelection() {
		final ISelection selection = getCurrentSelection();

		if (selection instanceof IStructuredSelection) {
			return (IStructuredSelection) selection;
		}

		return null;
	}

	/**
	 * @return lista de elementos de determinado tipo selecionados.
	 */
	protected boolean isSelected(final Class<?> clazz) {
		boolean result = false;
		final IStructuredSelection selection = getCurrentStructuredSelection();

		if (selection != null) {
			for (final Iterator<?> it = selection.iterator(); it.hasNext();) {
				final Object element = it.next();

				if (element.getClass().isInstance(clazz)) {
					result = true;
					break;
				}
			}
		}

		return result;
	}

	protected void checkPermission() throws ExecutionException {
		final IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		final IAppServerInfo server = serverManager.getCurrentServer();
		final String environment = server.getCurrentEnvironment();
		final String username = server.getUsername();

		if (!server.canPermission("COMPILE")) {
			final IStatus status = EditorActivator.logStatus(IStatus.ERROR, "Permiss�o",
					"Usu�rio [%s] sem permiss�o para compilar em [%s/%s].", username, server.getName(), environment);
			throw new ExecutionException(status.getMessage(), status.getException());
		}
	}

	protected void checkServer() throws ExecutionException {
		final IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		final IAppServerInfo server = serverManager.getCurrentServer();

		if (server == null) {
			final IStatus status = EditorActivator.logStatus(IStatus.ERROR, "Compilação",
					"Não foi selecionado nenhum servidor Protheus.");
			throw new ExecutionException(status.getMessage(), status.getException());
		}

		if (server.getCurrentEnvironment() == null) {
			final IStatus status = EditorActivator.logStatus(IStatus.ERROR, "Compilação",
					"Não foi selecionado nenhum ambiente para o servidor [%s]", server.getName());
			throw new ExecutionException(status.getMessage(), status.getException());
		}
		if (!server.isConnected()) {
			final IStatus status = EditorActivator.logStatus(IStatus.ERROR, "Compilação",
					"Servidor [%s] n�o conectado.", server.getName());
			throw new ExecutionException(status.getMessage(), status.getException());
		}
	}
}
