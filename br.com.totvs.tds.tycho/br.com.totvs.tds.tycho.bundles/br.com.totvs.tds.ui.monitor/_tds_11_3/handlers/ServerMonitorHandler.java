package br.com.totvs.tds.ui.monitor.handlers;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.statushandlers.StatusManager;

/**
 * Classe abstrata para 'handler' que utilizam o servi�o ISelectionService,<br>
 * para habilitar ou n�o comandos com base na lista de ID�s disponibilizado
 * por<br>
 * ItemMonitor.
 *
 * @author Matheus.Sales
 */
public abstract class ServerMonitorHandler extends AbstractHandler {

	/**
	 * @return o primeiro elemento selecionado.
	 */
	protected IItemMonitor getSelection() {
		final ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
				.getSelectionService();
		final ISelection selection = selectionService.getSelection();
		IItemMonitor element = null;

		if ((selection != null) & (selection instanceof IStructuredSelection)) {
			final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
			if (firstElement instanceof ItemMonitor) {
				element = (IItemMonitor) firstElement;
			}
		}

		return element;
	}

	/**
	 * @return lista de elementos selecionados
	 */
	protected ItemMonitor[] getSelections() {
		final ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
				.getSelectionService();
		final ISelection selection = selectionService.getSelection();
		ItemMonitor[] elements = null;

		if ((selection != null) & (selection instanceof IStructuredSelection)) {
			final List<?> list = ((IStructuredSelection) selection).toList();
			elements = list.toArray(new ItemMonitor[list.size()]);
		}

		return elements;
	}

	/**
	 * Lan�a erro de execu��o.
	 *
	 * @param exception Erro a ser lan�ado.
	 */
	protected void createThrow(final Exception exception) {
		final Status status = new Status(IStatus.ERROR, "<plugid>", exception.getMessage(), exception); //$NON-NLS-1$
		StatusManager.getManager().handle(status, StatusManager.BLOCK | StatusManager.LOG);
	}

}
