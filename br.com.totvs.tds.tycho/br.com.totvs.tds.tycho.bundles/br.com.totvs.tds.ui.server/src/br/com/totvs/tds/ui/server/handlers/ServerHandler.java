package br.com.totvs.tds.ui.server.handlers;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.interfaces.IItemInfo;

/**
 * Classe abstrata de tratamento que utilizam ações para servidores
 *
 * @author acandido
 */
public abstract class ServerHandler extends AbstractHandler {

	private static final IItemInfo[] EMPTY_ARRAY = new IItemInfo[0];

	/**
	 * @return o primeiro elemento selecionado.
	 */
	protected IItemInfo getSelection() {
		ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getSelectionService();
		ISelection selection = selectionService.getSelection();
		IItemInfo element = null;

		if (selection != null) {
			if (selection instanceof IStructuredSelection) {
				element = (IItemInfo) ((IStructuredSelection) selection).getFirstElement();
			}
		}

		return element;
	}

	/**
	 * @return lista de elementos selecionados
	 */
	protected IItemInfo[] getSelections() {
		ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getSelectionService();
		ISelection selection = selectionService.getSelection();
		IItemInfo[] elements = EMPTY_ARRAY;

		if (selection != null & selection instanceof IStructuredSelection) {
			List<?> list = ((IStructuredSelection) selection).toList();
			elements = list.toArray(new IItemInfo[list.size()]);
		}

		return elements;
	}

//	/**
//	 * Called by the framework to allow the handler to update its enabled state by
//	 * extracting the same information available at execution time. Clients may
//	 * override if they need to extract information from the application context.
//	 *
//	 * @param evaluationContext the application context. May be <code>null</code>
//	 * @since 3.4
//	 * @see #setBaseEnabled(boolean)
//	 */
//	@Override
//	public void setEnabled(Object evaluationContext) {
//		this.evaluationContext = evaluationContext;
//	}
}
