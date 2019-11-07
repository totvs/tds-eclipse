package br.com.totvs.tds.ui.server.handlers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.PlatformUI;

/**
 * Classe abstrata de tratamento que utilizam ações para servidores
 *
 * @author acandido
 */
public abstract class AbstractServerHandler extends AbstractHandler {

	private static final List<Object> EMPTY_ARRAY = Collections.emptyList();

	/**
	 * @param exception Erro a ser registrado.
	 */
	abstract protected void createThrow(final Exception exception);

	protected <T> T getSelection(Class<T> clazz) {
		ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getSelectionService();
		ISelection selection = selectionService.getSelection();
		Object element = null;

		if (selection != null) {
			if (selection instanceof IStructuredSelection) {
				element = ((IStructuredSelection) selection).getFirstElement();
				if (!clazz.isInstance(element)) {
					element = null;
				}
			}
		}

		return clazz.cast(element);

	}

	protected List<?> getSelections(Class<?> clazz) {
		ISelectionService selectionService = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getSelectionService();
		ISelection selection = selectionService.getSelection();
		List<Object> elements = EMPTY_ARRAY;

		if (selection != null & selection instanceof IStructuredSelection & !selection.isEmpty()) {
			elements = new ArrayList<Object>();
			for (Object element : ((IStructuredSelection) selection).toList()) {
				if (clazz.isInstance(element)) {
					elements.add(element);
				}
			}
		}

		return elements;
	};
}
