package br.com.totvs.tds.ui.server.handlers;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Classe abstrata de tratamento que utilizam ações para servidores
 *
 * @author acandido
 */
public abstract class ServerHandler extends AbstractServerHandler {

	/**
	 * @param exception Erro a ser registrado.
	 */
	@Override
	protected void createThrow(final Exception exception) {
		final Status status = new Status(IStatus.ERROR, ServerUIActivator.PLUGIN_ID, exception.getMessage(), exception); // $NON-NLS-1$
		ServerUIActivator.logStatus(status);
	}

	/**
	 * @return o primeiro elemento selecionado.
	 */
	protected IItemInfo getSelection() {
		return super.getSelection(IItemInfo.class);
	}

	/**
	 * @return lista de elementos selecionados
	 */
	protected IItemInfo[] getSelections() {
		List<?> result = super.getSelections(IItemInfo.class);

		return result.toArray(new IItemInfo[result.size()]);
	}
}
