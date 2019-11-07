package br.com.totvs.tds.ui.monitor.handlers;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import br.com.totvs.tds.ui.monitor.MonitorUIActivator;
import br.com.totvs.tds.ui.monitor.model.IItemMonitor;
import br.com.totvs.tds.ui.server.handlers.AbstractServerHandler;

/**
 * Classe abstrata de tratamento que utilizam ações para monitor de servidores
 *
 * @author acandido
 */
public abstract class ServerMonitorHandler extends AbstractServerHandler {

	/**
	 * @param exception Erro a ser registrado.
	 */
	@Override
	protected void createThrow(final Exception exception) {
		final Status status = new Status(IStatus.ERROR, MonitorUIActivator.PLUGIN_ID, exception.getMessage(),
				exception); // $NON-NLS-1$
		MonitorUIActivator.logStatus(status);
		// StatusManager.getManager().handle(status, StatusManager.BLOCK |
		// StatusManager.LOG);
	}

	/**
	 * @return o primeiro elemento selecionado.
	 */
	protected IItemMonitor getSelection() {
		return super.getSelection(IItemMonitor.class);
	}

	/**
	 * @return lista de elementos selecionados
	 */
	protected IItemMonitor[] getSelections() {
		final List<?> result = super.getSelections(IItemMonitor.class);

		return result.toArray(new IItemMonitor[result.size()]);
	}
}
