package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

public class RefreshMonitorHandler extends ServerMonitorHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		String value = event.getParameter("br.com.totvs.tds.ui.monitor.intervalParameter");

		if (value == null) {
			value = "0";
		}

		return Integer.valueOf(value);
	}

}
