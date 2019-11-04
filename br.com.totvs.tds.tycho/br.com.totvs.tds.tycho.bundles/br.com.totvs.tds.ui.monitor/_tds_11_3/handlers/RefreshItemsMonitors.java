package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import br.com.totvs.tds.server.ui.jobs.RefreshServerViewJob;

public class RefreshItemsMonitors extends ServerMonitorHandler {

	private boolean isEnabled = true;

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		RefreshServerViewJob job = new RefreshServerViewJob("RefreshMonitor", 0); //$NON-NLS-1$
		job.schedule();
		return null;
	}

	@Override
	public boolean isEnabled() {
		return isEnabled;
	}

}
