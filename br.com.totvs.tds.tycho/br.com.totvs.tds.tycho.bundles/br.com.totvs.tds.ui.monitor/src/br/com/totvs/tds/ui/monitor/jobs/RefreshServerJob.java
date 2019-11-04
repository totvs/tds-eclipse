package br.com.totvs.tds.ui.monitor.jobs;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;

import br.com.totvs.tds.server.rulers.ServerRules;
import br.com.totvs.tds.ui.TDSUIActivator;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class RefreshServerJob extends Job {

	private IServerMonitor serverMonitor;
	private List<IUserMonitor> users = Collections.emptyList();

	public RefreshServerJob(final IServerMonitor serverMonitor) {
		super(String.format("Monitor %s", serverMonitor.getServerName()));
		this.serverMonitor = serverMonitor;
		setRule(ServerRules.monitorRule(serverMonitor.getServerInfo().getId()));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T getAdapter(final Class<T> adapter) {
		if (adapter.equals(IServerMonitor.class)) {
			return (T) serverMonitor;
		} else if (adapter.equals(List.class)) {
			return (T) users;
		}

		return super.getAdapter(adapter);
	}

	@Override
	public IStatus run(final IProgressMonitor monitor) {
		if (monitor.isCanceled() || (getJobGroup().getState() == JobGroup.CANCELING)) {
			return Status.CANCEL_STATUS;
		}

		if (!serverMonitor.getServerInfo().isConnected()) {
			return TDSUIActivator.logStatus(IStatus.WARNING, "%s: Não conectado", serverMonitor.getServerName());
		}

		monitor.beginTask(String.format("Monitorando %s", serverMonitor.getServerName()), 2);
		monitor.worked(1);
		monitor.setTaskName("Obtendo informações");
		users = serverMonitor.getUsers();
		monitor.worked(1);

		return monitor.isCanceled() ? Status.CANCEL_STATUS : Status.OK_STATUS;
	}

}
