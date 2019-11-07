package br.com.totvs.tds.ui.monitor.jobs;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class DisconnectUserJob extends Job {

	private ArrayList<IUserMonitor> users;
	private boolean immediately;
	private boolean notifyBefore;

	public DisconnectUserJob(final ArrayList<IUserMonitor> users, final boolean immediately,
			final boolean notifyBefore) {
		super("Desconexão de Usuários");
		this.users = users;
		this.immediately = immediately;
		this.notifyBefore = notifyBefore;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		monitor.beginTask("Envio de mensagem", users.size());

		monitor.setTaskName("Verificando usuários");
		final Map<IServerMonitor, List<IUserMonitor>> usersMap = users.stream()
				.collect(Collectors.groupingBy(IUserMonitor::getParent));

		monitor.setTaskName("Desconectando usuário");
		for (final Entry<IServerMonitor, List<IUserMonitor>> element : usersMap.entrySet()) {

			for (final IUserMonitor user : element.getValue()) {
				monitor.worked(1);
				final IStatus status = user.getParent().killUser(this.immediately, user.getUsername(),
						user.getComputerName(), user.getThreadId(), user.getServer());

				if (!status.isOK()) {
					return status;
				}
			}
		}

		return Status.OK_STATUS;

	}
}
