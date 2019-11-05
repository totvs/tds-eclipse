package br.com.totvs.tds.ui.monitor.jobs;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.monitor.LogSessionMonitor;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class ServerMonitorJob extends Job {

	public static final QualifiedName WRITE_LOG = new QualifiedName("RefreshServerJob", "write.log");

	private static JobGroup jobGroup;
	private IProgressMonitor progressGroup;

	{
		jobGroup = new JobGroup("Monitoramento", 2, 4);
		progressGroup = getJobManager().createProgressGroup();
	}

	private Map<String, IServerMonitor> serverMonitorMap;

	public ServerMonitorJob(final Map<String, IServerMonitor> serverMonitors) {
		super("Monitoramento");
		this.serverMonitorMap = serverMonitors;
		setUser(true);
		setProperty(WRITE_LOG, false);
	}

	@Override
	public boolean belongsTo(final Object family) {

		return this.getClass().equals(family);
	}

	@Override
	public IStatus run(final IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		final int ticks = (serverMonitorMap.size() * 2) + 2;
		setProgressGroup(progressGroup, ticks);

		monitor.beginTask(String.format("Monitorando %d servidores", serverMonitorMap.size()), ticks);

		final List<Job> jobs = new ArrayList<Job>();
		for (final Entry<String, IServerMonitor> entry : serverMonitorMap.entrySet()) {
			monitor.worked(1);

			final RefreshServerJob job = new RefreshServerJob(entry.getValue());
			jobs.add(job);
			job.setJobGroup(jobGroup);
			job.setProgressGroup(progressGroup, 3);
			job.schedule();

			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

		}

		final String taskName = "Aguardando dados";
		monitor.setTaskName(taskName);
		monitor.worked(1);

		do {
			try {
				jobGroup.join(3000, monitor);
			} catch (final OperationCanceledException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			monitor.setTaskName(taskName + "*");

		} while (!jobGroup.getActiveJobs().isEmpty());

		if (!monitor.isCanceled()) {

			monitor.setTaskName("Atualizando");

			LogSessionMonitor logMonitor = null;
			if ((boolean) getProperty(WRITE_LOG)) {
				logMonitor = new LogSessionMonitor();
				// logMonitor.open();
			}

			for (final Job job : jobs) {
				monitor.worked(1);

				final IServerMonitor serverMonitor = job.getAdapter(IServerMonitor.class);
				if (job.getResult() == null) {
					serverMonitor.setStateString("(sem retorno válido");
				} else if (job.getResult().isOK()) {
					@SuppressWarnings("unchecked")
					final List<IUserMonitor> userList = job.getAdapter(List.class);
					serverMonitor.setChildren(userList);
				} else {
					serverMonitor.setStateString(job.getResult().toString());
				}

				if (logMonitor != null) {
					// writeToFile(serverMonitor, logMonitor);
				}
			}
		}

		IStatus status = jobGroup.getResult();
		if (status == null) {
			status = Status.OK_STATUS;
		}

		return monitor.isCanceled() ? Status.CANCEL_STATUS : status;
	}

	private void writeToFile(final IServerMonitor serverMonitor, final LogSessionMonitor logMonitor2)
			throws IOException {

		final IAppServerInfo serverInfo = serverMonitor.getServerInfo();
		final List<IUserMonitor> users = serverMonitor.getChildren();
		final LogSessionMonitor logMonitor = new LogSessionMonitor();

		logMonitor.header(serverInfo.getAddress().toString(), users.size());
		// logMonitor.write(users);
		logMonitor.footer();
		logMonitor.close();
	}

	/**
	 * @return the jobgroup
	 */
	public void cancelJobs() {
		if (jobGroup != null) {
			jobGroup.cancel();
		}
	}

}
