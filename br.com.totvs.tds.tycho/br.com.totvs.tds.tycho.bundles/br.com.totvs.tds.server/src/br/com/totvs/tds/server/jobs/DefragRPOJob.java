package br.com.totvs.tds.server.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;

//final LoadDefragRPOJob job = new LoadDefragRPOJob(Messages.EnvironmentsPropertiesBlock_19, validEnvironments,
//		serverConnector, clearPatchLog);
//long tempoInicio = System.currentTimeMillis();
//
//job.schedule();
//try {
//	job.join();
//} catch (InterruptedException e) {
//	e.printStackTrace();
//}
//
//if (job.getResult().equals(Status.OK_STATUS)) {
//	double tempoTotal = (System.currentTimeMillis() - tempoInicio);
//	tempoTotal = tempoTotal / 1000;
//
//	TdsLogging.getDefault().logInformation(Messages.EnvironmentsPropertiesBlock_20 + tempoTotal + "s"); //$NON-NLS-1$
//} else {
//	TdsLogging.getDefault().logError(Messages.EnvironmentsPropertiesBlock_22);
//	TdsLogging.getDefault().logError(job.getResult().getMessage());
//}

public class DefragRPOJob extends Job {

	// todos os recursos do ADVPL liberados
	public final int BUILD_DEFRAG_RPO = 0;
	private boolean clearPatchLog;
	private IAppServerInfo server;
	private String[] environments;

	public DefragRPOJob(final IAppServerInfo server, final String[] environments) {
		super(String.format("Desfragmentação: %s", server.getName()));
		this.server = server;
		this.environments = environments;

		this.clearPatchLog = true;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {

		monitor.beginTask("Desfragmentação de RPO", environments.length); //$NON-NLS-1$

		for (final String environment : environments) {
			monitor.setTaskName(String.format("Ambiente: %s", environment));
			server.defragRPO(environment, this.clearPatchLog);

			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			monitor.worked(1);
		}

		return Status.OK_STATUS;
	}

}