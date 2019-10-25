package br.com.totvs.tds.server.jobs;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.model.RpoTypeElement;

/**
 * Job to load all functions from the server.
 *
 * @author acandido
 * @author daniel.yampolschi
 *
 */
public class LoadRpoMapJob extends Job {
	protected IAppServerInfo server;
	protected String environment;
	private List<IRpoElement> rpoElements = Collections.emptyList();
	private RpoTypeElement objectType;
	private boolean includeTRes;

	public LoadRpoMapJob(final IAppServerInfo server, final String environment, final boolean includeTRes,
			final RpoTypeElement objectType) {
		super(String.format("Carregando mapa RPO: %s", objectType.getTitle()));

		this.server = server;
		this.environment = environment;
		this.includeTRes = includeTRes;
		this.objectType = objectType;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {

		final IStatus status = Status.CANCEL_STATUS;

		monitor.beginTask(getName(), IProgressMonitor.UNKNOWN);
		monitor.subTask("Esta etapa não é possível cancelar a operação e pode levar algum tempo.");

		try {
			this.rpoElements = server.getRpoMap(environment, objectType, includeTRes);
		} catch (final IllegalArgumentException e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		} catch (final Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		return status;
	}

	public int getListSize() {
		return rpoElements.size();
	}

	public List<IRpoElement> getList() {
		return rpoElements;
	}
}
