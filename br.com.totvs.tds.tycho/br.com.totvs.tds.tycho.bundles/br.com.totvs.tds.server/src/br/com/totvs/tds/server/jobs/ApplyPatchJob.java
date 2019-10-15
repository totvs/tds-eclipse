package br.com.totvs.tds.server.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import br.com.totvs.tds.server.ServerActivator;

/**
 * Job de aplicação de patchs.
 */
public final class ApplyPatchJob extends ProcessPatchJob {

	/**
	 * aplicação de patch.
	 *
	 * @param name nome do job
	 */
	public ApplyPatchJob(final String name, final ApplyPatchAttributes applyPatchAttributes) {
		super(name, applyPatchAttributes);
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		IStatus status = Status.CANCEL_STATUS;

		final int totalWork = (getApplyPatchAttributes().getApplyPatchFilesReturn().size() + 1) * 10;
		monitor.beginTask("Aplicação de Pacote de Atualização", totalWork);
		try {
			status = doApplyPatches(monitor);
			// }
		} catch (final Exception e) {
			status = ServerActivator.createStatus(IStatus.ERROR, e.getMessage(), e);
		}
		//
		return status;
	}

}