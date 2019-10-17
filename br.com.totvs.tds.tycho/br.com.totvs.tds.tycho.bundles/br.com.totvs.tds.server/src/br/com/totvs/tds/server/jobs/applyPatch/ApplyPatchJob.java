package br.com.totvs.tds.server.jobs.applyPatch;

import java.net.URI;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;

/**
 * Job de aplicação de patchs.
 */
public final class ApplyPatchJob extends Job {

	private final ApplyPatchAttributes applyPatchAttributes;

	/**
	 * aplicação de patch.
	 *
	 * @param name nome do job
	 */
	public ApplyPatchJob(final ApplyPatchAttributes applyPatchAttributes) {
		super("Aplicar Atualizações");

		this.applyPatchAttributes = applyPatchAttributes;
		setUser(true);
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		IStatus status = Status.CANCEL_STATUS;
		final List<ApplyPatchFileReturn> applyPatchFilesReturn = applyPatchAttributes.getApplyPatchFilesReturn();
		int patchCount = 1;
		final int totalWork = (applyPatchFilesReturn.size());
		monitor.beginTask("Aplicação de Pacote de Atualização", totalWork);

		for (final ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			final IPath patchFilename = applyPatchFileReturn.getPatchFile();

			if (applyPatchFileReturn.getApplyMode().ordinal() < ApplyPatchMode.APPLIED.ordinal()) {
				final String message = String.format("Aplicando pacote %s (%d/%d).", patchFilename, patchCount++,
						applyPatchFilesReturn.size());
				ServerActivator.logStatus(IStatus.INFO, message);
				monitor.subTask(message);

				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}

				status = doApplyPatch(applyPatchFileReturn, patchCount, totalWork);
				applyPatchFileReturn.setApplyMode(ApplyPatchMode.APPLIED);
				if (status.isOK()) {
					applyPatchFileReturn.setPatchState(ApplyPatchState.OK);
					applyPatchFileReturn.setValidationMessage("");
				} else {
					applyPatchFileReturn.setApplyMode(ApplyPatchMode.NOT_APPLIED);
					applyPatchFileReturn.setPatchState(
							(status.getSeverity() == IStatus.ERROR ? ApplyPatchState.ERROR : ApplyPatchState.WARNING));
					applyPatchFileReturn.setValidationMessage(status.getMessage());
				}
			} else {
				ServerActivator.logStatus(IStatus.INFO, "Pacote ignorado %s (%d/%d). Motivo: %s",
						patchFilename.lastSegment(), patchCount++, applyPatchFilesReturn.size(),
						applyPatchFileReturn.getApplyMode().getText());
			}
		}
		//
		return status;
	}

	private IStatus doApplyPatch(final ApplyPatchFileReturn applyPatchFileReturn, final int index, final int total) {
		IStatus status = Status.OK_STATUS;
		//
		final IAppServerInfo appServer = applyPatchAttributes.getCurrentAppServer();
		final String environment = applyPatchAttributes.getEnvironment();
		final URI patchFilename = applyPatchFileReturn.getPatchFile().toFile().toURI();
		final String filename = applyPatchFileReturn.getPatchFile().lastSegment();

		final ApplyPatchReturn ar = appServer.applyPatch(environment, patchFilename,
				applyPatchFileReturn.getApplyMode() == ApplyPatchMode.APPLY_ALL, applyPatchFileReturn.isLocal());

		final String applyPatchMessage = ar.getReturnMessage();

		// finaliza o processo
		if (ar.isOperationOk()) {
			status = ServerActivator.logStatus(IStatus.OK, "Pacote %s aplicado (%d/%d)", filename, index, total);
		} else {
			status = ServerActivator.logStatus(IStatus.ERROR,
					"Erro na aplicação do pacote.\n\tPacote: %s (%d/%d)\n\tMotivo: %s", filename, index, total,
					applyPatchMessage);
		}
		//
		return status;
	}
}