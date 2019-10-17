package br.com.totvs.tds.server.jobs.applyPatch;

import java.util.List;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;

/**
 * Job de aplicação de patchs.
 */
public final class ValidatePatchJob extends Job {

	private final ApplyPatchAttributes applyPatchAttributes;
	private ApplyPatchFileReturn target;

	/**
	 * aplicação de patch.
	 *
	 * @param name nome do job
	 */
	public ValidatePatchJob(final ApplyPatchAttributes applyPatchAttributes) {
		super("Validação");

		this.applyPatchAttributes = applyPatchAttributes;
		setUser(true);
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		IStatus status = Status.CANCEL_STATUS;
		final List<ApplyPatchFileReturn> applyPatchFilesReturn = filter(applyPatchAttributes.getApplyPatchFilesReturn(),
				this.target);
		int patchCount = 1;
		final int totalWork = (applyPatchFilesReturn.size());
		monitor.beginTask("Validação de Pacote de Atualização", totalWork);

		for (final ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			final IPath patchFilename = applyPatchFileReturn.getPatchFile();

			if (applyPatchFileReturn.getApplyMode().ordinal() <= ApplyPatchMode.VALIDATE_PATCH.ordinal()) {
				final String message = String.format("Validando pacote %s (%d/%d).", patchFilename, patchCount,
						applyPatchFilesReturn.size());
				ServerActivator.logStatus(IStatus.INFO, message);
				monitor.subTask(message);

				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}

				status = doValidatePatch(applyPatchFileReturn, patchCount, totalWork);
				applyPatchFileReturn.setApplyMode(ApplyPatchMode.VALIDATE_PATCH);
				if (status.isOK()) {
					applyPatchFileReturn.setPatchState(ApplyPatchState.OK);
					applyPatchFileReturn.setValidationMessage("");
				} else {
					applyPatchFileReturn.setPatchState(
							(status.getSeverity() == IStatus.ERROR ? ApplyPatchState.ERROR : ApplyPatchState.WARNING));
					applyPatchFileReturn.setValidationMessage(status.getMessage());
				}
			} else {
				ServerActivator.logStatus(IStatus.INFO, "Pacote ignorado %s (%d/%d). Motivo: %s",
						patchFilename.lastSegment(), patchCount, applyPatchFilesReturn.size(),
						applyPatchFileReturn.getApplyMode().getText());
			}

			patchCount++;
		}
		//
		return status;
	}

	private List<ApplyPatchFileReturn> filter(final List<ApplyPatchFileReturn> applyPatchFilesReturn,
			final ApplyPatchFileReturn target) {
		if (target != null) {
			return applyPatchFilesReturn.stream().filter(p -> p.equals(target)).collect(Collectors.toList());
		}

		return applyPatchFilesReturn;
	}

	private IStatus doValidatePatch(final ApplyPatchFileReturn applyPatchFileReturn, final int index, final int total) {
		IStatus status = Status.OK_STATUS;
		//
		final IAppServerInfo appServer = applyPatchAttributes.getCurrentAppServer();
		final String environment = applyPatchAttributes.getEnvironment();
		final IPath patchFilename = applyPatchFileReturn.getPatchFile();

		final ValidationPatchReturn vpr = appServer.validPatch(environment, patchFilename.toFile().toPath().toUri(),
				applyPatchFileReturn.isLocal());

		// finaliza o processo
		if (vpr.isOperationOk()) {
			applyPatchFileReturn.setPatchState(ApplyPatchState.OK);
			status = ServerActivator.logStatus(IStatus.OK, "Pacote %s validado (%d/%d)", patchFilename.lastSegment(),
					index, total);
		} else {
			status = ServerActivator.logStatus(IStatus.ERROR,
					"Erro na validação do pacote.\n\tPacote: %s (%d/%d)\n\tMotivo: %s", patchFilename.lastSegment(),
					index, total, vpr.getReturnMessage());

			processValidationMessage(vpr, applyPatchFileReturn);
		}
		//
		return status;
	}

	private void processValidationMessage(final ValidationPatchReturn vpr,
			final ApplyPatchFileReturn applyPatchFileReturn) {
		final String validationMessage = vpr.getReturnMessage();

		if ((validationMessage != null) && !validationMessage.isEmpty()) {
			applyPatchFileReturn.setPatchState(ApplyPatchState.ERROR);
			applyPatchFileReturn.setValidationMessage(validationMessage);
		} else if ((applyPatchFileReturn.getOldPrograms() != null) && applyPatchFileReturn.getOldPrograms().isEmpty()) {
			final StringJoiner sb = new StringJoiner("\n\t", "", "\n");
			sb.add(String.format("Validação efetuada com restrições.\n\t%s",
					ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS));
			sb.add(String.format("Arquivo: %s", applyPatchFileReturn.getPatchFile()));
			sb.add(String.format("Arquivo: %s", applyPatchFileReturn.getPatchFile()));
			sb.add(String.format("%-12.12s %-21.21s %-21.21s", "Fonte", "Pacote", "RPO"));
			for (final String[] program : applyPatchFileReturn.getOldPrograms()) {
				sb.add(String.format("%-12.12s %-21.21s %-21.21s", program[0], program[1], program[2]));
			}
			ServerActivator.logStatus(IStatus.INFO, sb.toString());

			applyPatchFileReturn.setPatchState(ApplyPatchState.WARNING);
			applyPatchFileReturn.setValidationMessage(ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS);
			applyPatchFileReturn.setOldPrograms(applyPatchFileReturn.getOldPrograms());
		}
	}

	public void setTarget(final ApplyPatchFileReturn target) {
		this.target = target;
	}
}
