package br.com.totvs.tds.server.jobs.applyPatch;

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn.MessageType;

/**
 * Job de aplicação de pacotes.
 */
public class ValidatePatchJob extends Job {

	private final ApplyPatchAttributes applyPatchAttributes;

	/**
	 * Aplicação de pacotes patch.
	 *
	 * @param name nome do job
	 */
	public ValidatePatchJob(final ApplyPatchAttributes applyPatchAttributes) {
		super("validate");

		this.applyPatchAttributes = applyPatchAttributes;
		setUser(true);
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		IStatus status = Status.OK_STATUS;
		int patchCount = 1;
		final IAppServerInfo appServer = applyPatchAttributes.getCurrentAppServer();
		final String environment = applyPatchAttributes.getEnvironment();
		final List<ApplyPatchFileReturn> applyPatchFilesReturn = applyPatchAttributes.getApplyPatchFilesReturn();
		//
		//
		for (final ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			final IPath patchFilename = applyPatchFileReturn.getPatchFile();
			final String message = String.format("Validando pacote %s (%d/%d)", patchFilename, patchCount++,
					applyPatchFilesReturn.size());

			monitor.subTask(message);
			ServerActivator.logStatus(IStatus.INFO, message);

			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			final ValidationPatchReturn vpr = appServer.validPatch(environment, patchFilename.toFile().toPath().toUri(),
					applyPatchFileReturn.isLocal());
			if (!vpr.isOperationOk()) {
				applyPatchFileReturn.setMessageType(ApplyPatchFileReturn.MessageType.ERROR);
				applyPatchFileReturn.setValidationMessage(vpr.getReturnMessage());
				applyPatchFileReturn.setOldPrograms(vpr.getOldPrograms());
			}
			monitor.worked(1);
			status = processValidationMessage(patchFilename.toOSString(), applyPatchFileReturn);
		}

		return status;
	}

	private IStatus processValidationMessage(final String patchFilename,
			final ApplyPatchFileReturn applyPatchFileReturn) {
		final String validationMessage = applyPatchFileReturn.getValidationMessage();
		final ApplyPatchMode applyMode = applyPatchFileReturn.getApplyMode();

		if ((ApplyPatchMode.VALIDATE_PATCH.equals(applyMode))) {
			if ((validationMessage == null) || validationMessage.isEmpty()) {
				ServerActivator.logStatus(IStatus.INFO, "Validação efetuada com sucesso.\n\tPacote: %s", patchFilename);
			} else {
				final String msg = (String.format("Falha durante processamento do pacote.\n\tPacote: %s\n\tMensagem %s",
						patchFilename, applyPatchFileReturn.getValidationMessage()));
				applyPatchFileReturn.setMessageType(MessageType.ERROR);
				return ServerActivator.createStatus(IStatus.CANCEL, msg);
			}
//		} else if ((validationMessage != null) && !validationMessage.isEmpty()) {
//			ServerActivator.logStatus(IStatus.ERROR, validationMessage);
//			applyPatchFileReturn.setMessageType(MessageType.ERROR);
//		} else if ((applyPatchFileReturn.getOldPrograms() != null)
//				&& !applyPatchFileReturn.getOldPrograms().isEmpty()) {
//			ServerActivator.logStatus(IStatus.INFO, "Validação efetuada, porém há restrições.\n\t%s",
//					ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS);
//			final StringJoiner sb = new StringJoiner("\n\t", "\n\t", "\n");
//			sb.add(String.format("Arquivo: %s", patchFilename));
//			sb.add(String.format("%-12.12s %-21.21s %-21.21s", "Fonte", "Pacote", "RPO"));
//			for (final String[] program : applyPatchFileReturn.getOldPrograms()) {
//				sb.add(String.format("%-12.12s %-21.21s %-21.21s", program[0], program[1], program[2]));
//			}
//			ServerActivator.logStatus(IStatus.INFO, sb.toString());
//
//			applyPatchFileReturn.setValidationMessage(ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS);
//			applyPatchFileReturn.setMessageType(MessageType.WARNING);
//			applyPatchFileReturn.setApplyMode(ApplyPatchMode.VALIDATE_PATCH);
		}

		return Status.OK_STATUS;
	}

}
