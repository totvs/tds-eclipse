package br.com.totvs.tds.server.jobs.applyPatch;

import java.net.URI;
import java.nio.file.Paths;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn.MessageType;

/**
 * Job de aplicação de pacotes.
 */
public class _ProcessPatchJob extends Job {

	private final ApplyPatchAttributes applyPatchAttributes;
	private IAppServerInfo appServer;

	/**
	 * Aplicação de pacotes patch.
	 *
	 * @param name nome do job
	 */
	public _ProcessPatchJob(final String name, final ApplyPatchAttributes applyPatchAttributes) {
		super(name);

		this.applyPatchAttributes = applyPatchAttributes;
		setUser(true);
	}

	/**
	 * @return the applyPatchAttributes
	 */
	public ApplyPatchAttributes getApplyPatchAttributes() {
		return applyPatchAttributes;
	}

	private void logInformation(final String message) {
		ServerActivator.logStatus(IStatus.INFO, message);
	}

	private void logError(final String message) {
		ServerActivator.logStatus(IStatus.ERROR, message);
	}

	protected IStatus doValidatePatchs(final IProgressMonitor monitor) throws Exception {
		IStatus status = Status.OK_STATUS;
		int patchCount = 1;
		java.nio.file.Path serverPatch = null;
		IPath patchFilename;
		final String environment = applyPatchAttributes.getEnvironment();
		final List<ApplyPatchFileReturn> applyPatchFilesReturn = applyPatchAttributes.getApplyPatchFilesReturn();
		//
		monitor.beginTask("Validando pacotes de atualização", applyPatchFilesReturn.size());
		//

		for (final ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			patchFilename = applyPatchFileReturn.getPatchFile();
			logInformation(String.format(". Processando pacote %s (%d/%d)", patchFilename, patchCount++,
					applyPatchFilesReturn.size()));

			if (!applyPatchFileReturn.isLocal()) {
				serverPatch = Paths.get(patchFilename.toOSString()); // getSourceFile();
			}

			logInformation("Validando o pacote...");

			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			final ValidationPatchReturn vpr = appServer.validPatch(environment, serverPatch.toUri(),
					applyPatchFileReturn.isLocal());
			applyPatchFileReturn.setValidationMessage(vpr.getReturnMessage());
			applyPatchFileReturn.setOldPrograms(vpr.getOldPrograms());
			applyPatchFileReturn.getApplyMode();

			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			status = processValidationMessage(patchFilename.toOSString(), applyPatchFileReturn);
		}

		return status;
	}

	protected IStatus doApplyPatches(final IProgressMonitor monitor) throws Exception {
		IStatus status = Status.OK_STATUS;
		//

		monitor.subTask("Aplicando pacotes de atualização");
		//
		int patchCount = 1;
		appServer = applyPatchAttributes.getCurrentAppServer();
		final String environment = applyPatchAttributes.getEnvironment();
		final List<ApplyPatchFileReturn> applyPatchFilesReturn = applyPatchAttributes.getApplyPatchFilesReturn();
		final int total = applyPatchFilesReturn.size();
		for (final ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			final IPath patchFilename = applyPatchFileReturn.getPatchFile();
			final String originalFile = applyPatchFileReturn.getOriginalFile();

			final int count = patchCount++;
			logInformation(String.format("Validando pacote %s (%d/%d)", originalFile, count, total));

			if (applyPatchFileReturn.getMessageType().equals(MessageType.DUPLICATE_FILE)) {
				logInformation(String.format("Pacote ignorado por duplicidade.\n\tPacote: %s", patchFilename));
				continue;
			}

			status = validateApplyPatchProcess(monitor, environment, applyPatchFileReturn);

			// aplica o pacote
			if (!status.isOK()) {
				logError(String.format(". Pacote inválido %s (%d/%d)", originalFile, count, total));
				return status;
			}

			monitor.subTask("Aplicando pacote de atualização");
			logInformation(String.format(". Aplicando pacote %s (%d/%d)", originalFile, count, total));

			final ApplyPatchReturn ar = appServer.applyPatch(environment, patchFilename.toFile().toURI(),
					applyPatchFileReturn.getApplyMode() == ApplyPatchMode.APPLY_ALL, applyPatchFileReturn.isLocal());

			final String applyPatchMessage = ar.getReturnMessage();
			applyPatchFileReturn.setValidationMessage(applyPatchMessage);

			// finaliza o processo
			if (ar.isOperationOk()) {
				ServerActivator.logStatus(IStatus.OK,
						String.format(". Pacote %s aplicado (%d/%d)", originalFile, count, total));
			} else {
				status = ServerActivator.logStatus(IStatus.ERROR,
						". Erro na aplicação do pacote.\n\tPacote: %s (%d/%d)\n\tMotivo: %s", originalFile, count,
						total, applyPatchMessage);
				applyPatchFileReturn.setMessageType(MessageType.ERROR);

				return status;
			}
		}

		//
		return status;
	}

	private IStatus validateApplyPatchProcess(final IProgressMonitor monitor, final String environment,
			final ApplyPatchFileReturn applyPatchFileReturn) throws Exception {
		IStatus status = Status.OK_STATUS;
		if (monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		final ApplyPatchMode originalApplyMode = applyPatchFileReturn.getApplyMode();
		// valida o pacote no server
		final ValidationPatchReturn vpr = validatePackageInServer(monitor, applyPatchFileReturn, environment);
		final ApplyPatchMode applyMode = vpr.getApplyPatchMode();

		String validationMessage = "";

		// Caso o modo seja Apply All significa que o usuario ja aceitou a aplicaçãode
		// patch com problemas
		if (!ApplyPatchMode.APPLY_ALL.equals(originalApplyMode)) {

			if ((validationMessage != null) && !validationMessage.trim().isEmpty()) {
				applyPatchFileReturn.setValidationMessage(validationMessage);
			} else {
				validationMessage = verifyOlderSourcesInPackage(applyMode, vpr, applyPatchFileReturn);
			}
			// Falha durante processamento do pacote
			if ((validationMessage != null) && !validationMessage.isEmpty()) {
				status = getCancelStatus(applyPatchFileReturn, validationMessage, monitor);
			}
		}
		//
		return status;
	}

	private IStatus getCancelStatus(final ApplyPatchFileReturn applyPatchFileReturn, final String validationMessage,
			final IProgressMonitor monitor) {
		logInformation(String.format("Falha durante processamento do pacote.\n\tPacote: %s\n\tMotivo:: %s",
				applyPatchFileReturn.getPatchFile(), validationMessage));
		applyPatchFileReturn.setMessageType(MessageType.WARNING);
		applyPatchFileReturn.setApplyMode(ApplyPatchMode.VALIDATE_ERROR);

		return ServerActivator.createStatus(IStatus.CANCEL, validationMessage);
	}

	private String verifyOlderSourcesInPackage(final ApplyPatchMode applyMode, final ValidationPatchReturn vpr,
			final ApplyPatchFileReturn applyPatchFileReturn) {
		String validationMessage = null;
		if ((applyMode != null) && applyMode.equals(ApplyPatchMode.VALIDATE_PATCH) && (vpr.getOldPrograms() != null)
				&& !vpr.getOldPrograms().isEmpty()) {
			final String warningMessage = ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS;
			applyPatchFileReturn.setValidationMessage(warningMessage);
			// aborta aplicaçãode pacote de atualização devido
			// a algum fonte ser antigo no pacote corrente
			validationMessage = "Erro ao validar o pacote de atualização";
		}
		return validationMessage;
	}

	private ValidationPatchReturn validatePackageInServer(final IProgressMonitor monitor,
			final ApplyPatchFileReturn applyPatchFileReturn, final String environment) throws Exception {
		monitor.subTask("Validando pacote de atualização");

		final URI uri = new Path(applyPatchFileReturn.getPatchFullPath()).toFile().toURI();
		final ValidationPatchReturn vpr = appServer.validPatch(environment, uri, applyPatchFileReturn.isLocal());
		applyPatchFileReturn.setValidationMessage(vpr.getReturnMessage());
		applyPatchFileReturn.setOldPrograms(vpr.getOldPrograms());
		if ((vpr.getOldPrograms() == null) || vpr.getOldPrograms().isEmpty()) {
			applyPatchFileReturn.setApplyMode(ApplyPatchMode.APPLY_ALL);
		}
		final ApplyPatchMode applyMode = applyPatchFileReturn.getApplyMode();
		vpr.setApplyPatchMode(applyMode);
		return vpr;
	}

	private IStatus processValidationMessage(final String patchFilename,
			final ApplyPatchFileReturn applyPatchFileReturn/*
															 * , final String signatureMessage
															 */) {
		final String validationMessage = applyPatchFileReturn.getValidationMessage();
		final ApplyPatchMode applyMode = applyPatchFileReturn.getApplyMode();
		if ((applyMode != null) && (applyMode != ApplyPatchMode.VALIDATE_PATCH)) {
			if ((validationMessage == null) || validationMessage.isEmpty()) {
				logInformation(String.format("Procedimento executado com sucesso.\n\tPacote: %s", patchFilename));
				applyPatchFileReturn.setMessageType(MessageType.OK);
			} else {
				final String msg = (String.format("Falha durante processamento do pacote.\n\tPacote: %s\n\tMensagem %s",
						patchFilename, applyPatchFileReturn.getValidationMessage()));
				applyPatchFileReturn.setMessageType(MessageType.ERROR);
				return ServerActivator.createStatus(IStatus.CANCEL, msg);
			}
		} else if ((validationMessage != null) && !validationMessage.isEmpty()) {
			logInformation(validationMessage);
			applyPatchFileReturn.setMessageType(MessageType.ERROR);
		} else if ((applyPatchFileReturn.getOldPrograms() != null)
				&& !applyPatchFileReturn.getOldPrograms().isEmpty()) {
			logInformation("Validação efetuada, porém há restrições.\\n"
					+ ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS);
			logInformation(String.format("Arquivo: %s", patchFilename));
			logInformation(String.format("%-12.12s %-21.21s %-21.21s", "Fonte", "Pacote", "RPO"));
			for (final String[] program : applyPatchFileReturn.getOldPrograms()) {
				logInformation(String.format("%-12.12s %-21.21s %-21.21s", program[0], program[1], program[2]));
			}
			applyPatchFileReturn.setValidationMessage(ApplyPatchFileReturn.RPO_CONTAINS_MORE_RECENT_OBJECTS);
			applyPatchFileReturn.setMessageType(MessageType.WARNING);
			applyPatchFileReturn.setApplyMode(ApplyPatchMode.VALIDATE_PATCH);
		} else {
			applyPatchFileReturn.setMessageType(MessageType.OK);
		}
		return Status.OK_STATUS;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		// TODO Auto-generated method stub
		return null;
	}

}
