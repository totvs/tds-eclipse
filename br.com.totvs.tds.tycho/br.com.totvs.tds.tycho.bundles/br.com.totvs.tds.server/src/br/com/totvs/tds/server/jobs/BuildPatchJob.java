package br.com.totvs.tds.server.jobs;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IServerManager;

/**
 * Job para construção de pacotes de atualização.
 */
public class BuildPatchJob extends Job {

	private static final String JOB_NAME = "patchBuilderJob";
	private BuildPatchAttributes attributes;

	/**
	 * Job para construção de patchs.
	 */
	public BuildPatchJob(final BuildPatchAttributes attributes) {
		super(JOB_NAME);
		setUser(true);

		this.attributes = attributes.clone();
	}

	@Override
	public boolean belongsTo(final Object family) {
		return (JOB_NAME + "." + this.attributes.getEnvironment()).equals(family);
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		monitor.beginTask("Geração de pacote de atualização", 2);
		monitor.setTaskName(getName());

		ServerActivator.logStatus(IStatus.WARNING, "Geração de Pacote", "Iniciando Geração de Pacote de Atualização");

		monitor.worked(1);
		monitor.subTask("Validando parâmetros");

		if (attributes.getProcesso().equals(BuildPatchProcessType.BY_WORKAREA) && (attributes.getVerifyFilesRPO())) {
			final List<String> resourceNames = attributes.getResources();

			ServerActivator.logStatus(IStatus.INFO, "Geração de Pacote",
					"Validando a existência do(s) recurso(s) no RPO");
			final ArrayList<String> notHas = existFilesRPO(resourceNames);

			if (!notHas.isEmpty()) {
				String fontes = "";
				for (final String fonte : notHas) {
					fontes += fonte + "\n\t"; // $NON-NLS-2$
				}

				ServerActivator.logStatus(IStatus.ERROR, "Geração de Pacote",
						"Os fontes a seguir não foram localizados no RPO:\n\t%s", fontes);

				return ServerActivator.logStatus(IStatus.ERROR, "Geração de Pacote",
						"O pacote não pode ser gerado. Detalhes no log.");
			}
		}

		IStatus ret = Status.CANCEL_STATUS;
		try {
			if (attributes.isValid()) {
				ServerActivator.logStatus(IStatus.INFO, "Geração de Pacote", "Pelo processo: %s",
						attributes.getProcesso().getLabel());
				ret = doRun(monitor);
			} else {
				ret = ServerActivator.showStatus(IStatus.ERROR, "Geração de Pacote",
						"Atributos de geração inválidos.\n\t%s", attributes.getErrorMessage());
			}
		} catch (final Exception e) {
			ret = ServerActivator.showStatus(IStatus.ERROR, "Geração de Pacote", e.getMessage(), e);
		}

		if (ret == Status.OK_STATUS) {
			ServerActivator.showStatus(IStatus.OK, "Geração de Pacote",
					"Geração do pacote de atualização efetuada com sucesso.\n\tArquivo: %s",
					attributes.getOutputFile());
		}

		return ret;
	}

	private IStatus doRun(final IProgressMonitor monitor) throws Exception {
		IStatus ret = Status.CANCEL_STATUS;
		final IAppServerInfo server = attributes.getServer();
		monitor.worked(2);

		if (attributes.getProcesso() == BuildPatchProcessType.BY_COMPARISON) {
			// PATCH generatePatchByComparison incluir aqui tratamento para renomear o
			// arquivo criado pelo
			// servidor
			// o server está criando o arquivo direto na pasta selecionada
			ret = generatePatchByComparison(monitor);
		} else {
			final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
			final IServerManager serverManager = serviceLocator.getService(IServerManager.class);
			final String authorizationCode = serverManager.getAuthorizationKey().getAuthorizationCode();
			final java.nio.file.Path tempFolder = Files.createTempDirectory("tds");

			final int resultPatchGenerate = lsService.patchGenerate(server.getToken(), authorizationCode,
					attributes.getEnvironment(), attributes.isLocal(), attributes.getFilename(), tempFolder.toString(),
					attributes.getResources().toArray(new String[attributes.getResources().size()]),
					attributes.getMasterPatch(), attributes.getPatchType().getPatchType());

			if (resultPatchGenerate != 0) {
				ret = ServerActivator.showStatus(IStatus.ERROR, "Geração de Pacote",
						"Servidor retornou com erro. Código: %d", resultPatchGenerate);
			} else if (saveToLocalFile(tempFolder)) {
				ret = Status.OK_STATUS;
			} else {
				ret = ServerActivator.showStatus(IStatus.ERROR, "Geração de Pacote",
						"Não foi possível salvar o pacote gerado. Veja log para detalhes.");
			}
		}

		return ret;
	}

	private IStatus generatePatchByComparison(final IProgressMonitor monitor) throws Exception {
		monitor.subTask("Por comparação");
		final IAppServerInfo server = attributes.getServer();
		final String patchFilePath = attributes.getPatchFilePath();
		final String masterPatch = attributes.getMasterPatch();

		final PatchType patchType = attributes.getPatchType();
		final String currentEnvironment = attributes.getEnvironment();

		if (attributes.isLocal()) {
			final File tempFolder = new File(
					attributes.getPatchFilePath() + File.separator + Long.toHexString(new Date().getTime()));
			tempFolder.mkdir();

//			serverReturn = server.generateLocalCompPatch(currentEnvironment, masterPatch, tempFolder.getAbsolutePath(),
//					patchType.getPatchType(), "");
//			if (serverReturn.isOperationOk()) {
//				this.saveLocalFile(patchType, tempFolder.getAbsolutePath());
//			}
			tempFolder.delete();
		} else {
//			serverReturn = server.generateCompPatch(currentEnvironment, masterPatch, patchFilePath,
//					patchType.getPatchType(), "");
		}

		final IStatus status = null;
//		if (serverReturn.isOperationOk()) {
//			status = Status.OK_STATUS;
//		} else {
//			throw new TdsException(serverReturn.getReturnMessage());
//		}

		return status;
	}

	/*
	 * Salva o arquivo gerado pelo servidor no local de destino.
	 */
	private boolean saveToLocalFile(final java.nio.file.Path tempFolder) {
		final File[] files = tempFolder.toFile().listFiles();

		if (files.length == 0) {
			return false;
		}

		final String[] parts = files[0].getName().split("\\.(?=[^\\.]+$)");
		String filename = attributes.getFilename().trim();
		filename = (filename.isEmpty()) ? files[0].getName() : filename + "." + parts[parts.length - 1];
		if (attributes.isPrefix()) {
			filename = attributes.getEnvironment() + "_" + filename;
		}

		final File patchFile = Paths.get(attributes.getPatchFilePath(), filename).toFile();

		if (patchFile.exists()) {
			patchFile.delete();
		}
		attributes.setOutputFile(patchFile.toPath());

		try {
			Files.move(files[0].toPath(), patchFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
		} catch (final IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Geração de Pacote", "Cópia de [%s] para [%s].", files[0].toPath(),
					patchFile.toPath(), e);
		}

		return true;
	}

	private ArrayList<String> existFilesRPO(final List<String> resourcePathNames) {
		final IAppServerInfo connector = attributes.getServer();
		final String environment = attributes.getEnvironment();

		List<IRpoElement> resourcesRPO = Collections.emptyList();
		final ArrayList<String> nonexistent = new ArrayList<>();

		if ((connector != null) && (environment != null)) {
			try {
				final LoadRpoMapJob loadMapjob = new LoadRpoMapJob("Carregando RPO", attributes.getServer(),
						attributes.getEnvironment(), true);
				loadMapjob.schedule();
				loadMapjob.join();

				if (loadMapjob.getResult().isOK()) {
					resourcesRPO = loadMapjob.getRpoMap();
				}

			} catch (final Exception e) {
				e.printStackTrace();
			}
		}

		for (final String nameResourcePath : resourcePathNames) {
			if (nameResourcePath.endsWith(".ch") || nameResourcePath.endsWith(".CH")) { //$NON-NLS-2$
				continue;
			}

			final boolean exist = resourcesRPO.stream().filter(e -> e.getName().equalsIgnoreCase(nameResourcePath))
					.count() > 0;
			if (!exist) {
				nonexistent.add(nameResourcePath);
			}
		}

		return nonexistent;
	}

	public void setAttributes(final BuildPatchAttributes attributes) {
		this.attributes = attributes;
	}

}
