package br.com.totvs.tds.server.jobs;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
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
import br.com.totvs.tds.server.model.RpoTypeElement;

/**
 * Job para construção de pacotes de atualização.
 */
public class BuildPatchJob extends Job {

	private static final String JOB_NAME = "patchBuilderJob"; //$NON-NLS-1$
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
		return (JOB_NAME + "." + this.attributes.getEnvironment()).equals(family); //$NON-NLS-1$
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		monitor.beginTask(Messages.BuildPatchJob_Update_package_generation, 2);
		monitor.setTaskName(getName());

		ServerActivator.logStatus(IStatus.WARNING, Messages.BuildPatchJob_Starting_update_package_generation);

		monitor.worked(1);
		monitor.subTask(Messages.BuildPatchJob_Validating_parameters);

		if (attributes.getProcesso().equals(BuildPatchProcessType.BY_WORKAREA) && (attributes.getVerifyFilesRPO())) {
			final List<String> resourceNames = attributes.getResources();

			ServerActivator.logStatus(IStatus.INFO, Messages.BuildPatchJob_Validating_existence_resource);
			final ArrayList<String> notHas = existFilesRPO(resourceNames);

			if (!notHas.isEmpty()) {
				String fontes = ""; //$NON-NLS-1$
				for (final String fonte : notHas) {
					fontes += fonte + "\n\t"; // $NON-NLS-2$ //$NON-NLS-1$
				}

				ServerActivator.logStatus(IStatus.ERROR, Messages.BuildPatchJob_Following_sources_not_found, fontes);

				return ServerActivator.logStatus(IStatus.ERROR, Messages.BuildPatchJob_Package_cannot_be_generated);
			}
		}

		IStatus ret = Status.CANCEL_STATUS;
		try {
			if (attributes.isValid()) {
				ServerActivator.logStatus(IStatus.INFO, Messages.BuildPatchJob_By_process,
						attributes.getProcesso().getLabel());
				ret = doRun(monitor);
			} else {
				ret = ServerActivator.showStatus(IStatus.ERROR, Messages.BuildPatchJob_Invalid_generation_attributes,
						attributes.getErrorMessage());
			}
		} catch (final Exception e) {
			ret = ServerActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
		}

		if (ret == Status.OK_STATUS) {
			ServerActivator.showStatus(IStatus.OK, Messages.BuildPatchJob_Update_package_generation_succeeded,
					attributes.getOutputFile());
		}

		return ret;
	}

	private IStatus doRun(final IProgressMonitor monitor) throws Exception {
		IStatus ret = Status.CANCEL_STATUS;
		final IAppServerInfo server = attributes.getServer();
		monitor.worked(2);

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final IServerManager serverManager = serviceLocator.getService(IServerManager.class);
		final String authorizationCode = serverManager.getAuthorizationKey().getAuthorizationCode();
		final java.nio.file.Path tempFolder = Files.createTempDirectory("tds"); //$NON-NLS-1$

		if (attributes.getProcesso() == BuildPatchProcessType.BY_COMPARISON) {
			final int resultPatchGenerate = lsService.patchGenerate(server.getToken(), authorizationCode,
					attributes.getEnvironment(), attributes.isLocal(), attributes.getFilename(), tempFolder.toString(),
					new String[0], attributes.getMasterPatch(), attributes.getPatchType().getPatchType());

			if (resultPatchGenerate != 0) {
				ret = ServerActivator.showStatus(IStatus.ERROR, Messages.BuildPatchJob_Server_returned_error,
						resultPatchGenerate);
			} else if (saveToLocalFile(tempFolder)) {
				ret = Status.OK_STATUS;
			} else {
				ret = ServerActivator.showStatus(IStatus.ERROR, Messages.BuildPatchJob_Unable_save_generated_package);
			}
		} else {
			String[] resources = null;

			if (!attributes.getResourcesFiles().isEmpty()) {
				final List<String> resourceList = attributes.getResourcesFiles().stream()
						.filter(resource -> canCompile(resource)).map(IFile::getFullPath).map(IPath::toString)
						.collect(Collectors.toList());
				resources = resourceList.toArray(new String[resourceList.size()]);
			} else {
				resources = attributes.getResources().toArray(new String[attributes.getResources().size()]);
			}
			final int resultPatchGenerate = lsService.patchGenerate(server.getToken(), authorizationCode,
					attributes.getEnvironment(), attributes.isLocal(), attributes.getFilename(), tempFolder.toString(),
					resources, attributes.getMasterPatch(), attributes.getPatchType().getPatchType());

			if (resultPatchGenerate != 0) {
				ret = ServerActivator.showStatus(IStatus.ERROR, Messages.BuildPatchJob_Server_returned_error,
						resultPatchGenerate);
			} else if (saveToLocalFile(tempFolder)) {
				ret = Status.OK_STATUS;
			} else {
				ret = ServerActivator.showStatus(IStatus.ERROR, Messages.BuildPatchJob_Unable_save_generated_package);
			}
		}

		return ret;
	}

	private boolean canCompile(final IFile resource) {
//		final IWrapperManager wm = WrapperManager.getInstance();
//
//		boolean result = ;
//
//		final IProject project = resource.getProject();
//			final IResourceWrapper wf = wm.getWrapper(file); // testar natureza do projeto
//
//			if (!wf.isIgnoreCompile()) {
//
//		return result ;
		return true;
	}

	/*
	 * Salva o arquivo gerado pelo servidor no local de destino.
	 */
	private boolean saveToLocalFile(final java.nio.file.Path tempFolder) {
		final File[] files = tempFolder.toFile().listFiles();

		if (files.length == 0) {
			return false;
		}

		final String[] parts = files[0].getName().split("\\.(?=[^\\.]+$)"); //$NON-NLS-1$
		String filename = attributes.getFilename().trim();
		filename = (filename.isEmpty()) ? files[0].getName()
				: filename + Messages.BuildPatchJob_19 + parts[parts.length - 1];
		if (attributes.isPrefix()) {
			filename = attributes.getEnvironment() + "_" + filename; //$NON-NLS-1$
		}

		final File patchFile = Paths.get(attributes.getPatchFilePath(), filename).toFile();

		if (patchFile.exists()) {
			patchFile.delete();
		}
		attributes.setOutputFile(patchFile.toPath());

		try {
			Files.move(files[0].toPath(), patchFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
		} catch (final IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, Messages.BuildPatchJob_Copy_from_to, files[0].toPath(),
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
				final LoadRpoMapJob loadMapjob = new LoadRpoMapJob(attributes.getServer(), attributes.getEnvironment(),
						true, RpoTypeElement.OBJECT);
				loadMapjob.schedule();
				loadMapjob.join();

				if (loadMapjob.getResult().isOK()) {
					resourcesRPO = loadMapjob.getList();
				}

			} catch (final Exception e) {
				e.printStackTrace();
			}
		}

		for (final String nameResourcePath : resourcePathNames) {
			if (nameResourcePath.endsWith(".ch") || nameResourcePath.endsWith(".CH")) { //$NON-NLS-1$//$NON-NLS-2$
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
