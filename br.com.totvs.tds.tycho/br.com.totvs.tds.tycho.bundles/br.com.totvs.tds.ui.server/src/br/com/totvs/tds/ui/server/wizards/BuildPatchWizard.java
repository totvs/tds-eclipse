package br.com.totvs.tds.ui.server.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.server.jobs.BuildPatchJob;
import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.wizards.patch.PatchComparisonPage;
import br.com.totvs.tds.ui.server.wizards.patch.PatchRPOPage;
import br.com.totvs.tds.ui.server.wizards.patch.PatchSelectServerPage;
import br.com.totvs.tds.ui.server.wizards.patch.PatchWorkareaPage;

/**
 * Wizard de geração de patchs.
 */
public class BuildPatchWizard extends Wizard implements IWorkbenchWizard {

	private BuildPatchAttributes attributes;

	private final PatchSelectServerPage selectServerPage;
	private final PatchComparisonPage comparasionPage;
	private final PatchWorkareaPage workareaPage;
	private final PatchRPOPage rpoPage;

	/**
	 * Wizard de geração de patchs.
	 */
	/**
	 * Wizard de geração de patchs.
	 */
	public BuildPatchWizard(final BuildPatchAttributes attributes) {
		super();

		setNeedsProgressMonitor(true);
		setWindowTitle("Pacote de Atualização");

		this.attributes = attributes;

		this.selectServerPage = new PatchSelectServerPage(attributes);
		this.comparasionPage = new PatchComparisonPage(attributes);
		this.workareaPage = new PatchWorkareaPage(attributes);
		this.rpoPage = new PatchRPOPage(attributes);
	}

	@Override
	public void addPages() {
		addPage(selectServerPage);
		addPage(getComparasionPage());
		addPage(getWorkareaPage());
		addPage(getRpoPage());
	}

	@Override
	public boolean canFinish() {
		boolean pageCompleted = selectServerPage.isPageComplete()
				&& (comparasionPage.isPageComplete() || workareaPage.isPageComplete() || rpoPage.isPageComplete());

		return pageCompleted;
	}

	/**
	 * Checks for compilation error and returns true it finds any.
	 *
	 * @return
	 */
	private boolean checkForCompilationErrors() {
		if (attributes.isCompile() && BuildPatchProcessType.BY_WORKAREA.equals(attributes.getProcesso())) {

			IMarker[] foundMarkers = null;
			List<IResource> fileList = new ArrayList<IResource>();

			for (IResource resource : fileList) {
				try {
					foundMarkers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);
				} catch (CoreException e) {
					e.printStackTrace();
				}
				if (foundMarkers.length > 0) {
					try {
						for (IMarker iMarker : foundMarkers) {
							Object severityNumber = iMarker.getAttribute(IMarker.SEVERITY);
							if (severityNumber != null && (Integer) (severityNumber) == IMarker.SEVERITY_ERROR) {
								return true;
							}
						}
					} catch (CoreException e) {
						ServerActivator.showStatus(IStatus.ERROR, "Pacote de Atualização", e.getMessage(), e);
					}
				}
			}
		}

		return false;
	}

	protected void doFinish(final IProgressMonitor monitor) throws Exception {
		try {
			monitor.beginTask("Criando pacote de Atualização.", 10);
			ServerActivator.showStatus(IStatus.INFO, "Pacote de Atualização", "Criando pacote de Atualização.");
			BuildPatchProcessType process = attributes.getProcesso();

			if (attributes.isCompile() && (process.equals(BuildPatchProcessType.BY_WORKAREA))) {
				// TODO: Melhorar colocando "family" e "rules". eliminado join
				ServerActivator.showStatus(IStatus.INFO, "Pacote de Atualização",
						"Compilando fontes na geração do pacote. Ambiente: %s", attributes.getEnvironment());

//					@SuppressWarnings("unchecked")
//					Object[] objectsList = ((List<Object>) attributes.getAdapter(ISelectedList.class)).toArray();
				//
//					procResourceList(fileList, objectsList);
				//
//					IAppServerConnector connector = attributes.getServer();
				//
//					ConfigResourceJob configJob = new ConfigResourceJob(connector.getCredencial());
//					configJob.setServer((IAppServerInfo) attributes.getServer().getServerInfo());
//					configJob.setEnvironments(attributes.getEnvironmentsList());
//					configJob.setResourceList(fileList);
//					configJob.setCheckSum(true);
//					configJob.setOutputLevel(outputLevel);
//					configJob.setOutputFile(outputFile);
//					configJob.setPriorVelocity(isPriorVelocity);
//					// XXX SOURCESTAMP: Projeto do Source Stamp interrompido por tempo indeterminado
//					// configJob.setFiredBySourceStamp(attributes.isFiredBySourceStamp());
				//
//					CompileResourcesJob job = new CompileResourcesJob(configJob);
//					job.schedule();
//					job.join();
			}

			if (checkForCompilationErrors()) {
				ServerActivator.showStatus(IStatus.ERROR, "Pacote de Atualização",
						"Geração do pacote de Atualização cancelado devido a erros de compilação. Verifique a Visão [Problemas].");
				return;
			}

			List<String> environments = attributes.getEnvironmentsList();
			attributes.setPrefix(environments.size() > 1);

			for (String environment : environments) {
				attributes.setEnvironment(environment);
				BuildPatchJob patchBuilderJob = new BuildPatchJob(this.attributes);
				patchBuilderJob.schedule();
			}
		} catch (Exception e) {
			ServerActivator.showStatus(IStatus.ERROR, "Pacote de Atualização", e.getLocalizedMessage(), e);
		}
	}

	public PatchComparisonPage getComparasionPage() {
		return comparasionPage;
	}

	public PatchRPOPage getRpoPage() {
		return rpoPage;
	}

	public PatchWorkareaPage getWorkareaPage() {
		return workareaPage;
	}

	@Override
	public void init(final IWorkbench workbench, final IStructuredSelection selection) {
	}

	@Override
	public boolean performFinish() {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(final IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(monitor);
				} catch (Exception e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};

		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			ServerUIActivator.logStatus(IStatus.CANCEL, "Pacote de Atualização", "Processo interronpido.");
			return false;
		} catch (InvocationTargetException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, "Pacote de Atualização", e.getMessage(), e);
			return false;
		}

		return true;
	}

	/*
	 * Filtra os arquivos para gerar o patch
	 */
	private void procResourceList(final List<IResource> fileList, final Object[] objects) {
		try {
			for (Object resource : objects) {
				if (resource instanceof IProject) {
					procResourceList(fileList, ((IProject) resource).members());
				} else if (resource instanceof IFolder) {
					procResourceList(fileList, ((IFolder) resource).members());
				} else if (resource instanceof IFile
						&& !"PDB".equalsIgnoreCase(((IFile) resource).getFileExtension())) { //$NON-NLS-1$
					fileList.add((IFile) resource);
				}
			}
		} catch (CoreException e) {
			ServerActivator.showStatus(IStatus.ERROR, "Pacote de Atualização",
					"Erro durante processamento de recursos.", e);
		}
	}
}
