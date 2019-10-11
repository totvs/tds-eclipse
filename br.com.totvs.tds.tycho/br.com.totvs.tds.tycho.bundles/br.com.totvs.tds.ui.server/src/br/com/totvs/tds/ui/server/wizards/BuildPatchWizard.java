package br.com.totvs.tds.ui.server.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;

import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.server.jobs.BuildPatchJob;
import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.server.jobs.CompileJob;
import br.com.totvs.tds.server.jobs.CompileMapData;
import br.com.totvs.tds.ui.sdk.SdkUIUtils;
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
		setWindowTitle(Messages.BuildPatchWizard_Patch);

		this.attributes = attributes;

		this.selectServerPage = new PatchSelectServerPage(attributes);
		this.comparasionPage = new PatchComparisonPage(attributes);
		this.workareaPage = new PatchWorkareaPage(attributes);
		this.rpoPage = new PatchRPOPage(attributes);
	}

	@Override
	public void addPages() {
		addPage(selectServerPage);
		addPage(comparasionPage);
		addPage(workareaPage);
		addPage(rpoPage);
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
						ServerActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
					}
				}
			}
		}

		return false;
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
		attributes.setSelection(selection);
	}

	@Override
	public boolean performFinish() {
		try {
			getContainer().run(true, true, new IRunnableWithProgress() {
				@Override
				public void run(final IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
					monitor.beginTask(Messages.BuildPatchWizard_Creating_patch, 10);
					ServerActivator.showStatus(IStatus.INFO, Messages.BuildPatchWizard_Creating_patch);
					BuildPatchProcessType process = attributes.getProcesso();

					if (attributes.isCompile() && (process.equals(BuildPatchProcessType.BY_WORKAREA))) {
						ServerActivator.showStatus(IStatus.INFO,
								Messages.BuildPatchWizard_Compiling_sources_patch_generation,
								attributes.getEnvironment());
						Map<String, CompileMapData> compileMap = new HashMap<String, CompileMapData>();

						attributes.getResourceFiles().stream().forEach(resource -> {
							try {
								SdkUIUtils.prepareToCompile(resource, compileMap);
							} catch (CoreException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						});

						final CompileOptions compileOptions = new CompileOptions();
						compileOptions.setRecompile(true);

						final Job job = new CompileJob(compileOptions, compileMap);
						job.schedule();
						job.join(); // TODO: Melhorar processo de forma a eliminar o "join".

						if (checkForCompilationErrors()) {
							ServerActivator.showStatus(IStatus.ERROR,
									Messages.BuildPatchWizard_Patch_generation_canceled_compilation_errors);
							return;
						}
					}
					List<String> environments = attributes.getEnvironmentsList();
					attributes.setPrefix(environments.size() > 1);

					ArrayList<BuildPatchJob> jobs = new ArrayList<BuildPatchJob>();
					for (String environment : environments) {
						attributes.setEnvironment(environment);
						// TODO: Criar dependencia de CompileJob
						BuildPatchJob patchBuilderJob = new BuildPatchJob(attributes);
						jobs.add(patchBuilderJob);
						patchBuilderJob.schedule();
					}

					// TODO: Melhorar processo de forma a eliminar o "join".
					boolean running = true;
					while (running) {
						running = false;
						for (BuildPatchJob buildPatchJob : jobs) {
							if (buildPatchJob.getState() == Job.RUNNING) {
								running = true;
								buildPatchJob.join(2000, monitor);
								break;
							}
						}

						if (running) {
							monitor.worked(1);
						}
					}
				}
			});
		} catch (OperationCanceledException e) {
			ServerUIActivator.logStatus(IStatus.CANCEL, Messages.BuildPatchWizard_Interrupt_process);
			return false;
		} catch (InterruptedException e) {
			ServerUIActivator.logStatus(IStatus.CANCEL, Messages.BuildPatchWizard_Interrupt_process);
			return false;
		} catch (InvocationTargetException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			return false;
		}

		return true;
	}

}
