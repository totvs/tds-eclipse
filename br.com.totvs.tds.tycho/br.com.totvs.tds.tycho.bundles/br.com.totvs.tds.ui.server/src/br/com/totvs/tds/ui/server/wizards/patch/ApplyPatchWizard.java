package br.com.totvs.tds.ui.server.wizards.patch;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchAttributes;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchJob;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Wizard principal para aplicação de patch.
 *
 */
public class ApplyPatchWizard extends Wizard implements IWorkbenchWizard {

	private ApplyPatchAttributes attributes = new ApplyPatchAttributes();

	private ApplyPatchPage applyPatchPage;

	/**
	 * Construtor para aplicação de patch passando os atributos de aplicação.
	 *
	 * @param attributes atributos de aplicação de patch
	 * @see ToolsApplPatchAttributes
	 */
	public ApplyPatchWizard(final ApplyPatchAttributes attributes) {
		setWindowTitle("Aplicação de Pacote de Atualização");

		setNeedsProgressMonitor(true);
		this.attributes = attributes;
	}

	@Override
	public void addPages() {
		applyPatchPage = new ApplyPatchPage(attributes);

		addPage(applyPatchPage);
	}

	@Override
	public void init(final IWorkbench workbench, final IStructuredSelection selection) {
		System.out.println("ApplyPatchWizard.init()");
	}

	@Override
	public boolean performFinish() {
		boolean performFinish = false;
		//
		final ApplyPatchJob applyJob = new ApplyPatchJob(attributes); // $NON-NLS-1$

		IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(final IProgressMonitor monitor) throws InvocationTargetException {
				try {
					// coloca o Job na fila
					applyJob.schedule();
					// aguarda a conclusão do Job travando a UI
					applyJob.join();
				} catch (InterruptedException e) {
					ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
					;
				}
			}
		};

		try {
			getContainer().run(true, false, op);
		} catch (InvocationTargetException | InterruptedException e) {
			ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
		}

		// verifica o estado da execução do Job
		IStatus result = applyJob.getResult();
		if (result == null || !result.isOK()) {
			// apresenta dialogo das op��es de a��o para os erros de valida��es encotrados
			applyPatchPage.showValidationErrors();
		} else {
			// assistente finalizado com sucesso
			applyPatchPage.clearTemporaryPatches();
			performFinish = true;
		}
		//
		return performFinish;
	}

}