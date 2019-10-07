package br.com.totvs.tds.ui.sdk.wizard;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.wrapper.IWorkspaceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.ProjectVO;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperUtil;

/**
 * Classe ProjectWizard.
 * 
 * @author leo.watanabe
 *
 */
public class ProjectWizard extends Wizard implements INewWizard {

	private ProjectWizardPage page;
	private ProjectVO projectVO;
	private ISelection selection;

	/**
	 * Constructor for ProjectWizard.
	 */
	public ProjectWizard() {
		super();
		
		projectVO = new ProjectVO();
		setNeedsProgressMonitor(true);
		setWindowTitle(Messages.ProjectWizard_Protheus_project_wizard);
		setHelpAvailable(false);
	}

	/**
	 * Adding the page to the wizard.
	 */
	@Override
	public void addPages() {
		page = new ProjectWizardPage(selection, this.projectVO);
		addPage(page);
	}

	/**
	 * This method is called when 'Finish' button is pressed in the wizard. We will create an operation and run it using
	 * wizard as execution context.
	 */
	@Override
	public boolean performFinish() {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(final IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(monitor);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
			SdkUIActivator.logStatus(IStatus.ERROR, Messages.ProjectWizard_Project, e.getMessage(), e);
			page.setErrorMessage(e.getMessage());
			return false;
		}
		
		return true;
	}

	/**
	 * Cria um novo projeto.
	 * 
	 */

	private void doFinish(final IProgressMonitor monitor) throws CoreException {
		monitor.beginTask(Messages.ProjectWizard_Creating_project, 1);
		IWorkspaceWrapper workWrapper = WrapperUtil.getWorkspaceWrapper(ResourcesPlugin.getWorkspace());
		workWrapper.createProject(projectVO, monitor);
		monitor.worked(1);
		monitor.setTaskName("TOTVS"); //$NON-NLS-1$
		monitor.worked(1);
	}

	/**
	 * We will accept the selection in the workbench to see if we can initialize from it.
	 * 
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	@Override
	public void init(final IWorkbench workbench, final IStructuredSelection selection) {
		this.selection = selection;
	}

}