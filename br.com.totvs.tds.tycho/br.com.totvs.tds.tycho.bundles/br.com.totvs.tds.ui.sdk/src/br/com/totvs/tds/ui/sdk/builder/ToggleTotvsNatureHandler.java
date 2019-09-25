package br.com.totvs.tds.ui.sdk.builder;

import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

public class ToggleTotvsNatureHandler extends AbstractHandler {

	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelection(event);
		//
		if (selection instanceof IStructuredSelection) {
			for (Iterator<?> it = ((IStructuredSelection) selection).iterator(); it
					.hasNext();) {
				Object element = it.next();
				IProject project = null;
				if (element instanceof IProject) {
					project = (IProject) element;
				} else if (element instanceof IAdaptable) {
					project = ((IAdaptable) element).getAdapter(IProject.class);
				}
				if (project != null) {
					try {
						IProjectWrapper wrapper = WrapperManager.getInstance().getWrapper(project);
						wrapper.toggleNature(TotvsNature.NATURE_ID);
					} catch (CoreException e) {
						SdkUIActivator.logStatus(IStatus.ERROR, "Interno", e.getMessage(), e);
						throw new ExecutionException(e.getMessage(),e);
					}
				}
			}
		}

		return null;
	}
} 
