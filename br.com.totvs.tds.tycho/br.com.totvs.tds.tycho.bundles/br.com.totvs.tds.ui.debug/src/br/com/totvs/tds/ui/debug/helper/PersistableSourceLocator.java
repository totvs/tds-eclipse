package br.com.totvs.tds.ui.debug.helper;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.lsp4e.debug.debugmodel.DSPStackFrame;

import br.com.totvs.tds.ui.debug.DebugUIActivator;

/**
 * 
 * @author acandido
 *
 */

@SuppressWarnings("restriction")
public class PersistableSourceLocator extends AbstractSourceLookupDirector {

	public static final String ID = "br.com.totvs.tds.ui.debug.sourceLocator";
	private static final IProgressMonitor NULL_MONITOR = new NullProgressMonitor();;

	@Override
	public void initializeParticipants() {
		addParticipants(new ISourceLookupParticipant[] { new TdsSourceLookupParticipant() });
	}

	@Override
	public Object getSourceElement(final Object element) {
		Object ret = null;

		try {
			ISourceContainer[] csc = getSourcePathComputer().computeSourceContainers(getLaunchConfiguration(),
					NULL_MONITOR);
			for (ISourceContainer sc : csc) {
				ret = searchElement(sc, (DSPStackFrame) element);
				if (ret != null) {
					break;
				}
			}
		} catch (CoreException e) {
			DebugUIActivator.logStatus(IStatus.ERROR, "Deburador", e.getMessage(), e);
		}

		if (ret == null) {
			ret = super.getSourceElement(element);
		}
		//
		return ret;
	}

	private Object searchElement(ISourceContainer sourceContainer, DSPStackFrame element) {
		Object result = null;
		
		if (sourceContainer instanceof ProjectSourceContainer) {
			ProjectSourceContainer psc = (ProjectSourceContainer) sourceContainer;
			IProject project = psc.getProject();
			IPath projectLocation = project.getLocation();
			IPath sourceLocation = Path.fromPortableString(element.getSourceName());
			sourceLocation = sourceLocation.makeRelativeTo(projectLocation);
			result = project.getFile(sourceLocation);

			if (result == null) {
				System.out.println(result);
			}
		}
		
		return result;
	}

}
