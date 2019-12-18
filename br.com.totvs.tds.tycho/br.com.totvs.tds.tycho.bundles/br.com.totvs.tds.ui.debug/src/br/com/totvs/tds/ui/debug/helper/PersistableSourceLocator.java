package br.com.totvs.tds.ui.debug.helper;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;
import org.eclipse.lsp4e.debug.debugmodel.DSPStackFrame;

import br.com.totvs.tds.ui.debug.DebugUIActivator;

/**
 *
 * @author acandido
 *
 */

@SuppressWarnings("restriction")
public class PersistableSourceLocator extends AbstractSourceLookupDirector {

	public static final String ID = "br.com.totvs.tds.ui.debug.sourceLocator"; //$NON-NLS-1$
	private static final IProgressMonitor NULL_MONITOR = new NullProgressMonitor();;

	@Override
	public void initializeParticipants() {
		addParticipants(new ISourceLookupParticipant[] { new TdsSourceLookupParticipant() });
	}

	@Override
	public Object getSourceElement(final Object element) {
		Object ret = null;

		try {
			final ISourceContainer[] sourceContainers = getSourcePathComputer()
					.computeSourceContainers(getLaunchConfiguration(), NULL_MONITOR);
			for (final ISourceContainer sourceContainer : sourceContainers) {
				ret = searchElement(sourceContainer, (DSPStackFrame) element);
				if (ret != null) {
					break;
				}
			}
		} catch (final CoreException e) {
			DebugUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		if (ret == null) {
			ret = super.getSourceElement(element);
		}
		//
		return ret;
	}

	private Object searchElement(final ISourceContainer sourceContainer, final DSPStackFrame element) {
		Object result = null;

		try {
			result = sourceContainer.findSourceElements(element.getSourceName());
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
		}

		return result;
	}

}
