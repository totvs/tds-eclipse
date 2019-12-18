package br.com.totvs.tds.ui.debug.helper;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;
import org.eclipse.debug.core.sourcelookup.ISourceLookupDirector;
import org.eclipse.lsp4e.debug.debugmodel.DSPStackFrame;
import org.eclipse.lsp4e.debug.sourcelookup.AbsolutePathSourceContainer;

@SuppressWarnings("restriction")
public class TdsSourceLookupParticipant extends AbstractSourceLookupParticipant {

	@Override
	public String getSourceName(final Object object) throws CoreException {
		if (object instanceof DSPStackFrame) {
			return ((DSPStackFrame) object).getSourceName();
		}

		return null;
	}

	static class NoSourceElement {
	}

	private Map<Object, Object[]> fCachedResults = Collections.synchronizedMap(new HashMap<Object, Object[]>());

	/**
	 * Constructor for CSourceLookupParticipant.
	 */
	public TdsSourceLookupParticipant() {
		super();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant#
	 * findSourceElements(java.lang.Object)
	 */
	@Override
	public Object[] findSourceElements(final Object object) throws CoreException {
		// Check the cache
		final Object[] results = fCachedResults.get(object);
		if (results != null) {
			return results;
		}

		// Workaround for cases when the stack frame doesn't contain the source file
		// name
		String name = null;
		final IBreakpoint breakpoint = null;
		if (object instanceof String) {
			name = (String) object;
		}

		// Actually query the source containers for the requested resource
		Object[] foundElements = super.findSourceElements(object);

		// If none found, invoke the absolute path container directly
		if ((foundElements.length == 0) && (object instanceof IDebugElement)) {
			// debugger could have resolved it itself and "name" is an absolute path
			if (new File(name).exists()) {
				foundElements = new AbsolutePathSourceContainer().findSourceElements(name);
			} else {
				foundElements = new Object[] { new CSourceNotFoundElement((IDebugElement) object,
						((IDebugElement) object).getLaunch().getLaunchConfiguration(), name) };
			}
		}

		// Source lookup participant order is preserved where possible except for one
		// case:
		// - If we've stopped at a breakpoint the user has made on an IResource, we
		// definitely want to show
		// that IResource before others
		if ((breakpoint != null) && (breakpoint.getMarker() != null)
				&& (breakpoint.getMarker().getResource() != null)) {
			final IResource breakpointResource = breakpoint.getMarker().getResource();
			for (int i = 0; i < foundElements.length; i++) {
				if (foundElements[i].equals(breakpointResource)) {
					final Object temp = foundElements[0];
					foundElements[0] = foundElements[i];
					foundElements[i] = temp;
					break;
				}
			}
		}

		fCachedResults.put(object, foundElements);
		return foundElements;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant#
	 * sourceContainersChanged(org.eclipse.debug.core.sourcelookup.
	 * ISourceLookupDirector)
	 */
	@Override
	public void sourceContainersChanged(final ISourceLookupDirector director) {
		// chedResults.clear();

		super.sourceContainersChanged(director);
	}
}