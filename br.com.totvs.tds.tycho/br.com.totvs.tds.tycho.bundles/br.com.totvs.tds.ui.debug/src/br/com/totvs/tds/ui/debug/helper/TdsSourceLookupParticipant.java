package br.com.totvs.tds.ui.debug.helper;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;
import org.eclipse.lsp4e.debug.debugmodel.DSPStackFrame;

@SuppressWarnings("restriction")
public class TdsSourceLookupParticipant extends AbstractSourceLookupParticipant {

	@Override
	public String getSourceName(final Object object) throws CoreException {
		if (object instanceof DSPStackFrame) {
			return ((DSPStackFrame) object).getSourceName();
		}

		return null;
	}

}
