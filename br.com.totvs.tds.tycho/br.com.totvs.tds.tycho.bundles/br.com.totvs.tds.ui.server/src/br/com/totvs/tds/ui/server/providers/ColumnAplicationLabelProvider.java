package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.ColumnLabelProvider;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn.MessageType;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchMode;

/**
 * The application label provider.
 *
 * @author daniel.yampolschi
 * @author acandido
 *
 */
public class ColumnAplicationLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(final Object element) {
		if (element instanceof ApplyPatchFileReturn) {
			ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;
			ApplyPatchMode applyMode = applyPatchFileReturn.getApplyMode();
			MessageType messageType = applyPatchFileReturn.getMessageType();

			if ((messageType != null) && (messageType.equals(MessageType.DUPLICATE_FILE))) {
				return messageType.getSituation();
			}

			return applyMode.getText();
		}
		return null;
	}

}
