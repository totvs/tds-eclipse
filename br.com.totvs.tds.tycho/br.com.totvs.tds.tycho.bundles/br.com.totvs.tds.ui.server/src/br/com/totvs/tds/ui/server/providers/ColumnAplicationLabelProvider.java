package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.ColumnLabelProvider;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchMode;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchState;

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

			return applyPatchFileReturn.getApplyMode().getText();
		}

		return null;
	}

}
