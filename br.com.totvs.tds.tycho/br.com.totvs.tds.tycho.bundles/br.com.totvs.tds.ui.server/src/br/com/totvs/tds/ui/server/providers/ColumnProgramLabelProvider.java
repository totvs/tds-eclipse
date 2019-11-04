package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchState;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * The Label Provider for the Column Program.
 *
 * @author daniel.yampolschi
 * @author acandido
 *
 */
public class ColumnProgramLabelProvider extends ColumnLabelProvider {

	@Override
	public Image getImage(final Object element) {
		ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;
		ApplyPatchState patchState = applyPatchFileReturn.getPatchState();

		return getImageState(patchState);
	}

	private Image getImageState(final ApplyPatchState messageType) {
		Image image = null;

		switch (messageType) {
		case NEW:
			image = ServerUIIcons.getHelp().createImage(true);
			break;
		case NEW_ZIP:
			image = ServerUIIcons.getZip().createImage(true);
			break;
		case OK:
			image = ServerUIIcons.getOk().createImage(true);
			break;
		case WARNING:
			image = ServerUIIcons.getWarning().createImage(true);
			break;
		case ERROR:
		case DUPLICATE:
			image = ServerUIIcons.getError().createImage(true);
			break;
		default:
			break;
		}

		return image;
	}

	@Override
	public String getText(final Object element) {
		ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;

		return applyPatchFileReturn.getPatchFile().lastSegment(); // nome do arquivo
	}
}
