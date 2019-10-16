package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn.MessageType;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * The Label Provider for the Column Program.
 *
 * @author daniel.yampolschi
 * @author acandido
 *
 */
public class ColumnProgramLabelProvider extends ColumnLabelProvider {

	private boolean showFullPath = false;

	@Override
	public Image getImage(final Object element) {
		Image validoImage = null;
		ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;
		MessageType messageType = applyPatchFileReturn.getMessageType();

		if (messageType != null) {
			validoImage = getImageByMessageType(messageType);
		}

		return validoImage;
	}

	private Image getImageByMessageType(final MessageType messageType) {
		Image image = null;
		switch (messageType) {
		case NEW:
			image = ServerUIIcons.getHelp().createImage();
			break;
		case NEW_ZIP:
			image = ServerUIIcons.getZip().createImage();
			break;
		case OK:
			image = ServerUIIcons.getOk().createImage();
			break;
		case WARNING:
			image = ServerUIIcons.getWarning().createImage();
			break;
		case ERROR:
		case DUPLICATE_FILE:
			image = ServerUIIcons.getError().createImage();
			break;
		default:
			break;
		}
		return image;
	}

	@Override
	public String getText(final Object element) {
		ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;

		String text = applyPatchFileReturn.getPatchFile().lastSegment(); // nome do arquivo
		if (showFullPath) {
			text = (applyPatchFileReturn.getOriginalFile() == null) ? applyPatchFileReturn.getPatchFullPath()
					: applyPatchFileReturn.getOriginalFile();
		}

		return text;
	}

	/**
	 * @return the showFullPath
	 */
	public boolean isShowFullPath() {
		return showFullPath;
	}

	/**
	 * @param showFullPath the showFullPath to set
	 */
	public void setShowFullPath(boolean showFullPath) {
		this.showFullPath = showFullPath;
	}
}
