package br.com.totvs.tds.ui.server.providers;

import org.eclipse.jface.viewers.ColumnLabelProvider;

import br.com.totvs.tds.server.jobs.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.ApplyPatchFileReturn.MessageType;
import br.com.totvs.tds.server.jobs.ApplyPatchMode;

/**
 * The application label provider.
 *
 * @author daniel.yampolschi
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
				return "Pacote em duplicidade. Somente o primeiro será processado.";
			}

			if (applyMode == null) {
				return "Indefinido";
			}

			switch (applyMode) {
			case VALIDATE_PATCH:
				return "Validar pacote";
			case APPLY_ALL:
				return "Aplicar todos";
			case APPLY_NEWEST_ONLY:
				return "Somente atualizados";
			case VALIDATE_ERROR:
				return "Erro de validação";
			default:
				return null;
			}
		}
		return null;
	}

}
