package br.com.totvs.tds.server.jobs.applyPatch;

/**
 * Enum MessageType.
 *
 * @author leo.watanabe
 *
 */
public enum ApplyPatchState {
	NEW, NEW_ZIP, OK, WARNING, ERROR, DUPLICATE;

	public String getSituation() {
		switch (ApplyPatchState.this) {
		case NEW:
		case NEW_ZIP:
			return "Novo";
		case OK:
			return "OK";
		case WARNING:
			return "Há restrições";
		case ERROR:
			return "Há erros";
		default:
			return "Duplicado";
		}
	}
};
