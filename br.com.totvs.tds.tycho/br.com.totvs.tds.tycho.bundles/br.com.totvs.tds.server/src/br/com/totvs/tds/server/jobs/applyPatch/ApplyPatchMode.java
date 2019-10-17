package br.com.totvs.tds.server.jobs.applyPatch;

/**
 * Modos de aplicação do pacote.
 */
public enum ApplyPatchMode {
	// não mudar a ordem. há operações que dependem do método ordinal()
	NEED_VALIDATE, // necessita validar
	VALIDATE_PATCH, // validado
	APPLY_ALL, // aplicar todos os fontes
	APPLY_NEWEST_ONLY, // aplicar somente fontes mais novos
	APPLIED, // aplicado
	NOT_APPLIED; // não aplicado devido a erro

	public String getText() {
		switch (ApplyPatchMode.this) {
		case NEED_VALIDATE:
			return "Validar";
		case VALIDATE_PATCH:
			return "Pacote validado";
		case APPLY_ALL:
			return "Aplicar todos";
		case APPLY_NEWEST_ONLY:
			return "Somente atualizados";
		case APPLIED:
			return "Aplicado";
		case NOT_APPLIED:
			return "Não aplicado";
		default:
			return null;
		}
	}
}
