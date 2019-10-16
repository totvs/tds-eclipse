package br.com.totvs.tds.server.jobs.applyPatch;

/**
 * Modos de aplicação do pacote.
 */
public enum ApplyPatchMode {

	/**
	 * Only validates the Patch file.
	 */
	VALIDATE_PATCH 
	/**
	 * Apply all sources.
	 */
	,APPLY_ALL 
	/**
	 * Apply only the sources that have a newer date then the one in the RPO.
	 */
	,APPLY_NEWEST_ONLY 
	/**
	 * Inidicates that there was an error on the patch validation.
	 */
	,VALIDATE_ERROR;

	public String getText() {
		switch (ApplyPatchMode.this) {
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
}

