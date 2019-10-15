package br.com.totvs.tds.server.jobs;

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
	,VALIDATE_ERROR 
}

