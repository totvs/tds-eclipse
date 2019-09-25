package br.com.totvs.tds.server.jobs;

/**
 * Enum representando os tipos de processo que podem ser utilizados na geração de patchs.
 */
public enum BuildPatchProcessType {

	UNDEFINED(-1, "(não definido)"), //
	BY_COMPARISON(0, "por comparação (RPO)"),  //
	BY_RPO(1, "a partir do RPO"), //
	BY_WORKAREA(2, "a partir da �rea de trabalho");

	/**
	 * C�digo do processo.
	 */
	private int code;

	/**
	 * Etiqueta do processo.
	 */
	private String label;

	private BuildPatchProcessType(int code, String label) {
		this.code = code;
		this.label = label;
	}

	/**
	 * Retorna a etiqueta do processo.
	 * 
	 * @return A etiqueta do processo.
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * O C�digo do processo.
	 * 
	 * @return the code
	 */
	public int getCode() {
		return code;
	}

}
