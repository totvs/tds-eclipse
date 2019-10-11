package br.com.totvs.tds.server.jobs;

/**
 * Enum representando os tipos de processo que podem ser utilizados na geração de patchs.
 */
public enum BuildPatchProcessType {

	UNDEFINED(-1, Messages.BuildPatchProcessType_Undefined), //
	BY_COMPARISON(0, Messages.BuildPatchProcessType_By_comparasion),  //
	BY_RPO(1, Messages.BuildPatchProcessType_FromRpo), //
	BY_WORKAREA(2, Messages.BuildPatchProcessType_From_workarea);

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
