package br.com.totvs.tds.server.jobs;

/**
 * Enum para modos de limpeza.
 */
public enum CleanUpType {

	CLEANUP_PATCH(0), CLEANUP_COMPILE(1);

	private int type;

	/**
	 * Construtor para conter o type num�rico a ser utilizado nas mensagens.
	 *
	 * @param type tipo de limpeza
	 */
	private CleanUpType(final int type) {
		this.type = type;
	}

	/**
	 * Retorna o type numérico a ser utilizado nas mensagens.
	 *
	 * @return tipo de limpeza
	 */
	public int getType() {
		return this.type;
	}

}
