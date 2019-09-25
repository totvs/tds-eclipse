package br.com.totvs.tds.server.model;

public enum RPOTypeElement {
	FUNCTION("0", "Função"),
	OBJECT("1", "Objeto RPO"),
	PROGRAM("2", "Fonte"),
	RESOURCE("3", "Recurso"), UNKNOWN("4", "Desconhecido");

	final private String rpoCode;
	final private String title;

	private RPOTypeElement(String rpoCode, String title) {
		this.rpoCode = rpoCode;
		this.title = title;
	}

	/**
	 * @return the rpoCode
	 */
	public String getRpoCode() {

		return rpoCode;
	}

	public String getTitle() {

		return this.title;
	}
}
