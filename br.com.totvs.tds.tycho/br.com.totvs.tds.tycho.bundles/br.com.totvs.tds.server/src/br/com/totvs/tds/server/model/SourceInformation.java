package br.com.totvs.tds.server.model;

/**
 * Classe para representar uma imformação do log de patch.
 */
public class SourceInformation {

	private String buildType;
	private String date;
	private String name;
	private String size;
	private String sourceType;

	/**
	 * Retorna o tipo de build do fonte.
	 * 
	 * @return tipo de build
	 */
	public String getBuildType() {
		return buildType;
	}

	/**
	 * Retorna a data de aplicação do fonte.
	 * 
	 * @return data
	 */
	public String getDate() {
		return date;
	}

	/**
	 * Retorna o nome do fonte.
	 * 
	 * @return nome
	 */
	public String getName() {
		return name;
	}

	/**
	 * Retorna o tamanho do fonte.
	 * 
	 * @return tamanho
	 */
	public String getSize() {
		return size;
	}

	/**
	 * Retorna o tipo do fonte.
	 * 
	 * @return tipo
	 */
	public String getSourceType() {
		return sourceType;
	}

	/**
	 * Define o tipo de build do fonte.
	 * 
	 * @param buildType
	 *            tipo de build
	 */
	public void setBuildType(final String buildType) {
		this.buildType = buildType;
	}

	/**
	 * Define a data do fonte.
	 * 
	 * @param date
	 *            data
	 */
	public void setDate(final String date) {
		this.date = date;
	}

	/**
	 * Define o nome do fonte.
	 * 
	 * @param name
	 *            nome
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Define o tamanho do fonte.
	 * 
	 * @param size
	 *            tamanho
	 */
	public void setSize(final String size) {
		this.size = size;
	}

	/**
	 * Define o tipo do fonte.
	 * 
	 * @param sourceType
	 *            tipo
	 */
	public void setSourceType(final String sourceType) {
		this.sourceType = sourceType;
	}

}
