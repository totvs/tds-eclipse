package br.com.totvs.tds.server.interfaces;

import java.util.List;

/**
 * Interface Company.
 * 
 * @author leo.watanabe
 *
 */
public interface IOrganization {

	/**
	 * @return the code
	 */
	String getCode();

	/**
	 * @return the current subsidiary
	 */
	ISubsidiary getCurrentSubsidiary();

	/**
	 * @return the name
	 */
	String getName();

	/**
	 * @return the subsidiaries
	 */
	List<ISubsidiary> getSubsidiaries();

	/**
	 * 
	 * @param code
	 * @return ISubsidiary
	 */
	ISubsidiary getSubsidiary(String code);

	/**
	 * @param code
	 *            the cod to set
	 */
	void setCode(String code);

	/**
	 * Ajusta a filial corrente.
	 * 
	 * @param subsidiary
	 *            novo filial corrente
	 */
	void setCurrentSubsidiary(ISubsidiary subsidiary);

	/**
	 * @param name
	 *            the name to set
	 */
	void setName(String name);
}