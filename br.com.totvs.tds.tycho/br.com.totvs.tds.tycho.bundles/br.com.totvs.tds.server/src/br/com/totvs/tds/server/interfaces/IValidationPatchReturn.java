package br.com.totvs.tds.server.interfaces;

import java.util.List;

/**
 * Interface ValidationPatchReturn.
 *
 * @author leo.watanabe
 *
 */
public interface IValidationPatchReturn extends IServerReturn {

	/**
	 * Obt�m a lista oldProgramsReturn.
	 *
	 * @return
	 */
	List<String[]> getOldPrograms();

	/**
	 * Define a lista oldProgramsReturn.
	 *
	 * @param oldProgramsReturn
	 */
	void setOldPrograms(List<String[]> oldProgramsReturn);

}