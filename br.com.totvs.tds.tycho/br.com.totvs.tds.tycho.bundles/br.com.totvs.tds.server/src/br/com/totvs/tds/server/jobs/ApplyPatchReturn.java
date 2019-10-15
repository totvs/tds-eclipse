package br.com.totvs.tds.server.jobs;

import br.com.totvs.tds.server.interfaces.IApplyPatchReturn;

/**
 * ApplyPatchReturn.
 *
 * @author leo.watanabe
 *
 */
public class ApplyPatchReturn extends ServerReturn implements IApplyPatchReturn {

	/**
	 * Construtor.
	 *
	 * @param retSuccess
	 * @param retMessage
	 */
	public ApplyPatchReturn(final boolean retSuccess, final String retMessage) {
		super(retSuccess, retMessage);
	}

	/**
	 * Construtor.
	 *
	 * @param retSuccess
	 */
	public ApplyPatchReturn(final boolean retSuccess) {
		super(retSuccess, null);
	}

}
