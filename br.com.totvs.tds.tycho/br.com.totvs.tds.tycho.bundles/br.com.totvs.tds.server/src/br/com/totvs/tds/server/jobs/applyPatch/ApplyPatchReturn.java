package br.com.totvs.tds.server.jobs.applyPatch;

import br.com.totvs.tds.server.interfaces.IApplyPatchReturn;
import br.com.totvs.tds.server.jobs.ServerReturn;

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
