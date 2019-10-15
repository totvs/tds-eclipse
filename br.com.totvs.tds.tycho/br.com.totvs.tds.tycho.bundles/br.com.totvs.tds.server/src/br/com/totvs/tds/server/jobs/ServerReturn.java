package br.com.totvs.tds.server.jobs;

import br.com.totvs.tds.server.interfaces.IServerReturn;

/**
 * ServerReturn.
 *
 * @author leo.watanabe
 *
 */
public class ServerReturn implements IServerReturn {

	private boolean operationOk;

	private String returnMessage;

	/**
	 * Construtor.
	 *
	 * @param retSuccess
	 * @param retMessage
	 */
	public ServerReturn(final boolean retSuccess, final String retMessage) {
		this.returnMessage = retMessage;
		this.operationOk = retSuccess;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.core.messageservice.statusreturn.IServerReturnStatus#
	 * isOperationOK()
	 */
	@Override
	public final boolean isOperationOk() {
		return this.operationOk;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.core.messageservice.statusreturn.IServerReturnStatus#
	 * setOperationOk(boolean)
	 */
	@Override
	public final void setOperationOk(final boolean status) {
		this.operationOk = status;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.core.messageservice.statusreturn.IServerReturnStatus#
	 * getReturnMessage()
	 */
	@Override
	public final String getReturnMessage() {
		return this.returnMessage;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.core.messageservice.statusreturn.IServerReturnStatus#
	 * setReturnMessage(java.lang.String)
	 */
	@Override
	public final void setReturnMessage(final String msg) {
		this.returnMessage = msg;
	}

}
