package br.com.totvs.tds.server.interfaces;

/**
 * Interface ServerReturn.
 *
 * @author leo.watanabe
 *
 */
public interface IServerReturn {

	/**
	 * Obt�m a flag operationOk.
	 *
	 * @return
	 */
	boolean isOperationOk();

	/**
	 * Define a flag operationOk.
	 *
	 * @param status
	 */
	void setOperationOk(boolean status);

	/**
	 * Obt�m o returnMessage.
	 *
	 * @return
	 */
	String getReturnMessage();

	/**
	 * Define o returnMessage.
	 *
	 * @param msg
	 */
	void setReturnMessage(String msg);

}