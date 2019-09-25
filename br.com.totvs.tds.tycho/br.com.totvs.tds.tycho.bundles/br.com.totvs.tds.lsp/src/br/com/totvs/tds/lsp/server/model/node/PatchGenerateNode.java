package br.com.totvs.tds.lsp.server.model.node;

public class PatchGenerateNode {
	private Object id;
	private int returnCode = -1;

	/**
	 * @return the id
	 */
	public Object getId() {
		return id;
	}
	/**
	 * @param id the id to set
	 */
	public void setId(Object id) {
		this.id = id;
	}
	/**
	 * @return the returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}
	/**
	 * @param returnCode the returnCode to set
	 */
	public void setReturnCode(int returnCode) {
		this.returnCode = returnCode;
	}

}
