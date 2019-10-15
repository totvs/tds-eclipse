package br.com.totvs.tds.lsp.server.model.node;

public class ApplyPatchNode {
	private int returnCode;
	private String files;

	/**
	 * @return the returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * @param returnCode the returnCode to set
	 */
	public void setReturnCode(final int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * @return the files
	 */
	public String getFiles() {
		return files;
	}

	/**
	 * @param files the files to set
	 */
	public void setFiles(final String files) {
		this.files = files;
	}
}
