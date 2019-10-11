package br.com.totvs.tds.lsp.server.model.node;

public class PatchDirListNode {
	private Object id;
	private String[] directory;

	/**
	 * @return the id
	 */
	public Object getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(final Object id) {
		this.id = id;
	}

	public String[] getDirectory() {
		return directory;
	}

	public void setDirectory(final String[] directory) {
		this.directory = directory;
	}

}
