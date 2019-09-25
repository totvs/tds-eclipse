package br.com.totvs.tds.lsp.server.model.node;

public class NodeInfo {
	private String buildVersion;
	private Object id;

	/**
	 * @return the buildVersion
	 */
	public String getBuildVersion() {
		return buildVersion;
	}

	/**
	 * @return the id
	 */
	public Object getId() {
		return id;
	}

	/**
	 * @param buildVersion the buildVersion to set
	 */
	public void setBuildVersion(String buildVersion) {
		this.buildVersion = buildVersion;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(Object id) {
		this.id = id;
	}
}
