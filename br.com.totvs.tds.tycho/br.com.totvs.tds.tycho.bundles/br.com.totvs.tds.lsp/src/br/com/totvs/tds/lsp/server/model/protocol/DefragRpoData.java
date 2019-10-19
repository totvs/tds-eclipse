package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class DefragRpoData {
	private DefragRpoInfo defragRpoInfo;

	public DefragRpoData(DefragRpoInfo defragRpoInfo) {
		this.defragRpoInfo = defragRpoInfo;
	}

	/**
	 * @return the defragRPOInfo
	 */
	public DefragRpoInfo getDefragRpoInfo() {
		return defragRpoInfo;
	}

	/**
	 * @param defragRPOInfo the defragRPOInfo to set
	 */
	public void setDefragRpoInfo(final DefragRpoInfo defragRPOInfo) {
		this.defragRpoInfo = defragRPOInfo;
	}

}
