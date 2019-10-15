package br.com.totvs.tds.lsp.server.model.protocol;

public class PatchApplyData {

	private PatchApplyInfo patchApplyInfo;

	public PatchApplyData(final PatchApplyInfo patchApplyInfo) {
		this.patchApplyInfo = patchApplyInfo;
	}

	/**
	 * @return the patchApplyInfo
	 */
	public PatchApplyInfo getPatchApplyInfo() {
		return patchApplyInfo;
	}

	/**
	 * @param patchApplyInfo the patchApplyInfo to set
	 */
	public void setPatchApplyInfo(final PatchApplyInfo patchApplyInfo) {
		this.patchApplyInfo = patchApplyInfo;
	}

}
