package br.com.totvs.tds.lsp.server.model.protocol;

public class PatchGenerateData {
	private PatchGenerateInfo patchGenerateInfo;

	public PatchGenerateData(PatchGenerateInfo patchGenerateInfo) {
		this.patchGenerateInfo = patchGenerateInfo;
	}

	/**
	 * @return the PatchGenerateInfo
	 */
	public PatchGenerateInfo getPatchGenerateInfo() {
		return patchGenerateInfo;
	}
}
