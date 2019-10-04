package br.com.totvs.tds.lsp.server.model.protocol;

public class ValidKeyData {

	KeyInfo keyInfo;

	/**
	 * @param KeyInfo
	 */
	public ValidKeyData(final KeyInfo keyInfo) {
		this.keyInfo = keyInfo;
	}

	/**
	 * @return the KeyInfo
	 */
	public KeyInfo getKeyInfo() {
		return keyInfo;
	}

	/**
	 * @param KeyInfo the KeyInfo to set
	 */
	public void setKeyInfo(final KeyInfo KeyInfo) {
		this.keyInfo = KeyInfo;
	}
}