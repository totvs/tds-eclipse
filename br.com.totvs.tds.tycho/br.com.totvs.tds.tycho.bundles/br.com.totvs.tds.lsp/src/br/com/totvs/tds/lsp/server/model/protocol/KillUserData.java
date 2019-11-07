package br.com.totvs.tds.lsp.server.model.protocol;

public class KillUserData {

	private KillUserInfo killUserInfo;

	public KillUserData(final KillUserInfo killUserInfo) {
		this.killUserInfo = killUserInfo;
	}

	/**
	 * @return the killUserInfo
	 */
	public KillUserInfo getKillUserInfo() {
		return killUserInfo;
	}

	/**
	 * @param killUserInfo the killUserInfo to set
	 */
	public void setKillUserInfo(final KillUserInfo killUserInfo) {
		this.killUserInfo = killUserInfo;
	}

}
