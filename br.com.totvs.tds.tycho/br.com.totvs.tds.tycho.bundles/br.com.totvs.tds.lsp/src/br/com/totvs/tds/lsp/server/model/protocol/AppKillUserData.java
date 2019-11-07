package br.com.totvs.tds.lsp.server.model.protocol;

public class AppKillUserData {

	private AppKillUserInfo appKillUserInfo;

	public AppKillUserData(final AppKillUserInfo killUserInfo) {
		this.appKillUserInfo = killUserInfo;
	}

	/**
	 * @return the appKillUserInfo
	 */
	public AppKillUserInfo getAppKillUserInfo() {
		return appKillUserInfo;
	}

	/**
	 * @param appKillUserInfo the appKillUserInfo to set
	 */
	public void setAppKillUserInfo(final AppKillUserInfo appKillUserInfo) {
		this.appKillUserInfo = appKillUserInfo;
	}

}
