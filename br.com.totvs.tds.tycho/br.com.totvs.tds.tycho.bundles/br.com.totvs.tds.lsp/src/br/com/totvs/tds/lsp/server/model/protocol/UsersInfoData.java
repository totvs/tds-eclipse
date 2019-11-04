package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class UsersInfoData {

	private UsersInfo getUsersInfo;

	public UsersInfoData(final UsersInfo usersInfo) {
		this.setGetUsersInfo(usersInfo);
	}

	/**
	 * @return the getUsersInfo
	 */
	public UsersInfo getGetUsersInfo() {
		return getUsersInfo;
	}

	/**
	 * @param getUsersInfo the getUsersInfo to set
	 */
	public void setGetUsersInfo(final UsersInfo getUsersInfo) {
		this.getUsersInfo = getUsersInfo;
	}

}
