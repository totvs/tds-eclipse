package br.com.totvs.tds.lsp.server.model.protocol;

public class DisconnectData {
	private DisconnectInfo disconnectInfo;

	public DisconnectData(DisconnectInfo disconnectInfo) {
		this.disconnectInfo = disconnectInfo;
	}

	/**
	 * @return the validationInfo
	 */
	public DisconnectInfo getDisconnectInfo() {
		return disconnectInfo;
	}
}
