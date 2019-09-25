package br.com.totvs.tds.lsp.server.model.protocol;

public class SlaveData {

	private SlaveInfo slaveInfo;

	public SlaveData(SlaveInfo slaveInfo) {
		this.slaveInfo = slaveInfo;
	}

	/**
	 * @return the slaveInfo
	 */
	public SlaveInfo getSlaveInfo() {
		return slaveInfo;
	}

}
