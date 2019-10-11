package br.com.totvs.tds.lsp.server.model.protocol;

public class PathDirListData {

	private PathDirListInfo pathDirListInfo;

	public PathDirListData(final PathDirListInfo pathDirListInfo) {
		this.pathDirListInfo = pathDirListInfo;
	}

	public PathDirListInfo getPatchDirInfo() {
		return pathDirListInfo;
	}

	public void setPatchDirInfo(final PathDirListInfo pathDirListInfo) {
		this.pathDirListInfo = pathDirListInfo;
	}

}
