package br.com.totvs.tds.ui.server.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;

public class SmartClientServersVO {

	private List<IServerInfo> advplAppServers;

	private final Map<String, IServerInfo> mapServersSelecionados = new HashMap<String, IServerInfo>();

	private String serverTypeApp;

	private String targetFile = ""; //$NON-NLS-1$

	private IGroupInfo targetNode;

	public List<IServerInfo> getAdvplAppServers() {
		return advplAppServers;
	}

	public Map<String, IServerInfo> getServersSelecionados() {
		return mapServersSelecionados;
	}

	public String getServerTypeApp() {
		return serverTypeApp;
	}

	public String getTargetFile() {
		return targetFile;
	}

	public IGroupInfo getTargetNode() {
		return targetNode;
	}

	public void setAdvplAppServers(List<IServerInfo> advplAppServers) {
		this.advplAppServers = advplAppServers;
	}

	public void setServerTypeApp(String serverTypeApp) {
		this.serverTypeApp = serverTypeApp;
	}

	public void setTargetFile(String targetFile) {
		this.targetFile = targetFile;
	}

	public void setTargetNode(IGroupInfo targetNode) {
		this.targetNode = targetNode;
	}

	public boolean validAttributes() {
		return (mapServersSelecionados.size() > 0) && (!targetFile.isEmpty());
	}

}
