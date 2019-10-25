package br.com.totvs.tds.ui.server.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;

public class SmartClientServersVO {

	private List<IAppServerInfo> advplAppServers;

	private final Map<String, IAppServerInfo> mapServersSelecionados = new HashMap<String, IAppServerInfo>();

	private String serverTypeApp;

	private String targetFile = ""; //$NON-NLS-1$

	private IGroupInfo targetNode;

	public List<IAppServerInfo> getAdvplAppServers() {
		return advplAppServers;
	}

	public Map<String, IAppServerInfo> getServersSelecionados() {
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

	public void setAdvplAppServers(List<IAppServerInfo> advplAppServers) {
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
