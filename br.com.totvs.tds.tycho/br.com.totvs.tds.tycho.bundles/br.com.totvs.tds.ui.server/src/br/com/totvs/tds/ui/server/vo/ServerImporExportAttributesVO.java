package br.com.totvs.tds.ui.server.vo;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;

public class ServerImporExportAttributesVO {

	public enum ServerMergeOption {
		KEEP, NONE, SUBSTITUTE
	}

	public IItemInfo selection;
	private final HashMap<String, IItemInfo> mapServersSelecionados = new HashMap<String, IItemInfo>();
	private Set<String> repeatedServerNames;
	private Object[] selectedItems;
	private ServerMergeOption serverMergeOption = ServerMergeOption.NONE;
	private String targetFile = ""; //$NON-NLS-1$
	private IGroupInfo targetNode;
	private IGroupInfo totvsServers;

	// private XMLServerRoot xmlServerRoot;

	/**
	 * Retorna os servidores selecionados.<br>
	 * Neste caso � necess�rio que o objeto retornado seja um HashMap ao inv�s do
	 * seu tipo primitivo (Map)<br>
	 * pois o HashMap Já oferece suporte � serialização.
	 *
	 * @return
	 */
	public HashMap<String, IItemInfo> getItemsSelected() {
		return mapServersSelecionados;
	}

	public Set<String> getRepeatedServerNames() {
		if (repeatedServerNames == null) {
			return new HashSet<String>();
		}
		return repeatedServerNames;
	}

	public Object[] getSelectedItems() {
		return selectedItems;
	}

	public IItemInfo getSelection() {
		return selection;
	}

	public ServerMergeOption getServerMergeOption() {
		return serverMergeOption;
	}

	public String getTargetFile() {
		return targetFile;
	}

	public IGroupInfo getTargetNode() {
		return targetNode;

	}

	public IGroupInfo getTotvsServers() {
		return totvsServers;
	}

//	public XMLServerRoot getXMLServerRoot() {
//		if(xmlServerRoot == null) {
//			xmlServerRoot = new ObjectFactory().createXMLServerRoot();
//		}
//		return xmlServerRoot;
//	}

	public void setRepeatedServerNames(Collection<String> list) {
		this.repeatedServerNames = new HashSet<String>(list);
	}

	public void setSelectedItems(Object[] selectedItems) {
		this.selectedItems = selectedItems;
	}

	public void setServerMergeOption(ServerMergeOption serverMergeOption) {
		this.serverMergeOption = serverMergeOption;
	}

	public void setTargetFile(String targetFile) {
		this.targetFile = targetFile;
	}

	public void setTargetNode(IGroupInfo target) {
		targetNode = target;
	}

	public void setTotvsServers(IGroupInfo totvsServers) {
		this.totvsServers = totvsServers;
	}

//	public void setXMLServerRoot(final XMLServerRoot xmlServerRoot) {
//		this.xmlServerRoot = xmlServerRoot;
//	}

	public boolean validAttributes() {
		return (mapServersSelecionados.size() > 0) && (!targetFile.isEmpty()) && (targetFile.endsWith(".srv")); //$NON-NLS-1$
	}

}
