package br.com.totvs.tds.ui.server.fileSystem;

import java.util.List;

public interface IServerDirectoryItemNode {

	void addItemNode(IServerDirectoryItemNode itemNode);

	String getAbsolutPath();

	List<IServerDirectoryItemNode> getChildren();

	String getName();

	IServerDirectoryItemNode getParent();

	IServerDirectoryServerNode getServerNode();

	boolean hasChildren();

	void removeItemNode(IServerDirectoryItemNode itemNode);

	void setAbsolutPath(String absolutPath);

	void setName(String name);

	void setParent(IServerDirectoryItemNode parent);

}
