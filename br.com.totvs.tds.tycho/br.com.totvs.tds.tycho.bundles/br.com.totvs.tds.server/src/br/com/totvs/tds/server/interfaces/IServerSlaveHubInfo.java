package br.com.totvs.tds.server.interfaces;

public interface IServerSlaveHubInfo extends IItemInfo {

	IServerInfo getMasterLoadBalance();

	boolean isEmpty();

	void clear();

	void add(IAppServerSlaveInfo slave);

}
