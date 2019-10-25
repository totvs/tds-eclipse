package br.com.totvs.tds.server.interfaces;

public interface IServerSlaveHubInfo extends IItemInfo {

	IAppServerInfo getMasterLoadBalance();

	boolean isEmpty();

	void clear();

	void add(IAppServerSlaveInfo slave);

}
