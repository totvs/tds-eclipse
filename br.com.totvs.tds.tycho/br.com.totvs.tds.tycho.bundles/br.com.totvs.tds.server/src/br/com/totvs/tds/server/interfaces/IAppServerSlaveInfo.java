package br.com.totvs.tds.server.interfaces;

import java.net.URI;

import br.com.totvs.tds.server.interfaces.IServerInfo.ServerType;

/**
 * Interface AppServerSlaveInfo.
 *
 * @author acandido
 *
 */
public interface IAppServerSlaveInfo extends IItemInfo {

	void setAddress(URI addres);

	ServerType getServerType();

	URI getAddress();

	IAppServerInfo getMaster();
}
