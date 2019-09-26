/**
 * 
 */
package br.com.totvs.tds.server.model;

import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.net.URI;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAppServerSlaveInfo;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;

/**
 * Base de servidores Protheus.
 * 
 * @author acandido
 */
public class AppServerSlaveInfo extends ItemInfo implements IAppServerSlaveInfo {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private IServerSlaveHubInfo hub;
	private URI addres;

	/**
	 * Construtor.
	 * 
	 * @param name
	 */
	public AppServerSlaveInfo(IServerSlaveHubInfo hub, final String name) {
		super(name);

		this.hub = hub;
		this.hub.add(this);
		setParent(this.hub);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.server.internal.IServerInfo#containsNode(java.lang.
	 * String )
	 */

	@Override
	public boolean isPersistent() {

		return false;
	}

	@Override
	public String getIconName() {
		return "server";
	}

	/**
	 * @return the master
	 */
	@Override
	public IAppServerInfo getMaster() {
		return (IAppServerInfo) this.hub.getMasterLoadBalance();
	}

	/**
	 * @return the hub
	 */
	public IServerSlaveHubInfo getHub() {
		return hub;
	}

	@Override
	public void setAddress(URI addres) {
		this.addres = addres;
	}

	@Override
	public URI getAddress() {
		return this.addres;
	}

	@Override
	public String getServerType() {
		return getMaster().getServerType();
	}

	@Override
	public void doReadExternal(ObjectInput in) {
		// TODO Auto-generated method stub

	}

	@Override
	public void doWriteExternal(ObjectOutput out) {
		// TODO Auto-generated method stub

	}

}