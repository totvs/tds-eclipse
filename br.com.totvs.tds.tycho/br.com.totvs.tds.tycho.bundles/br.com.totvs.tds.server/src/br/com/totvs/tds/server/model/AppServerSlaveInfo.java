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
import br.com.totvs.tds.server.interfaces.ServerType;

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
	 * S Construtor.
	 *
	 * @param name
	 */
	public AppServerSlaveInfo(final IServerSlaveHubInfo hub, final String name) {
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
		return "server"; //$NON-NLS-1$
	}

	/**
	 * @return the master
	 */
	@Override
	public IAppServerInfo getMaster() {
		return this.hub.getMasterLoadBalance();
	}

	/**
	 * @return the hub
	 */
	public IServerSlaveHubInfo getHub() {
		return hub;
	}

	@Override
	public void setAddress(final URI addres) {
		this.addres = addres;
	}

	@Override
	public URI getAddress() {
		return this.addres;
	}

	@Override
	public ServerType getServerType() {
		return getMaster().getServerType();
	}

	@Override
	public void doReadExternal(final ObjectInput in) {
		// TODO Auto-generated method stub

	}

	@Override
	public void doWriteExternal(final ObjectOutput out) {
		// TODO Auto-generated method stub

	}

}
