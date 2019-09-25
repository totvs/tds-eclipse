package br.com.totvs.tds.server.model;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAppServerSlaveInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;

/**
 * ServerSlaveHubInfo.
 * 
 * @author leo.watanabe
 * 
 */
public class ServerSlaveHubInfo extends GroupInfo implements IServerSlaveHubInfo, IGroupInfo {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public ServerSlaveHubInfo(IAppServerInfo masterLoadBalance) {
		super("Escravos"); //$NON-NLS-1$
		
		setParent(masterLoadBalance);
	}

	@Override
	public IServerInfo getMasterLoadBalance() {
		return (IServerInfo) getParent();
	}

	@Override
	public boolean isPersistent() {
		return false;
	}

	@Override
	public boolean isEmpty() {

		return !this.hasChildren();
	}

	@Override
	public void clear() {
		this.getChildren().clear();
	}

	@Override
	public void add(IAppServerSlaveInfo slave) {
		slave.setParent(this);
		this.getChildren().add(slave);
	}

}
