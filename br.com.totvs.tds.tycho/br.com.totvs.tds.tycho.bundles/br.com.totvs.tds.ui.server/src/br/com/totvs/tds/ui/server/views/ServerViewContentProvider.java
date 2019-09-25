package br.com.totvs.tds.ui.server.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;

/**
 * Provedor de conte�do para a Visão de servidores.
 * 
 * @author acandido
 */
public final class ServerViewContentProvider implements ITreeContentProvider {

	private static final Object[] EMPTY_LIST = new Object[0];

	public ServerViewContentProvider() {
	}

	@Override
	public void dispose() {
	}

	@Override
	public Object[] getChildren(final Object parent) {
		if (parent instanceof IAppServerInfo) {
			IAppServerInfo appServerInfo = (IAppServerInfo) parent;
			List<IItemInfo> childrenList = new ArrayList<IItemInfo>(appServerInfo.getEnvironments());

			if (appServerInfo.isConnected()) {
				IServerSlaveHubInfo slavesHub = appServerInfo.getSlaveLoadBalance();
				if (!slavesHub.isEmpty()) {
					childrenList.add(slavesHub);
				}
			}
				
			return childrenList.toArray();
		} else if (parent instanceof IGroupInfo) {
			return ((IGroupInfo) parent).getChildren().toArray();
		}

		return EMPTY_LIST;
	}

	@Override
	public Object[] getElements(final Object parent) {
		if (parent instanceof Object[]) {
			return (Object[]) parent;
		}
		return new Object[0];
	}

	@Override
	public Object getParent(final Object child) {
		Object parent = null;
		if (child instanceof IItemInfo) {
			parent = ((IItemInfo) child).getParent();
		}
		return parent;
	}

	@Override
	public boolean hasChildren(final Object parent) {
		boolean ret = false;
		
		if (parent instanceof IAppServerInfo) {
			ret = !((IAppServerInfo) parent).getEnvironments().isEmpty();
		} else if (parent instanceof IGroupInfo) {
			ret = ((IGroupInfo) parent).hasChildren();
		}
		
		return ret;
	}

	@Override
	public void inputChanged(final Viewer v, final Object oldInput, final Object newInput) {

	}

}
