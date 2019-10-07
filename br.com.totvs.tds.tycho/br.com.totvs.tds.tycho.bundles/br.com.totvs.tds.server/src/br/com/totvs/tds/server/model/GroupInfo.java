package br.com.totvs.tds.server.model;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.ArrayList;
import java.util.List;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;

/**
 * informações sobre Grupo.
 *
 * @author acandido
 */
public class GroupInfo extends ItemInfo implements IGroupInfo {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	private final List<IItemInfo> children = new ArrayList<IItemInfo>();

	/**
	 * Construtor.
	 */
	public GroupInfo() {
		super();
	}

	/**
	 * Construtor.
	 *
	 * @param name nome do grupo
	 */
	public GroupInfo(final String name) {
		super(name);
	}

	@Override
	public void addChild(final IItemInfo child) throws RuntimeException {
		if (child == null) {
			throw new NullPointerException();
		}
		final boolean isValid = child.isValid();

		if (isValid) {
			if (containsNode(child.getName())) {
				throw new RuntimeException(String.format("Já existe um item com esse nome. Nome: %s", child.getName()));
			}
		}

		child.setParent(this);
		children.add(child);

		firePropertyChange("add_children", this, child); //$NON-NLS-1$
	}

	@Override
	public boolean containsNode(final String name) {
		final IItemInfo target = searchNode(name);

		return target != null;
	}

	@Override
	public List<IItemInfo> getChildren() {
		return children;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#containsNode(java.lang.
	 * String )
	 */

	@Override
	public String getIconName() {

		return "group";
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#hasChildren()
	 */
	@Override
	public boolean hasChildren() {
		return children.size() > 0;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#removeChild(br.com.totvs
	 * .tds.server.internal.IServerInfo)
	 */
	@Override
	public void removeChild(final IItemInfo child) {
		if (children.contains(child)) {
			children.remove(child);
			child.setParent(null);
			firePropertyChange("remove_child", this, child); //$NON-NLS-1$
		} else {
			for (final IItemInfo iItemInfo : children) {
				if (iItemInfo instanceof IGroupInfo) {
					((IGroupInfo) iItemInfo).removeChild(child);
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.server.internal.IServerInfo#searchNode(java.lang.String)
	 */
	@Override
	public IItemInfo searchNode(final String name) {
		if ((getName() != null) && getName().equalsIgnoreCase(name)) {
			return this;
		}
		//
		for (final IItemInfo element : getChildren()) {
			if (element.getName().equalsIgnoreCase(name)) {
				return element;
			} else if (element instanceof IGroupInfo) {
				final IItemInfo itemInfo = ((IGroupInfo) element).searchNode(name);
				if (itemInfo != null) {
					return itemInfo;
				}
			}
		}
		//
		return null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#size()
	 */
	@Override
	public int size() {
		return getChildren().size();
	}

	/**
	 * Obt�m uma lista com determinado tipo de item, independente de hierarquia.
	 *
	 * @param class1
	 * @return the list
	 */
	@Override
	public List<IItemInfo> toList(final Class<?> target) {
		final List<IItemInfo> ret = new ArrayList<IItemInfo>();

		for (final IItemInfo element : getChildren()) {
			if (target.isInstance(element)) {
				ret.add(element);
			}
			if (element instanceof IGroupInfo) {
				ret.addAll(((IGroupInfo) element).toList(target));
			}

			if (element instanceof IAppServerInfo) {
				final IAppServerInfo serverInfo = (IAppServerInfo) element;
				final IServerSlaveHubInfo slave = serverInfo.getSlaveLoadBalance();
				if (slave != null) {
					final List<IItemInfo> slavesChildrens = ((IGroupInfo) slave).getChildren();
					if (slavesChildrens != null) {
						ret.addAll(slavesChildrens);
					}
				}
			}
		}

		return ret;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IItemInfo#doReadExternal(ObjectInput)
	 */
	@Override
	public void doReadExternal(final ObjectInput in) throws IOException, ClassNotFoundException {
		// Obtem os filhos
		IItemInfo child = null;
		final int size = in.readInt();
		for (int i = 0; i < size; i++) {
			child = (IItemInfo) in.readObject();
			this.addChild(child);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IItemInfo#doWriteExternal(ObjectInput)
	 */
	@Override
	public void doWriteExternal(final ObjectOutput out) throws IOException {
		// filhos
		out.writeInt(children.size());
		for (final IItemInfo element : children) {
			out.writeObject(element);
		}
	}

	@Override
	public IGroupInfo[] getFullParent() {
		final List<IGroupInfo> parents = new ArrayList<IGroupInfo>();
		IItemInfo auxParent = getParent();

		while (auxParent != null) {
			parents.add(0, (IGroupInfo) auxParent);
			auxParent = auxParent.getParent();
		}

		return parents.toArray(new IGroupInfo[parents.size()]);
	}

}
