package br.com.totvs.tds.server.tools;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.Group.GroupList;
import br.com.totvs.tds.server.xml.ObjectFactory;
import br.com.totvs.tds.server.xml.Server;
import br.com.totvs.tds.server.xml.XMLServerRoot;
import br.com.totvs.tds.server.xml.XMLVersionControl;

/**
 * A class to provide tools for export server objects.<br>
 * Each process that needs a
 *
 * @author daniel.yampolschi
 *
 */
public final class ExportTool {

	public enum GROUP_TYPE {
		GROUP, ROOT
	}

	public static final long serialVersionUID = 3L;

	/**
	 * Adds the IItemInfo element to the ServerTree structure informed using the
	 * informed factory.<br>
	 * if the parentGroup is passed as <b>null</b> it will be added in the root.
	 *
	 * @param serverTreeFactory
	 * @param root
	 * @param parentGroup
	 * @param nodeElement
	 * @return
	 */
	public static Group addElementToStructure(final ObjectFactory serverTreeFactory, final Group root,
			final Group parentGroup, final IItemInfo nodeElement) {
		Group groupToReturn = null;
		if (nodeElement instanceof IGroupInfo) {
			groupToReturn = addGroup(serverTreeFactory, root, parentGroup, nodeElement);
		} else if (nodeElement instanceof IServerInfo) {
			groupToReturn = addServer(serverTreeFactory, root, parentGroup, nodeElement);
		}
		return groupToReturn;
	};

	/**
	 * Exports the list of servers to the specified file.<br>
	 *
	 * @param selectedServesStructure - The list of servers to be exported.
	 *                                <b>Cannot be null</b>
	 * @param file                    - The target file where the information will
	 *                                be saved. The file must exist and have write
	 *                                permission.
	 * @param monitor                 - The monitor
	 * @throws IOException
	 * @throws FileNotFoundException
	 * @throws InterruptedException
	 * @throws JAXBException
	 */
	public static void exportServers(final XMLServerRoot xmlroot, final File file, final IProgressMonitor monitor)
			throws FileNotFoundException, IOException, InterruptedException, JAXBException {
		JAXBContext jaxbContext = JAXBContext.newInstance(XMLServerRoot.class);
		Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
		//
		// // output pretty printed
		jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
		//
		monitor.subTask("Exportação: Servidores");
		monitor.worked(1);
		//
		jaxbMarshaller.marshal(xmlroot, file);
		// jaxbMarshaller.marshal(selectedServesStructure, System.out);
		//
	}

	/**
	 * Tries to find the specified group with the id informed in the list informed.
	 *
	 * @param groupList
	 * @param id
	 * @return
	 */
	public static Group findGroupByName(final GroupList groupList, final String name) {
		Group groupFound = null;
		for (Group groupChild : groupList.getGroup()) {
			if (groupChild.getName().equals(name)) {
				groupFound = groupChild;
				break;
			}

			groupFound = findGroupByName(groupChild.getGroupList(), name);
		}
		return groupFound;
	}

	public static IGroupInfo findNode(final IGroupInfo parentNode, final String nameToFind) {
		IGroupInfo groupFound = null;
		List<IItemInfo> children = parentNode.getChildren();
		for (IItemInfo groupChild : children) {
			if (groupChild instanceof IGroupInfo && groupChild.getName().equals(nameToFind)) {
				groupFound = (IGroupInfo) groupChild;
				break;
			}
		}
		return groupFound;
	}

	public static XMLServerRoot initializeServerStructure(final ObjectFactory objectFactory) {
		XMLServerRoot xmlServerRoot = objectFactory.createXMLServerRoot();
		Group serverTreeRoot = objectFactory.createGroup();
		serverTreeRoot.setName("Messages.ExportTool_2");
		xmlServerRoot.setVersion(XMLVersionControl.V_11_3_8);
		serverTreeRoot.setGroupList(new GroupList());
		xmlServerRoot.setServerTreeRoot(serverTreeRoot);
		return xmlServerRoot;
	}

	/**
	 * Finds and removes the IItemInfo element from the structure.
	 *
	 * @param root
	 * @param nodeElement
	 */
	public static void removeElementFromStructure(final Group root, final IItemInfo nodeElement) {
		boolean removed = false;
		if (nodeElement != null && nodeElement.getParent() != null) {
			List<Group> groupList = root.getGroupList().getGroup();
			for (Group group : groupList) {
				removed = removeGroupFromStrucutre(group, nodeElement);
			}
			if (!removed) {
				removeServerFromStructure(root, nodeElement);
			}
		} else {
			root.getGroupList().getGroup().clear();
		}
	}

	/**
	 * Converts a IGroupInfo to an entity Group to be added to the XML file.
	 *
	 * @param factory
	 * @param groupInfo
	 * @return
	 */
	public static Group toGroup(final ObjectFactory factory, final IGroupInfo groupInfo) {
		Group group = factory.createGroup();
		group.setName(groupInfo.getName());
		group.setGroupList(new GroupList());
		String parentName = groupInfo.getParent() != null ? groupInfo.getParent().getName() : ""; //$NON-NLS-1$
		group.setParent(parentName);
		return group;
	}

	/**
	 * Converts a IServerInfo to an entity Server to be added to the XML file.
	 *
	 * @param factory
	 * @param serverInfo
	 * @return
	 */
	public static Server toServer(final ObjectFactory factory, final IServerInfo serverInfo) {
		String name = serverInfo.getName();
		int port = serverInfo.getAppServerPort();
		String serverType = ""; // serverInfo.getServerType();
		String timeStamp = ""; // serverInfo.getVersion().getTimestamp();
		String version = serverInfo.getVersion();

		Server server = factory.createServer();
		URI address = serverInfo.getAddress();

		IItemInfo parent = serverInfo.getParent();
		server.setBuild(version);
		server.setPort(port);
		server.setAddress(address.toString());
		server.setName(name);
		server.setServerType(serverType);
		server.setTimeStamp(timeStamp);
		if (parent != null) {
			server.setParentName(parent.getName());
		}
		return server;
	}

	private static Group addGroup(final ObjectFactory serverTreeFactory, final Group serverTreeRoot,
			final Group fParentGroup, final IItemInfo nodeElement) {
		Group groupToReturn = null;
		Group parentGroup = fParentGroup;
		IGroupInfo groupInfo = (IGroupInfo) nodeElement;
		Group group = ExportTool.toGroup(serverTreeFactory, groupInfo);
		GroupList groupList = null;
		if (parentGroup == null) {
			parentGroup = serverTreeRoot;
		}
		groupList = parentGroup.getGroupList();
		Group tmpGroup = findGroupByName(groupList, group.getName());
		if (tmpGroup == null) {
			groupList.getGroup().add(group);
			group.setParent(parentGroup.getName());
			groupToReturn = group;
		} else {
			groupToReturn = tmpGroup;
		}
		return groupToReturn;
	}

	private static Group addServer(final ObjectFactory serverTreeFactory, final Group serverTreeRoot,
			final Group fParentGroup, final IItemInfo nodeElement) {
		Server server = ExportTool.toServer(serverTreeFactory, (IServerInfo) nodeElement);
		List<Server> serverList = null;
		Group parentGroup = fParentGroup;
		if (parentGroup == null) {
			parentGroup = serverTreeRoot;
		}
		serverList = parentGroup.getServer();
		serverList.add(server);
		server.setParentName(parentGroup.getName());

		return parentGroup;
	}

	private static boolean removeElementFromGroup(final GroupList groupList, final IItemInfo nodeElement) {
		boolean removed = false;
		for (Group group : groupList.getGroup()) {
			removed = removeFromServerList(group.getServer(), nodeElement);
			if (!removed) {
				removed = removeElementFromGroup(group.getGroupList(), nodeElement);
				if (removed) {
					break;
				}
			}
		}
		return removed;
	}

	private static boolean removeFromServerList(final List<Server> serverList, final IItemInfo nodeElement) {
		boolean removed = false;
		String nodeName = nodeElement.getName();
		for (Server childServer : serverList) {
			String serverName = childServer.getName();
			if (serverName.equals(nodeName)) {
				serverList.remove(childServer);
				removed = true;
				break;
			}
		}
		return removed;
	}

	private static boolean removeGroupFromStrucutre(final Group rootGroup, final IItemInfo nodeElement) {
		List<Group> rootGroupList = rootGroup.getGroupList().getGroup();
		boolean removed = false;
		String nodeName = nodeElement.getName();
		for (Group childGroup : rootGroupList) {
			String groupName = childGroup.getName();
			if (groupName.equals(nodeName)) {
				rootGroupList.remove(childGroup);
				removed = true;
				break;
			}

			removed = removeGroupFromStrucutre(childGroup, nodeElement);
		}

		return removed;
	}

	private static boolean removeServerFromStructure(final Group root, final IItemInfo nodeElement) {
		boolean removed = removeFromServerList(root.getServer(), nodeElement);
		if (!removed) {
			removed = removeElementFromGroup(root.getGroupList(), nodeElement);
		}
		return removed;
	}

	final Location rootConfig = Platform.getConfigurationLocation();

	// private HashMap<String, IServerInfo> convertToMap(Set<Entry<String,
	// IServerInfo>> serversSet) {
	// HashMap<String, IServerInfo> map = new HashMap<String, IServerInfo>();
	// for (Entry<String, IServerInfo> node : serversSet) {
	// String serverName = node.getKey();
	// IServerInfo serverInfo = node.getValue();
	// map.put(serverName, serverInfo);
	// }
	// return map;
	// }

	// private void writeExternal(ObjectOutput out, HashMap<String, IItemInfo>
	// serversMap) throws IOException {
	// out.writeLong(serialVersionUID);
	// out.writeObject(serversMap);
	// out.flush();
	// }
}
