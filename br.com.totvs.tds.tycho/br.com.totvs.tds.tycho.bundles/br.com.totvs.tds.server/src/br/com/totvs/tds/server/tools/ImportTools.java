package br.com.totvs.tds.server.tools;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.model.GroupInfo;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.Server;
import br.com.totvs.tds.server.xml.XMLServerRoot;

/**
 * A class for the import server processes.
 * 
 * @author daniel.yampolschi
 *
 */
public class ImportTools {

	public static final String IS_DUPLICATED = "isDuplicated"; //$NON-NLS-1$

	public static boolean existsInServerView(final IItemInfo itemToFind, final IServerManager serverManager) {
		String name = itemToFind.getName();
		IItemInfo itemFound = null;
		if (itemToFind instanceof IGroupInfo) {
			itemFound = serverManager.getGroup(name);
		} else {
			itemFound = serverManager.getServer(name);
		}
		return itemFound != null;
	}

	/**
	 * Reads the itemsList looking for items that already exists in the server view.
	 * 
	 * @param itemsToFind
	 * @param serverManager
	 * @return duplicatedFound - List of duplicated items found.
	 */
	public static List<IItemInfo> findDuplicatedItems(final Object[] itemsToFind, final IServerManager serverManager) {
		List<IItemInfo> duplicatedFound = new ArrayList<IItemInfo>();
		for (Object selectedObject : itemsToFind) {
			IItemInfo selectedItem = (IItemInfo) selectedObject;
			boolean isDuplicated = existsInServerView(selectedItem, serverManager);
			if (isDuplicated && !duplicatedFound.contains(selectedObject)) {
				duplicatedFound.add(selectedItem);
			}
			selectedItem.setProperty(IS_DUPLICATED, isDuplicated);
			if (selectedItem instanceof IGroupInfo) {
				IGroupInfo group = (IGroupInfo) selectedItem;
				duplicatedFound.addAll(findDuplicatedItems(group.getChildren().toArray(), serverManager));
			}
		}
		return duplicatedFound;
	}

	public static XMLServerRoot importXml(final String filePath) {
		try {
			JAXBContext context = JAXBContext.newInstance(XMLServerRoot.class);
			Unmarshaller un = context.createUnmarshaller();
			XMLServerRoot serverRoot = (XMLServerRoot) un.unmarshal(new File(filePath));
			return serverRoot;
		} catch (JAXBException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Converts a Group object to a IGroupInfo.<br>
	 * It does not check the name integrity.
	 * 
	 * @param group - The Group object to be converted.
	 * @return A IGroupInfo.
	 */
	public static IGroupInfo toGroupInfo(final Group group) {
		return ImportTools.toGroupInfo(group, false);
	}

	/**
	 * Converts a Group object to a IGroupInfo.<br>
	 * The caller can choose to verify the server's name integrity.
	 * 
	 * @param group           - The Group to convert.
	 * @param checkServerName - Whether to verify the each server name integrity
	 *                        based on TDS name pattern.
	 * @return - A IGroupInfo
	 */
	public static IGroupInfo toGroupInfo(final Group group, final boolean checkServerName) {
		IGroupInfo info = new GroupInfo();
		info.setName(group.getName());
		addServersToGroupInfo(info, group.getServer(), checkServerName);
		addGroupsToGroupInfo(info, group.getGroupList().getGroup(), checkServerName);
		return info;
	}

	private static void addGroupsToGroupInfo(final IGroupInfo rootGroup, final List<Group> groupList,
			final boolean checkServerName) {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		for (Group group : groupList) {
			IGroupInfo groupInfo = serverManager.newGroup(group.getName());
			groupInfo.setParent(rootGroup);
			rootGroup.getChildren().add(groupInfo);
			addGroupsToGroupInfo(groupInfo, group.getGroupList().getGroup(), checkServerName);
			addServersToGroupInfo(groupInfo, group.getServer(), checkServerName);
		}
	}

	private static void addServersToGroupInfo(final IGroupInfo parentGroupInfo, final List<Server> serverList,
			final boolean checkServerName) {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		for (Server server : serverList) {
			IAppServerInfo serverInfo = null;
			try {
				serverInfo = serverManager.newAppServer(server.getName());
				//serverInfo.setServerType(server.getServerType());
				serverInfo.setAppServerPort(server.getPort());
				URI uri = new URI(server.getAddress());
				serverInfo.setAddress(uri);
				serverInfo.setConnected(false);
				serverInfo.setParent(parentGroupInfo);
				serverInfo.setVersion(getVersion(server));
				parentGroupInfo.addChild(serverInfo);
				if (checkServerName) {
					verifyNameChanged(serverInfo);
				}
			} catch (RuntimeException e1) {
				ServerActivator.logStatus(IStatus.ERROR, e1.getMessage(), e1);
				if (serverInfo != null ) { //&& e1.getErrorCode() == RuntimeException.SERVER_DUPLICATEDNAME) {
					serverInfo.setParent(parentGroupInfo);
					parentGroupInfo.getChildren().add(serverInfo);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	private static String getVersion(Server server) {
		System.out.println("ImportTools.getVersion()"); //$NON-NLS-1$
//		String buildName = server.getBuild();
//		ServerVersion[] versions = AppServerConnector.getAvailableServerVersion(server.getServerType());
//
//		for (ServerVersion version : versions) {
//			if (version.getName().equals(buildName)) {
//				return version;
//			}
//		}

		return null;
	}

	//TODO: tratamento de rename na importação
	private static void verifyNameChanged(final IAppServerInfo serverInfo) {
		System.out.println("ImportTools.verifyNameChanged()"); //$NON-NLS-1$
//		boolean nameChanged = (boolean) serverInfo.getProperty(IServerConstants.PROPERTY_NAME_CHANGED);
//		if (nameChanged) {
//			String newName = (String) serverInfo.getProperty(IServerConstants.PROPERTY_NEW_NAME);
//			String oldName = serverInfo.getName();
//			serverInfo.setName(newName);
//			String infoMessage = "Nome do servidor alterado de [%s] para [%s]";
//			ServerActivator.logStatus(IStatus.INFO,"Visão Servidor", infoMessage, oldName, newName);
//		}
	}

}
