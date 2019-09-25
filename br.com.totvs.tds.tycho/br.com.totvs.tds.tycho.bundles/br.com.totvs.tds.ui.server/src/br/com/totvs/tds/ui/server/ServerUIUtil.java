package br.com.totvs.tds.ui.server;

import java.util.List;

import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.tools.ExportTool;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.ObjectFactory;
import br.com.totvs.tds.ui.TDSUtil;

/**
 * Classe utilit�ria de ServerUI.
 *
 * @author leo.watanabe
 */
public final class ServerUIUtil {

	private static Listener verifyNumbersOnlyListener = new Listener() {

		@Override
		public void handleEvent(final Event e) {
			String string = e.text;
			for (int i = 0; i < string.length(); i++) {
				if (string.charAt(i) < '0' || string.charAt(i) > '9') {
					e.doit = false;
					return;
				}
			}
		}
	};

	/**
	 * Check all children from the selected node in the checkboxTreeViewer.
	 *
	 * @param nodeElement
	 * @param factory
	 * @param serverTreeRoot
	 * @param parentGroup
	 * @param checkboxTreeViewer
	 */
	private static void checkChildren(final IItemInfo nodeElement, final ObjectFactory factory,
			final Group serverTreeRoot, final CheckboxTreeViewer checkboxTreeViewer) {
		if (nodeElement instanceof IGroupInfo) {
			IGroupInfo groupInfo = (IGroupInfo) nodeElement;
			Group parentGroup = ExportTool.findGroupByName(serverTreeRoot.getGroupList(), groupInfo.getName());
			// if(parentGroup == null) {
			// parentGroup = ExportTool.toGroup(factory, groupInfo);
			// }
			List<IItemInfo> children = groupInfo.getChildren();
			for (IItemInfo child : children) {
				if (checkboxTreeViewer != null) {
					checkboxTreeViewer.setChecked(child, true);
				}
				ExportTool.addElementToStructure(factory, serverTreeRoot, parentGroup, child);
				if (child instanceof IGroupInfo) {
					checkChildren(child, factory, serverTreeRoot, checkboxTreeViewer);
				}
			}
		}
	}

	/**
	 * Check all parents from the selected node in the checkboxTreeViewer.
	 *
	 * @param nodeElement
	 * @param factory
	 * @param serverTreeRoot
	 * @param checkboxTreeViewer
	 * @return
	 */
	public static Group checkParents(final IItemInfo nodeElement, final ObjectFactory factory,
			final Group serverTreeRoot, final CheckboxTreeViewer checkboxTreeViewer) {
		IItemInfo parentInfo = nodeElement.getParent();
		Group group = null;
		if (parentInfo != null) {
			Group parentGroup = checkParents(parentInfo, factory, serverTreeRoot, checkboxTreeViewer);
			group = ExportTool.addElementToStructure(factory, serverTreeRoot, parentGroup, nodeElement);
			if (checkboxTreeViewer != null) {
				checkboxTreeViewer.setChecked(parentInfo, true);
			}
		} else {
			// group = ExportTool.findGroupById(serverTreeRoot.getGroupList(),
			// nodeElement.getId().toString());
			// if(serverTreeRoot.getId() == null) {
			// serverTreeRoot.setId(nodeElement.getId().toString());
			// }
			group = serverTreeRoot;
		}
		return group;
	}

	/**
	 * Any place that uses a SmartClient combo can call this method.<br>
	 * This automatically checks for the registered SmartClients to fill the
	 * Combo.<br>
	 * Note that this method must be called from the UI Thread
	 *
	 * @param combo
	 */
	private static void fillComboSmartClient(final Combo combo) {
		System.out.println("ServerUIUtil.fillComboSmartClient()"); //$NON-NLS-1$
//		List<SmartClientConfig> allSmartClients = SmartClientUtil.getAllSmartClientsRegistered();
//		List<String> items = Arrays.asList(combo.getItems());
//		for (SmartClientConfig smartClientConfig : allSmartClients) {
//			String smartClientToAdd = smartClientConfig.getName();
//			boolean itemFound = false;
//			for (String existentItem : items) {
//				if (existentItem.equals(smartClientToAdd)) {
//					itemFound = true;
//					break;
//				}
//			}
//			if (!itemFound) {
//				combo.add(smartClientToAdd);
//			}
//		}
	}

	private static boolean hasChildrenChecked(final IItemInfo parentInfo, final CheckboxTreeViewer checkboxTreeViewer) {
		boolean hasChildrenChecked = false;
		if (checkboxTreeViewer != null) {
			if (parentInfo instanceof IGroupInfo) {
				IGroupInfo groupInfo = (IGroupInfo) parentInfo;
				List<IItemInfo> children = groupInfo.getChildren();
				for (IItemInfo childInfo : children) {
					hasChildrenChecked = checkboxTreeViewer.getChecked(childInfo);
					if (hasChildrenChecked) {
						break;
					}
				}
			}
		}
		return hasChildrenChecked;
	}

	/**
	 * Gets the host and check if it is localhost or 127.0.0.*
	 *
	 * @param serverInfo
	 * @return
	 */
	public static boolean isLocalhost(final IServerInfo serverInfo) {
		String host = serverInfo.getAddress().getHost();
		return host.equalsIgnoreCase("localhost") || host.startsWith("127.0.0."); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Opens a Folder Dialog. Must be called whithin the UI Thread.
	 *
	 * @param title      - The dialog's title.
	 * @param filterPath - Sets the path that the dialog will use to filter the
	 *                   directories it shows to the argument, which may be null. If
	 *                   the string is null, then the operating system's default
	 *                   filter path will be used. <br>
	 *                   <br>
	 *                   Note that the path string is platform dependent. For
	 *                   convenience, either '/' or '\' can be used as a path
	 *                   separator.
	 *
	 *
	 * @return a string describing the absolute path of the selected directory, or
	 *         null if the dialog was cancelled or an error occurred
	 */
	public static String openFolderDialog(final Shell shell, final String title, final String filterPath) {
		return TDSUtil.directoryDialog(shell, title, null, filterPath);
	}

	/**
	 * Uncheck all parents from the selected node in the informed checkbosTree
	 * Viewer.
	 *
	 * @param nodeElement
	 * @param serverTreeRoot
	 * @param checkboxTreeViewer
	 */
	private static void unCheckParents(final IItemInfo nodeElement, final Group serverTreeRoot,
			final CheckboxTreeViewer checkboxTreeViewer) {
		IItemInfo parentInfo = nodeElement.getParent();
		ExportTool.removeElementFromStructure(serverTreeRoot, nodeElement);
		if (parentInfo != null && !hasChildrenChecked(parentInfo, checkboxTreeViewer)) {
			if (checkboxTreeViewer != null) {
				checkboxTreeViewer.setChecked(parentInfo, false);
			}
			unCheckParents(parentInfo, serverTreeRoot, checkboxTreeViewer);
		}
	}

	/**
	 * Obt�m uma inst�ncia do listener de validação num�rica.
	 *
	 * @return listener de validação num�rica.
	 */
	public static Listener verifyNumbersOnlyListener() {
		return verifyNumbersOnlyListener;
	}

	private ServerUIUtil() {
	}

}
