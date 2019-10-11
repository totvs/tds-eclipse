package br.com.totvs.tds.ui.server.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.tools.ExportTool;
import br.com.totvs.tds.server.tools.ImportTools;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.ObjectFactory;
import br.com.totvs.tds.server.xml.Server;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;

/**
 * Assistente de importação de servidores.
 *
 * @author acandido
 */
public class ImportServersWizard extends Wizard {

	private final ServerImporExportAttributesVO impAttributes;
	private ImportServerMergePage merge;
	private SelectImportServersPage select;

	public ImportServersWizard(final ServerImporExportAttributesVO attributes) {
		setWindowTitle(Messages.ImportServersWizard_import_server_list);
		impAttributes = attributes;
	}

	private void addGroupsToStructure(final Group targetGroup, final List<Group> groupInfoList) {
		String targetGroupName = targetGroup.getName();
		for (Group group : groupInfoList) {
			String parentName = group.getParent();
			if (parentName == null || parentName.isEmpty()) {
				targetGroup.getGroupList().add(group);
			}
			Group parentGroup = findParent(groupInfoList, parentName);
			if (parentGroup != null) {
				parentGroup.getGroupList().add(group);
			} else {
				targetGroup.getGroupList().add(group);
				group.setParent(targetGroupName);
			}
		}
	}

	// @Override
	@Override
	public void addPages() {
		select = new SelectImportServersPage(impAttributes);
		merge = new ImportServerMergePage(impAttributes);
		addPage(select);
		addPage(merge);
	}

	private void addServersToStructure(final Group targetGroup, final List<Server> serverInfoList,
			final List<Group> groupInfoList) {
		String targetGroupName = targetGroup.getName();
		for (Server server : serverInfoList) {
			String parentName = server.getParentName();
			if (parentName == null) {
				targetGroup.getServer().add(server);
			} else {
				Group parentGroup = findParent(groupInfoList, parentName);
				if (parentGroup != null) {
					parentGroup.getServer().add(server);
				} else {
					targetGroup.getServer().add(server);
					server.setParentName(targetGroupName);
				}
			}
		}
	}

	@Override
	public boolean canFinish() {
		if (impAttributes.getRepeatedServerNames().size() > 0
				&& impAttributes.getServerMergeOption().equals(ServerImporExportAttributesVO.ServerMergeOption.NONE)) {
			return false;
		}

		if (!select.isPageComplete()) {
			return false;
		}

		return impAttributes.validAttributes() && (impAttributes.getTargetNode() != null);
	}

	protected void doFinish(final IProgressMonitor monitor) {
		Display.getDefault().asyncExec(new Runnable() {
			private void addChildren(final IGroupInfo targetNode, final List<IItemInfo> children) {
				for (IItemInfo itemInfo : children) {
					try {
						targetNode.addChild(itemInfo);
						itemInfo.setParent(targetNode);
					} catch (RuntimeException e) {
						String name = itemInfo.getName();
						IGroupInfo groupInfo = targetNode;
						if (!targetNode.getName().equals(name)) {
							groupInfo = ExportTool.findNode(targetNode, itemInfo.getName());
						}
						if (groupInfo != null) {
							List<IItemInfo> children2 = ((IGroupInfo) itemInfo).getChildren();
							addChildren(groupInfo, children2);
						}
					}
				}
			}

			@Override
			public void run() {

				// Adicicionar estrutura selecionada e verificar cada nome, se ja existir
				// adicionar um identificador
				// icremental no nome, da mesma forma que o windows faz.

				IServerManager serverManager = ServerActivator.getDefault().getServerManager();
				IGroupInfo targetNode = impAttributes.getTargetNode();
				ObjectFactory factory = new ObjectFactory();
				Group targetGroup = ExportTool.toGroup(factory, targetNode);
				Object[] selectedItems = impAttributes.getSelectedItems();
				targetGroup = mountSelectedStructure(targetGroup, selectedItems, factory);
				IGroupInfo mainGroupInfo = ImportTools.toGroupInfo(targetGroup, true);
				List<IItemInfo> children = mainGroupInfo.getChildren();
				removeDuplicated(children, serverManager);
				addChildren(targetNode, children);
				serverManager.hookChangeListener(serverManager.getItems());
			}
		});
	}

	private Group findParent(final List<Group> groupInfoList, final String parentName) {
		Group groupFound = null;
		for (Group group : groupInfoList) {
			String groupName = group.getName();
			if (groupName.equals(parentName)) {
				groupFound = group;
				break;
			}
		}
		return groupFound;
	}

	private Object[] getAllItemsAsArray(final List<IItemInfo> children, final ArrayList<IItemInfo> arrayListToReturn) {
		for (IItemInfo child : children) {
			arrayListToReturn.add(child);
			if (child instanceof IGroupInfo) {
				IGroupInfo groupInfo = (IGroupInfo) child;
				getAllItemsAsArray(groupInfo.getChildren(), arrayListToReturn);
			}
		}
		return arrayListToReturn.toArray();
	}

	@Override
	public IWizardPage getNextPage(final IWizardPage page) {
		merge.loadRepeatedServerNames();
		return super.getNextPage(page);
	}

	protected Group mountSelectedStructure(final Group targetGroup, final Object[] selectedItems,
			final ObjectFactory factory) {
		List<Server> serverInfoList = new ArrayList<Server>();
		List<Group> groupInfoList = new ArrayList<Group>();
		for (Object object : selectedItems) {
			if (object instanceof IServerInfo) {
				Server server = ExportTool.toServer(factory, (IServerInfo) object);
				serverInfoList.add(server);
			} else if (object instanceof IGroupInfo) {
				Group group = ExportTool.toGroup(factory, (IGroupInfo) object);
				groupInfoList.add(group);
			}
		}
		addServersToStructure(targetGroup, serverInfoList, groupInfoList);
		addGroupsToStructure(targetGroup, groupInfoList);

		return targetGroup;
	}

	// @Override
	@Override
	public boolean performFinish() {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(final IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(monitor);
				} catch (Exception e) {
					throw new InvocationTargetException(e);
				} catch (Throwable e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			// Util.logError(Messages.ImportServersView_1);
			return false;
		} catch (InvocationTargetException e) {
			e.getTargetException();
			// Util.logError(Messages.ImportServersView_2);
			// Util.logError(realException.getMessage());
			// MessageDialog.openError(getShell(), Messages.ImportServersView_3,
			// realException.getMessage());
			return false;
		}
		return true;
	}

	protected void removeDuplicated(final List<IItemInfo> newItems, final IServerManager serverManager) {
		Object[] allItemsAsArray = getAllItemsAsArray(newItems, new ArrayList<IItemInfo>());
		for (Object object : allItemsAsArray) {
			IItemInfo item = (IItemInfo) object;
			boolean itemFound = ImportTools.existsInServerView(item, serverManager);
			if (itemFound) {
				IItemInfo existentItem = null;
				if (item instanceof IGroupInfo) {
					existentItem = serverManager.getGroup(item.getName());
				} else {
					existentItem = serverManager.getServer(item.getName());
				}
				serverManager.remove(existentItem);
			}
		}
	}
}
