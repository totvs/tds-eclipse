package br.com.totvs.tds.ui.server.views;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Visão "Servidores".
 *
 * @author acandido
 */
public class ServerView extends ViewPart {

	/**
	 * new The ID of the view as specified by the extension.
	 */
	public static final String VIEW_ID = "br.com.totvs.tds.ui.server.views.serverView"; //$NON-NLS-1$

	private static final String ACTION_SELECT_MONITOR = "actionSelectMonitor"; // FIX: reavaliar o uso //$NON-NLS-1$
	private static final String ACTIVE_SERVERS = "activeServers"; //$NON-NLS-1$
	private static final String CURRENT_SERVER = "currenteServer"; //$NON-NLS-1$

	private static final String SERVER = "server"; //$NON-NLS-1$
	private final int CONNECT_ALL_SERVERS = 2;

	private final int CONNECT_LAST_SERVER_ONLY = 1;

	private IMemento memento = null;

	private ISelectionChangedListener selectionListener;

	private Composite superParent;

	private final FormToolkit toolkit = new FormToolkit(Display.getCurrent());
	private TreeViewer viewer;

	private PropertyChangeListener propertyChangeListener;

	/**
	 * The constructor.
	 */
	public ServerView() {

	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize it.
	 *
	 * @param parent - The parent composite
	 */
	@Override
	public final void createPartControl(final Composite parent) {
		superParent = parent;
		Composite container = toolkit.createComposite(parent, SWT.NONE);
		toolkit.paintBordersFor(container);
		container.setLayout(new FillLayout(SWT.HORIZONTAL));

		ILabelDecorator decorator = PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator();
		DecoratingLabelProvider labelProvider = new DecoratingLabelProvider(new ServerViewLabelProvider(), decorator);

		viewer = new TreeViewer(container, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		Tree tree = viewer.getTree();
		viewer.setLabelProvider(labelProvider);
		viewer.setContentProvider(new ServerViewContentProvider());
		toolkit.adapt(tree);
		toolkit.paintBordersFor(tree);
		viewer.setComparator(new ServerViewerComparator(viewer));
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		viewer.setInput(new Object[] { serverManager.getItems() });
		viewer.expandToLevel(2);

		initializeActions();
		initializeContextMenu();

		hookNotifications();
		hookDoubleClickAction();
		hookSelectionMode();
		hookClickAction();

		getSite().setSelectionProvider(viewer);

		// restaura estado da sess�o�o anterior
		if (memento != null) {
			Boolean state = memento.getBoolean(ACTION_SELECT_MONITOR);
			if (state != null) {
				// actionSelectToMonitor.setChecked(state);
			} else {
				state = false;
			}

			if (!state) {
				unhookClickAction();
			}

			IMemento mementoNode = memento.getChild(ACTIVE_SERVERS);
			List<String> targetServers = new ArrayList<String>();

			if (mementoNode != null) {
				IMemento[] servers = mementoNode.getChildren();
				for (int i = 0; i < servers.length; i++) {
					IMemento serverMemento = servers[i];
					targetServers.add(serverMemento.getString("name")); //$NON-NLS-1$
				}
			}

			mementoNode = memento.getChild(CURRENT_SERVER);
			String currentServer = (mementoNode == null) ? Messages.EMPTY_STRING : mementoNode.getString("name"); // $NON-NLS-2$ //$NON-NLS-1$
			ServerStateLoaderJob serverStateLoaderJob = new ServerStateLoaderJob(targetServers, currentServer);

			IWorkbenchSiteProgressService siteService = getSite().getAdapter(IWorkbenchSiteProgressService.class);
			siteService.schedule(serverStateLoaderJob, 0 /* now */, true /* use the half-busy cursor in the part */);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.WorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		unhookNotifications();

		toolkit.dispose();
		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") final Class adapter) {
		if (adapter.equals(TreeViewer.class)) {
			return viewer;
		}
		return super.getAdapter(adapter);
	}

	private void hookClickAction() {
		viewer.addSelectionChangedListener(selectionListener);
	}

	/**
	 * Define comportamento para o duplo-click.
	 */
	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			@SuppressWarnings("unused")
			@Override
			public void doubleClick(final DoubleClickEvent event) {
				try {
					IItemInfo itemSelect = null;

					IConfigurationElement[] commandList = null;
//					if (event.getSelection() instanceof TreeSelection) {
//						TreeSelection treeSelection = (TreeSelection) event.getSelection();
//						if (treeSelection.size() == 1) {
//							itemSelect = (IItemInfo) treeSelection.getFirstElement();
//							commandList = ServerHelper.getCommandList(itemSelect, true);
//						}
//					}

					if (commandList != null && commandList.length > 0) {
						IServiceLocator serviceLocator = PlatformUI.getWorkbench();

						Object commandServiceAsObject = serviceLocator.getService(ICommandService.class);
						ICommandService commandService = (ICommandService) commandServiceAsObject;
						Object handlerServiceAsObject = serviceLocator.getService(IHandlerService.class);
						IHandlerService handlerService = (IHandlerService) handlerServiceAsObject;

						// obtem o comando
						Command command = null;

						// mapa de parametros
						Map<String, String> parameters = new HashMap<String, String>();

//						if (itemSelect.getParent() instanceof LogixServerInfo) {
//							parameters.put("br.com.totvs.tds.ui.server.command.loginDialog", //$NON-NLS-1$
//									"br.com.totvs.tds.ui.server.tools.LogixLoginDialog"); //$NON-NLS-1$
//						} else if (itemSelect.getParent() instanceof ProtheusServerInfo) {
//							parameters.put("br.com.totvs.tds.ui.server.command.loginDialog", //$NON-NLS-1$
//									"br.com.totvs.tds.ui.server.tools.ProtheusLoginDialog"); //$NON-NLS-1$
//						}
						if (itemSelect instanceof IServerInfo) {
							command = commandService.getCommand("br.com.totvs.tds.ui.server.command.editItem"); //$NON-NLS-1$
						} else if (itemSelect instanceof IEnvironmentInfo) {
							command = commandService.getCommand("br.com.totvs.tds.ui.server.command.login"); //$NON-NLS-1$
							parameters.put("br.com.totvs.tds.ui.server.command.login.server", itemSelect.getParent() //$NON-NLS-1$
									.getName());
							parameters.put("br.com.totvs.tds.ui.server.command.login.environment", //$NON-NLS-1$
									itemSelect.getName());
						}
						// monta a parametrizacao do comando
						ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, parameters);

						// executa o comando parametrizado
						if ((handlerService != null) && (command != null)) {
							handlerService.executeCommand(pc, null);
						}

						viewer.refresh(false);
						IDecoratorManager decoratorManager = PlatformUI.getWorkbench().getDecoratorManager();
						decoratorManager.update(ServerViewDecorator.SERVER_DECORATOR_ID);
					}
				} catch (Exception e) {
					ServerActivator.logStatus(IStatus.ERROR, Messages.ServerView_server_view, e.getMessage(), e);
				}
			}
		});
	}

	/**
	 * Tratamento de notificações.
	 */
	private void hookNotifications() {
		propertyChangeListener = new PropertyChangeListener() {

			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				if (evt.getPropertyName().equals("_refresh_")) { //$NON-NLS-1$
					if (evt.getNewValue() == null) {
						viewer.refresh();
					} else {
						viewer.refresh(evt.getNewValue());
					}
				}

				if (evt.getPropertyName().equals("add_children")) { //$NON-NLS-1$
					IItemInfo parent = (IItemInfo) evt.getOldValue();
					viewer.refresh(parent);
					if (!viewer.getExpandedState(parent)) {
						viewer.expandToLevel(parent, 1);
					}

					TreeItem[] items = viewer.getTree().getItems();
					for (int i = 0; i < items.length; i++) {
						TreeItem treeItem = items[i];
						Object data = treeItem.getData();
						if (data.equals(evt.getNewValue())) {
							IAppServerInfo server = (IAppServerInfo) data;
							if ((boolean) server.getProperty(IServerConstants.IMMEDIATE_CONNECTION)) {
								viewer.getTree().setSelection(treeItem);

							}
						}
					}
				} else if (evt.getPropertyName().equals("remove_children")) { //$NON-NLS-1$
					IItemInfo parent = (IItemInfo) evt.getOldValue();
					if (!viewer.getExpandedState(parent)) {
						viewer.expandToLevel(parent, 1);
					}
					viewer.refresh(parent);
				} else if (evt.getPropertyName().equals("connected")) { //$NON-NLS-1$
					Boolean connected = Boolean.valueOf(evt.getNewValue().toString());
					IAppServerInfo server = (IAppServerInfo) evt.getSource();

					if (connected) {
						IServiceLocator serviceLocator = PlatformUI.getWorkbench();
						ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

						server.loadSlaves(lsService);
					}

					viewer.refresh(server);
				} else {
					viewer.refresh();
				}
			}
		};

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		serverManager.addPropertyChangeListener(propertyChangeListener);
	}

	private void hookSelectionMode() {
		selectionListener = new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
//				try {
////					monitor.setSelection(event.getSelection());
////					monitor.execute(new ExecutionEvent());
//				} catch (ExecutionException e) {
//					e.printStackTrace();
//				}
			}
		};
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite,
	 * org.eclipse.ui.IMemento)
	 */
	@Override
	public void init(IViewSite site, IMemento memento) throws PartInitException {
		super.init(site, memento);

		this.memento = memento;
	}

	/**
	 * Initialize the actions.
	 */
	private void initializeActions() {
//		actionSelectToMonitor = new Action(Messages.ServerView_1, IAction.AS_CHECK_BOX) {
//
//			@Override
//			public void run() {
//				if (this.isChecked()) {
//					hookClickAction();
//				} else {
//					unhookClickAction();
//				}
//			}
//		};
//		actionSelectToMonitor.setImageDescriptor(ResourceManager.getPluginImageDescriptor("br.com.totvs.tds.ui.server", //$NON-NLS-1$
//				"icons/selection.png")); //$NON-NLS-1$

	}

	private void initializeContextMenu() {
		Menu menu = viewer.getControl().getMenu();
		MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$

//		menuMgr.addMenuListener(new IMenuListener() {
//			@Override
//			public void menuAboutToShow(final IMenuManager manager) {
//				ServerView.this.fillContextMenu(manager);
//			}
//		});
//
		menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
//
		getSite().registerContextMenu(menuMgr, viewer);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
	 */
	@Override
	public void saveState(IMemento memento) {
		IPreferenceStore preferenceStore = ServerUIActivator.getDefault().getPreferenceStore();
		int reconnect = preferenceStore.getInt(IServerConstants.RECONNECT_POLICIES);
		List<IServerInfo> serversToSave = new ArrayList<IServerInfo>();
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		memento.putBoolean(ACTION_SELECT_MONITOR, false); // actionSelectToMonitor.isChecked()
		switch (reconnect) {
		case CONNECT_LAST_SERVER_ONLY:
			serversToSave.add(serverManager.getCurrentServer());
			break;
		case CONNECT_ALL_SERVERS:
			serversToSave.addAll(serverManager.getActiveServers());
			break;
		default: // DONT_CONNECT_SERVERS
			break;
		}

		if (!serversToSave.isEmpty()) {
			IMemento activeServers = memento.createChild(ACTIVE_SERVERS);
			for (IServerInfo serverInfo : serversToSave) {
				IMemento server = activeServers.createChild(SERVER);

				server.putString("id", serverInfo.getId().toString()); //$NON-NLS-1$
				server.putString("name", serverInfo.getName()); //$NON-NLS-1$
				server.putBoolean("connected", true); //$NON-NLS-1$
			}

			IAppServerInfo serverInfo = serverManager.getCurrentServer();
			activeServers = memento.createChild(CURRENT_SERVER);
			activeServers.putString("id", serverInfo.getId().toString()); //$NON-NLS-1$
			activeServers.putString("name", serverInfo.getName()); //$NON-NLS-1$
		}

		super.saveState(memento);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.WorkbenchPart#showBusy(boolean)
	 */
	@Override
	public void showBusy(boolean busy) {
		boolean enabled = !busy;

		superParent.setEnabled(enabled);

		getViewSite().getActionBars().updateActionBars();
	}

	private void unhookClickAction() {
		viewer.removeSelectionChangedListener(selectionListener);
	}

	/**
	 * Cancela o tratamento de eventos.
	 */
	private void unhookNotifications() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		serverManager.removePropertyChangeListener(propertyChangeListener);

		propertyChangeListener = null;
	}

}