package br.com.totvs.tds.ui.server.views;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
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
import org.eclipse.ui.handlers.RegistryToggleState;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Visão "Servidores".
 *
 * @author acandido
 */
public class ServerView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String VIEW_ID = "br.com.totvs.tds.ui.server.views.serverView"; //$NON-NLS-1$

	private static final String ACTION_SELECT_MONITOR = "actionSelectMonitor";

	private IMemento memento = null;

	private ISelectionChangedListener selectionListener;

	private Composite superParent;

	private final FormToolkit toolkit = new FormToolkit(Display.getCurrent());
	private TreeViewer viewer;

	private PropertyChangeListener propertyChangeListener;

	private Command actionSelectToMonitor;

	private IDoubleClickListener doubleClickListener;

	private Command sortTreeCommand;

	private IExecutionListener sortTreeListener;

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
		hookSortTree();

		getSite().setSelectionProvider(viewer);

		// restaura estado da sessão anterior
		if (memento != null) {
			Boolean state = memento.getBoolean(ACTION_SELECT_MONITOR);
			if (state == null) {
				state = false;
			}
			actionSelectToMonitor.getState(RegistryToggleState.STATE_ID).setValue(state);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.WorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		unhookSortTree();
		unhookDoubleClickAction();
		unhookSelectionMode();
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

	/**
	 * Define comportamento para o duplo-click.
	 */
	private void hookDoubleClickAction() {
		doubleClickListener = new IDoubleClickListener() {
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
						if (itemSelect instanceof IAppServerInfo) {
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
					ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
				}
			}
		};

		viewer.addDoubleClickListener(doubleClickListener);
	}

	/**
	 * Tratamento de notificações.
	 */
	private void hookNotifications() {
		propertyChangeListener = new PropertyChangeListener() {

			private TreeItem getTreeItem(TreeItem[] items, IAppServerInfo server) {
				for (int i = 0; i < items.length; i++) {
					TreeItem treeItem = items[i];
					Object data = treeItem.getData();
					if (data.equals(server)) {
						if ((boolean) server.getProperty(IServerConstants.IMMEDIATE_CONNECTION)) {
							return treeItem;
						}
					} else {
						return getTreeItem(treeItem.getItems(), server);
					}
				}

				return null;
			}

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

					IAppServerInfo server = (IAppServerInfo) evt.getNewValue();
					if ((boolean) server.getProperty(IServerConstants.IMMEDIATE_CONNECTION)) {
						TreeItem[] items = viewer.getTree().getItems();
						TreeItem treeItem = getTreeItem(items, server);

						if (treeItem != null) {
							viewer.getTree().setSelection(treeItem);

							IServiceLocator serviceLocator = PlatformUI.getWorkbench();
							ICommandService commandService = serviceLocator.getService(ICommandService.class);

							Command command = commandService
									.getCommand("br.com.totvs.tds.ui.server.commands.connectCommand"); //$NON-NLS-1$

							ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, null);

							IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);
							try {
								handlerService.executeCommand(pc, null);
							} catch (ExecutionException e) {
								ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
							} catch (NotDefinedException e) {
								ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
							} catch (NotEnabledException e) {
								ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
							} catch (NotHandledException e) {
								ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
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
					IAppServerInfo server = (IAppServerInfo) evt.getSource();
					viewer.refresh(server);
				} else {
					viewer.refresh();
				}
			}
		};

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		serverManager.addPropertyChangeListener(propertyChangeListener);
	}

	private void hookSortTree() {
		sortTreeListener = new IExecutionListener() {

			@Override
			public void notHandled(final String commandId, final NotHandledException exception) {
				// TODO Auto-generated method stub

			}

			@Override
			public void postExecuteFailure(final String commandId, final ExecutionException exception) {
				// TODO Auto-generated method stub
			}

			@Override
			public void postExecuteSuccess(final String commandId, final Object returnValue) {
				final Boolean selection = (Boolean) returnValue;

				if (selection) {
					ViewerComparator sorter = new ViewerComparator() {
						/**
						 * Returns a negative, zero, or positive number depending on whether the first
						 * element is less than, equal to, or greater than the second element.
						 */
						@Override
						public int compare(Viewer viewer, Object e1, Object e2) {
							if (e1 instanceof IItemInfo) {
								String str1 = (((IItemInfo) e1).getName());
								String str2 = (((IItemInfo) e2).getName());
								return getComparator().compare(str1, str2);
							}

							return 0;
						}
					};

					viewer.setComparator(sorter);
				} else {
					viewer.setComparator(null);
				}

				viewer.refresh();
			}

			@Override
			public void preExecute(final String commandId, final ExecutionEvent event) {
				// TODO Auto-generated method stub
			}
		};

		sortTreeCommand.addExecutionListener(sortTreeListener);
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
		IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		ICommandService commandService = serviceLocator.getService(ICommandService.class);

		actionSelectToMonitor = commandService
				.getCommand("br.com.totvs.tds.ui.server.commands.monitorSelectionCommand"); //$NON-NLS-1$

		sortTreeCommand = commandService.getCommand("br.com.totvs.tds.ui.server.commands.sortTreeCommand"); //$NON-NLS-1$

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
//		IPreferenceStore preferenceStore = ServerUIActivator.getDefault().getPreferenceStore();
//		int reconnect = preferenceStore.getInt(IServerConstants.RECONNECT_POLICIES);

		boolean stateValue = ((Boolean) actionSelectToMonitor.getState(RegistryToggleState.STATE_ID).getValue())
				.booleanValue();
		memento.putBoolean(ACTION_SELECT_MONITOR, stateValue);

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

	private void unhookDoubleClickAction() {
		viewer.removeDoubleClickListener(doubleClickListener);
		doubleClickListener = null;
	}

	private void unhookNotifications() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		serverManager.removePropertyChangeListener(propertyChangeListener);

		propertyChangeListener = null;
	}

	private void unhookSelectionMode() {
		viewer.removeSelectionChangedListener(selectionListener);
		selectionListener = null;
	}

	private void unhookSortTree() {
		viewer.setComparator(null);

		sortTreeCommand.removeExecutionListener(sortTreeListener);
		sortTreeCommand = null;
	}

}