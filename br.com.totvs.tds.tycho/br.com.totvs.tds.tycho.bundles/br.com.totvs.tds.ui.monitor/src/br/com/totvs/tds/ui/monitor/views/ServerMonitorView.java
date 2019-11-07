package br.com.totvs.tds.ui.monitor.views;

import java.beans.PropertyChangeListener;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.dialogs.SearchPattern;
import org.eclipse.ui.handlers.RegistryToggleState;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.monitor.MonitorUIActivator;
import br.com.totvs.tds.ui.monitor.columns.MonitorColumn;
import br.com.totvs.tds.ui.monitor.jobs.ServerMonitorJob;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;
import br.com.totvs.tds.ui.monitor.model.ServerMonitor;
import br.com.totvs.tds.ui.monitor.providers.ServerMonitorViewContentProvider;
import br.com.totvs.tds.ui.monitor.providers.ServerMonitorViewLabelProvider;
import br.com.totvs.tds.ui.server.views.ServerView;

/**
 * Visão "Monitor de Servidores".
 *
 * @author eriky.kashivagui
 *
 */
public final class ServerMonitorView extends ConfigurableTreeViewPart
		implements IConfigurableColumnView, IJobChangeListener {

	// The view ID
	public static final String VIEW_ID = "br.com.totvs.tds.ui.monitor.views.serverMonitorView"; //$NON-NLS-1$

	private static final TreePath[] TREE_PATH_EMPTY_ARRAY = new TreePath[0];
	private static final int INTERVAL_DEFAULT = 30; // intervalo padrão 30 segundos

	private class TableFilter extends ViewerFilter {
		private SearchPattern searchPattern = new SearchPattern();

		@Override
		public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
			final String filter = textFilter.getText().trim().toUpperCase();
			boolean result = true;

			if (!(filter.isEmpty()) && (element instanceof IUserMonitor)) {
				final IUserMonitor serverMonitor = (IUserMonitor) element;

				searchPattern.setPattern(filter);
				result = searchPattern.matches(serverMonitor.getClientType().toUpperCase());
//						|| searchPattern.matches(serverMonitor.getConection().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getDbThread().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getEnvironment().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getInstructions().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getInstructionsXSeconds().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getMachine().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getMemory().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getObservations().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getProcedure().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getProgram().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getRPO().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getSID().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getThreadId().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getTimeElapsed().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getTimeInactivity().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getTotalIos().toUpperCase())
//						|| searchPattern.matches(serverMonitor.getUser().toUpperCase());
			}

			return result;
		}
	}

	private TreeViewer viewer;
	private Tree treeMonitor;
	private Text textFilter;

	private Map<String, IServerMonitor> itemsMonitor = new ConcurrentHashMap<String, IServerMonitor>();
	private Map<String, IServerMonitor> pinItemsMonitor = new HashMap<String, IServerMonitor>();

	private IExecutionListener executionListener;

	private Command actionSelectToMonitor;
	private Command actionRefreshMonitor;

	private ISelectionListener selectionListener;

	private ServerMonitorJob serverMonitorJob;

	private PropertyChangeListener propertyChangeListener;
	private int intervalScheduler = 30000; // valor inicial para 1a execução

	@Override
	public void createPartControl(final Composite parent) {
		parent.setLayout(new GridLayout(2, false));

		final Label lblFiltro = new Label(parent, SWT.NONE);
		lblFiltro.setText("Filtro");
		lblFiltro.setToolTipText("ATENÇÃO: Não utilize caracteres coringas \"*\" ou \"?\"");

		textFilter = new Text(parent, SWT.BORDER);
		textFilter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		textFilter.setToolTipText("O filtro é aplicado somente aos nomes dos servidores");
		textFilter.addKeyListener(new KeyListener() {

			@Override
			public void keyReleased(final KeyEvent e) {
				viewer.refresh();
			}

			@Override
			public void keyPressed(final KeyEvent e) {
			}
		});

		viewer = new TreeViewer(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.MULTI);
		viewer.addFilter(new TableFilter());
		treeMonitor = viewer.getTree();

		final GridData gdTreeMonitor = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);

		final int treeMonitorHeight = 616;
		gdTreeMonitor.heightHint = treeMonitorHeight;
		treeMonitor.setLayoutData(gdTreeMonitor);
		treeMonitor.setHeaderVisible(true);
		treeMonitor.setLinesVisible(true);
		treeMonitor.setEnabled(true);

//		treeMonitor.addTreeListener(new TreeListener() {
//			@Override
//			public void treeExpanded(final TreeEvent e) {
//				if (e.item instanceof TreeItem) {
//					((TreeItem) e.item).setExpanded(true);
//				}
//			}
//
//			@Override
//			public void treeCollapsed(final TreeEvent e) {
//			}
//		});

		final GridData gridData = new GridData();
		gridData.verticalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		viewer.getControl().setLayoutData(gridData);

		createColumns();

		viewer.setLabelProvider(new ServerMonitorViewLabelProvider());
		viewer.setContentProvider(new ServerMonitorViewContentProvider());
		viewer.setInput(itemsMonitor);

		initializeActions();
		hookNotifications();
		hookActions();

		getSite().setSelectionProvider(viewer);
	}

	@Override
	public final void init(final IViewSite site, final IMemento memento) throws PartInitException {
		super.init(site, memento);

		if (memento != null) {
			final IMemento child = memento.getChild("pinnedServers");

			if (child != null) {
				final Boolean empty = child.getBoolean("empty");

				if ((empty != null) && !empty) {
					final IMemento[] pinnedServers = child.getChildren("server");
					final IServerManager serverManager = MonitorUIActivator.getDefault().getServerManager();

					for (final IMemento pinnedServer : pinnedServers) {
						final String name = pinnedServer.getString("name");
						final IAppServerInfo server = serverManager.getServer(name);

						addItem(pinItemsMonitor, server);
					}
				}

				final Integer intervalScheduler = child.getInteger("intervalScheduler");

				if ((intervalScheduler != null) && (intervalScheduler > 0)) {
					this.intervalScheduler = intervalScheduler;
				} else {
					this.intervalScheduler = INTERVAL_DEFAULT;
				}
			}
		}
	}

	@Override
	public final void saveState(final IMemento memento) {
		super.saveState(memento);

		IMemento child = memento.getChild("pinnedServers");
		if (child == null) {
			child = memento.createChild("pinnedServers");
		}
		child.putBoolean("empty", pinItemsMonitor.isEmpty());
		child.putInteger("intervalScheduler", intervalScheduler);

		for (final Entry<String, IServerMonitor> monitor : pinItemsMonitor.entrySet()) {
			final IMemento serverChild = child.createChild("server",
					monitor.getValue().getServerInfo().getId().toString());
			serverChild.putString("name", monitor.getKey());
		}
	}

	private void initializeActions() {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ICommandService commandService = serviceLocator.getService(ICommandService.class);

		actionSelectToMonitor = commandService
				.getCommand("br.com.totvs.tds.ui.server.commands.monitorSelectionCommand"); //$NON-NLS-1$
		actionRefreshMonitor = commandService.getCommand("br.com.totvs.tds.ui.monitor.commands.refreshMonitor"); //$NON-NLS-1$
	}

	private void hookNotifications() {
		propertyChangeListener = evt -> {
			if (evt.getPropertyName().equals("_refresh_")) { //$NON-NLS-1$
				updateItemsMonitor();
			}

			if (evt.getPropertyName().equals("remove_children")) { //$NON-NLS-1$
				updateItemsMonitor();
			}
		};

		final IServerManager serverManager = MonitorUIActivator.getDefault().getServerManager();
		serverManager.addPropertyChangeListener(propertyChangeListener);
	}

	private void hookActions() {
		executionListener = new IExecutionListener() {

			@Override
			public void preExecute(final String commandId, final ExecutionEvent event) {
				// TODO Auto-generated method stub
			}

			@Override
			public void postExecuteSuccess(final String commandId, final Object returnValue) {
				if (commandId.equals("br.com.totvs.tds.ui.server.commands.monitorSelectionCommand")) {
					final Boolean selection = (Boolean) returnValue;
					if (selection) {
						hookSelectionService();
					} else {
						unhookSelectionService();
					}
				} else if (commandId.equals("br.com.totvs.tds.ui.monitor.commands.refreshMonitor")) {
					final Integer value = (Integer) returnValue;

					if (value == 0) { // imediato
						final int oldIntervalScheduler = intervalScheduler;
						stopMonitorJob();
						intervalScheduler = 0;
						startMonitorJob();
						intervalScheduler = oldIntervalScheduler;
					} else if (value != intervalScheduler) {
						stopMonitorJob();
						intervalScheduler = value;
						startMonitorJob();
					}
				}
			}

			@Override
			public void postExecuteFailure(final String commandId, final ExecutionException exception) {
				// TODO Auto-generated method stub
			}

			@Override
			public void notHandled(final String commandId, final NotHandledException exception) {
				// TODO Auto-generated method stub

			}
		};

		actionSelectToMonitor.addExecutionListener(executionListener);
		actionRefreshMonitor.addExecutionListener(executionListener);

		final Boolean state = (Boolean) actionSelectToMonitor.getState(RegistryToggleState.STATE_ID).getValue();
		if (state) {
			hookSelectionService();
		}

	}

	protected void unhookSelectionService() {
		if (selectionListener != null) {
			final ISelectionService ss = getSite().getWorkbenchWindow().getSelectionService();
			ss.removeSelectionListener(selectionListener);
		}
	}

	private void hookSelectionService() {
		selectionListener = (sourcepart, selection) -> {
			if (sourcepart instanceof ViewPart) {
				final ViewPart viewPart = (ViewPart) sourcepart;
				if (viewPart.getSite().getId().equals(ServerView.VIEW_ID)) {
					final TreePath[] paths = ((TreeSelection) selection).getPaths();
					updateItemsMonitor(paths);
				}

			}
		};

		final ISelectionService ss = getSite().getWorkbenchWindow().getSelectionService();
		ss.addSelectionListener(selectionListener);

		final ISelection selection = ss.getSelection(ServerView.VIEW_ID);
		if (selection instanceof TreeSelection) {
			final TreePath[] paths = ((TreeSelection) selection).getPaths();
			updateItemsMonitor(paths);
		}
	}

	private void updateItemsMonitor() {
		updateItemsMonitor(TREE_PATH_EMPTY_ARRAY);
	}

	private void updateItemsMonitor(final TreePath[] paths) {
		final Map<String, IServerMonitor> selectedItems = new HashMap<String, IServerMonitor>();
		stopMonitorJob();

		for (final TreePath treePath : paths) {
			final IItemInfo item = (IItemInfo) treePath.getLastSegment();
			updateItemsMonitor(selectedItems, item);
		}

		itemsMonitor.clear();
		selectedItems.forEach((final String name, final IServerMonitor server) -> {
			itemsMonitor.put(name, server);
		});

		pinItemsMonitor.forEach((final String name, IServerMonitor server) -> {
			if (server == null) {
				final IAppServerInfo serverInfo = MonitorUIActivator.getDefault().getServerManager().getServer(name);
				server = new ServerMonitor(serverInfo, itemsMonitor);

			}
			itemsMonitor.put(name, server);
		});

		viewer.setInput(itemsMonitor);

		startMonitorJob();
		firePropertyChange(IWorkbenchPart.PROP_TITLE);
	}

	private void updateItemsMonitor(final Map<String, IServerMonitor> selectedItems, final IItemInfo itemInfo) {
		if (itemInfo instanceof IGroupInfo) {
			final IGroupInfo group = (IGroupInfo) itemInfo;

			for (final IItemInfo item : group.getChildren()) {
				if (item instanceof IGroupInfo) {
					updateItemsMonitor(selectedItems, item);
				} else if (item instanceof IAppServerInfo) {
					if (!isExist(selectedItems, item.getName())) {
						addItem(selectedItems, (IAppServerInfo) item);
					}
				}
			}
		} else if (itemInfo instanceof IAppServerInfo) {
			if (!isExist(selectedItems, itemInfo.getName())) {
				addItem(selectedItems, (IAppServerInfo) itemInfo);
			}
		}
	}

	/**
	 * Cria as colunas da tabela.
	 */
	private void createColumns() {
		final List<MonitorColumn> columns = MonitorColumn.getColumnsToMonitor();

		for (final MonitorColumn monitorColumn : columns) {
			createTreeColumn(monitorColumn);
		}
	}

	/**
	 * Cria uma coluna.
	 *
	 * @param monitorColumn - Dados de monitoramento.
	 * @return Coluna que foi criada.
	 */
	private TreeColumn createTreeColumn(final MonitorColumn monitorColumn) {
		final TreeColumn column = new TreeColumn(treeMonitor, SWT.NONE);
		column.setWidth(monitorColumn.getSize());
		column.setText(monitorColumn.getName());
		column.setResizable(true);
		column.setMoveable(monitorColumn.isFixed());
		column.setAlignment(monitorColumn.getAlignment());

		return column;
	}

	@Override
	public void setFocus() {
		treeMonitor.setFocus();
	}

	@Override
	public void refreshColumns() {
	}

	@Override
	protected Tree getTree() {
		return treeMonitor;
	}

	private void addItem(final Map<String, IServerMonitor> selectedItems, final IAppServerInfo server) {
		if ((treeMonitor == null) || treeMonitor.isDisposed()) {
			return;
		}

		final IServerMonitor serverMonitor = new ServerMonitor(server, itemsMonitor);
		selectedItems.put(serverMonitor.getServerName(), serverMonitor);
	}

	@Override
	public String getPartName() {
		if (itemsMonitor.size() == 1) {
			return String.format("%s (%d servidor)", super.getPartName(), itemsMonitor.size());
		}

		if (itemsMonitor.size() > 1) {
			return String.format("%s (%d servidores)", super.getPartName(), itemsMonitor.size());
		}

		return super.getPartName();
	}

	private boolean isExist(final Map<String, IServerMonitor> selectedItems, final String element) {

		return selectedItems.containsKey(element);
	}

	@Override
	public void dispose() {
		unhookSelectionService();
		unhookActions();
		stopMonitorJob();
		serverMonitorJob.cancel();

		super.dispose();
	}

	private void unhookActions() {
		actionSelectToMonitor.removeExecutionListener(executionListener);
		actionRefreshMonitor.removeExecutionListener(executionListener);

		executionListener = null;
	}

	@Override
	public void aboutToRun(final IJobChangeEvent event) {
		final Job job = event.getJob();

		job.setProperty(ServerMonitorJob.WRITE_LOG, true); // finalizar
		if (intervalScheduler == 0) {
			job.setProperty(ServerMonitorJob.NEXT_RUN, null);
		} else {
			final Calendar now = Calendar.getInstance();
			now.add(Calendar.SECOND, intervalScheduler);
			job.setProperty(ServerMonitorJob.NEXT_RUN, now);
		}
	}

	@Override
	public void awake(final IJobChangeEvent event) {

	}

	@Override
	public void done(final IJobChangeEvent event) {
		final Job job = event.getJob();
		final IStatus result = event.getResult();

		if (result.isOK()) {
			job.schedule(intervalScheduler * 1000);
		}

		refresh();
	}

	@Override
	public void running(final IJobChangeEvent event) {

	}

	@Override
	public void scheduled(final IJobChangeEvent event) {
	}

	@Override
	public void sleeping(final IJobChangeEvent event) {
	}

	private void refresh() {
		Display.getDefault().asyncExec(() -> {
			viewer.refresh();
		});
	}

	public synchronized void startMonitorJob() {
		if (serverMonitorJob == null) {
			serverMonitorJob = new ServerMonitorJob(itemsMonitor);
			serverMonitorJob.addJobChangeListener(this);
		}

		if (!itemsMonitor.isEmpty()) {
			serverMonitorJob.schedule(intervalScheduler);
		}
	}

	public synchronized void stopMonitorJob() {
		if (serverMonitorJob != null) {
			serverMonitorJob.cancel();
		}
	}

	public void addServer(final IAppServerInfo serverInfo) {
		if (!pinItemsMonitor.containsKey(serverInfo.getName())) {
			addItem(pinItemsMonitor, serverInfo);
			serverInfo.setPinnedMonitor(true);

			updateItemsMonitor();
		}
	}

	public void removeServer(final IAppServerInfo serverInfo) {
		if (pinItemsMonitor.containsKey(serverInfo.getName())) {
			pinItemsMonitor.remove(serverInfo.getName());
			serverInfo.setPinnedMonitor(false);

			updateItemsMonitor();
		}
	}

}