package br.com.totvs.tds.ui.monitor.views;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.dialogs.SearchPattern;
import org.eclipse.ui.handlers.RegistryToggleState;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.monitor.LogSessionMonitor;
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

	private static final long SCHEDULER = 30000;

	private static final int PROP_CONTENT_DESCRIPTION = 0;

	private boolean recordLog = false;

	public ServerMonitorView() {
	}

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

	private int ORDER_COLUMN = 0;

	private IExecutionListener executionListener;

	private Command actionSelectToMonitor;

	private ISelectionListener selectionListener;

	private ServerMonitorJob serverMonitorJob;

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
		hookActions();

		getSite().setSelectionProvider(viewer);
	}

	private void initializeActions() {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ICommandService commandService = serviceLocator.getService(ICommandService.class);

		actionSelectToMonitor = commandService
				.getCommand("br.com.totvs.tds.ui.server.commands.monitorSelectionCommand"); //$NON-NLS-1$
	}

	private void hookActions() {
		executionListener = new IExecutionListener() {

			@Override
			public void preExecute(final String commandId, final ExecutionEvent event) {
				// TODO Auto-generated method stub
			}

			@Override
			public void postExecuteSuccess(final String commandId, final Object returnValue) {
				final Boolean selection = (Boolean) returnValue;

				if (selection) {
					hookSelectionService();
				} else {
					unhookSelectionService();
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

	private void updateItemsMonitor(final TreePath[] paths) {
		final Map<String, IServerMonitor> selectedItems = new HashMap<String, IServerMonitor>();

		for (final TreePath treePath : paths) {
			final IItemInfo item = (IItemInfo) treePath.getLastSegment();
			updateItemsMonitor(selectedItems, item);
		}

		stopMonitorJob();

		itemsMonitor.clear();
		selectedItems.forEach((final String name, final IServerMonitor server) -> {
			itemsMonitor.put(name, server);
		});

		pinItemsMonitor.forEach((final String name, final IServerMonitor server) -> {
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
			final TreeColumn column = createTreeColumn(monitorColumn);
			column.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					ORDER_COLUMN = monitorColumn.getOrder();
					// orderColumn(ORDER_COLUMN);

					if (itemsMonitor.size() > 0) {
						treeMonitor.setSortColumn(column);
						treeMonitor.setSortDirection(SWT.DOWN);
					} else {
						treeMonitor.setSortColumn(null);
					}
					viewer.refresh();
				}
			});
		}
	}

//	private void orderColumn(final int columnNumber) {
//		ArrayList<IServerMonitor> arrayUsuarios = new ArrayList<IServerMonitor>();
//		final ArrayList<IServerMonitor> arrayServers = new ArrayList<IServerMonitor>();
//		for (final IServerMonitor server : itemsMonitor) {
//			arrayUsuarios = getChildrens(server.getChildren());
//			Collections.sort(arrayUsuarios, Comparator(columnNumber));
//			server.setChildren(arrayUsuarios);
//			arrayServers.add(server);
//		}
//		itemsMonitor = new ArrayList<IServerMonitor>();
//		for (final IServerMonitor server : arrayServers) {
//			itemsMonitor.add(server);
//		}
//		viewer.refresh();
//	}

	private Comparator<? super IServerMonitor> Comparator(final int columnNumber) {
		return (o1, o2) -> {

			switch (columnNumber) {
			case 0:
				return (o1.getServerName().compareTo(o2.getServerName()));
			case 1:
				return (o1.getEnvironment().compareTo(o2.getEnvironment()));
			case 2:
				return (o1.getMachine().compareTo(o2.getMachine()));
			case 3:
				final Long o1ThreadId = Long.valueOf(o1.getThreadId());
				final Long o2ThreadId = Long.valueOf(o2.getThreadId());
				return (o1ThreadId.compareTo(o2ThreadId));
			case 4:
				return (o1.getUserServer().compareTo(o2.getUserServer()));
			case 5:
				return (o1.getProgram().compareTo(o2.getProgram()));
			case 6:
				return (o1.getConection().compareTo(o2.getConection()));
			case 7:
				return (o1.getTimeElapsed().compareTo(o2.getTimeElapsed()));
			case 8:
				final Long o1Instrucoes = new Long(Long.valueOf(o1.getInstructions()));
				final Long o2Instrucoes = Long.valueOf(o2.getInstructions());
				return (o1Instrucoes.compareTo(o2Instrucoes));
			case 9:
				final Long o1InstrucoesXSegundo = Long.valueOf(o1.getInstructionsXSeconds());
				final Long o2InstrucoesXSegundo = Long.valueOf(o2.getInstructionsXSeconds());
				return (o1InstrucoesXSegundo.compareTo(o2InstrucoesXSegundo));
			case 10:
				return (o1.getObservations().compareTo(o2.getObservations()));
			case 11:
				final Long o1Memoria = Long.valueOf(o1.getMemory());
				final Long o2Memoria = Long.valueOf(o2.getMemory());
				return (o1Memoria.compareTo(o2Memoria));
			case 12:
				return (o1.getSID().compareTo(o2.getSID()));
			case 13:
				return (o1.getRPO().compareTo(o2.getRPO()));
			case 14:
				return (o1.getTypeConnection().compareTo(o2.getTypeConnection()));
			case 15:
				return (o1.getTimeInactivity().compareTo(o2.getTimeInactivity()));
			}
			return 0;
		};
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

	private List<IServerMonitor> refreshItems(final List<IServerMonitor> items)
			throws IllegalArgumentException, Exception {
		LogSessionMonitor logMonitor = null;

		if (recordLog) {
			logMonitor = new LogSessionMonitor();
			logMonitor.open();
		}

		for (final IServerMonitor item : items) {
			List<IUserMonitor> users = null;
			final IAppServerInfo serverInfo = item.getServerInfo();

			if (serverInfo.isConnected()) {
				users = item.getChildren();
				if (recordLog) {
					if (users != null) {
						logMonitor.header(serverInfo.getAddress().toString(), users.size());
						// logMonitor.write(users);
						logMonitor.footer();
					} else {
						logMonitor.write("< Informações não disponível. >\n".getBytes());
						logMonitor.write("< Refaça a conexão.           >\n".getBytes());
					}
				}
			}
			// item.removeChildrenAll();

			itemsMonitor.put(item.getServerName(), item);
		}

		if (recordLog) {
			logMonitor.close();

			MonitorUIActivator.logStatus(IStatus.WARNING, "Gerado log de sessões do monitor.\n\tArquivo: %s",
					logMonitor.getFilename());

			logMonitor = null;
		}

		return items;
	}

	@Override
	public void dispose() {
		unhookSelectionService();
		unhookActions();
		serverMonitorJob.cancelJobs();
		serverMonitorJob.cancel();

		super.dispose();
	}

	private void unhookActions() {
		actionSelectToMonitor.removeExecutionListener(executionListener);
		executionListener = null;
	}

	@Override
	public void aboutToRun(final IJobChangeEvent event) {

	}

	@Override
	public void awake(final IJobChangeEvent event) {

	}

	@Override
	public void done(final IJobChangeEvent event) {
		final Job job = event.getJob();
		final IStatus result = event.getResult();

		if (result.isOK()) {
			// update users
			job.schedule(SCHEDULER);
//		} else if (result.getSeverity() == IStatus.CANCEL) {
//			itemsMonitor.remove(serverMonitor.getServerName());
//			job.setProperty(ServerMonitorJob.CANCELED, true);
//			serverMonitor.setStateString("Cancelado");
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

		if (serverMonitorJob.getState() == Job.SLEEPING) {
			serverMonitorJob.wakeUp();
		} else {
			serverMonitorJob.schedule();
		}
	}

	public synchronized void stopMonitorJob() {
		if (serverMonitorJob != null) {
			serverMonitorJob.cancelJobs();
			serverMonitorJob.sleep();
		}
	}

	public void addServer(final IAppServerInfo serverInfo) {
		if (!pinItemsMonitor.containsKey(serverInfo.getName())) {
			addItem(pinItemsMonitor, serverInfo);

			updateItemsMonitor(new TreePath[0]);
		}
	}

	public void removeServer(final IAppServerInfo serverInfo) {
		if (pinItemsMonitor.containsKey(serverInfo.getName())) {
			pinItemsMonitor.remove(serverInfo.getName());

			updateItemsMonitor(new TreePath[0]);
		}
	}

}