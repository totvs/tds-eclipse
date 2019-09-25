package br.com.totvs.tds.ui.server.wizards.patch;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.ibm.icu.text.NumberFormat;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.server.jobs.LoadRpoMapJob;
import br.com.totvs.tds.server.model.RPOTypeElement;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * Geração de pacote a partir do RPO.
 */
public class PatchRPOPage extends WizardPage {

	class ElementFilter extends ViewerFilter {

		private Pattern pattern = null;
		private boolean program = true;
		private boolean resource = true;

		@Override
		public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
			boolean result = true;

			if (element instanceof IRpoElement) {
				IRpoElement node = (IRpoElement) element;
				RPOTypeElement type = node.getType();
				result = ((RPOTypeElement.PROGRAM.equals(type) && program)
						|| (!RPOTypeElement.PROGRAM.equals(type) && resource));

				if (result && (pattern != null)) {
					result = pattern.matcher(node.getName()).matches();
				}
			}

			return result;
		}

		public void setProgram(boolean program) {
			this.program = program;
		}

		public void setResource(boolean resource) {
			this.resource = resource;
		}

		/**
		 * @param target the target to set
		 */
		public void setTarget(final String target) {
			String regExp = ""; //$NON-NLS-1$

			if (target != null) { // $NON-NLS-1$
				regExp = target.trim();
				regExp = regExp.replace(".", "(\\.)"); //$NON-NLS-1$ //$NON-NLS-2$
				regExp = regExp.replace("?", "(.)"); //$NON-NLS-1$ //$NON-NLS-2$
				regExp = regExp.replace("*", "(.*)"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			pattern = regExp.isEmpty() ? null : Pattern.compile(regExp, Pattern.CASE_INSENSITIVE);
		}

	}

	private static final int BTN_WIDTH = 60;

	private BuildPatchAttributes attributes;

	private ElementFilter filterAll;
	private Button add;

	private Button addAll;
	private Button remove;

	private Button removeAll;

	private Table leftTable;
	private Table rightTable;

	private List<IRpoElement> leftElements = Collections.emptyList();
	private List<IRpoElement> rightElements = Collections.emptyList();

	private TableViewer leftTableViewer;
	private TableViewer rightTableViewer;

	private ToolItem tltmProgram;
	private ToolItem tltmResource;
	private ToolItem tltmTRes;
	private LoadRpoMapJob loadMapjob;

	/**
	 * Create the wizard.
	 *
	 * @param attributes
	 */
	public PatchRPOPage(final BuildPatchAttributes attributes) {
		super("patchRPOPage"); //$NON-NLS-1$

		setTitle("Geração a partir do RPO");
		setDescription("Este assistente o auxiliar� na geração do pacote de Atualização a partir do RPO.");
		setImageDescriptor(ServerUIIcons.getBuildPatch());
		this.attributes = attributes;
	}

	private void add() {
		try {
			final IStructuredSelection selection = (IStructuredSelection) leftTableViewer.getSelection();
			getContainer().run(true, true, new IRunnableWithProgress() {
				@Override
				public void run(final IProgressMonitor monitor) {
					Object[] items = selection.toArray();
					monitor.beginTask("Adicionando seleção", items.length);
					for (int i = 0; i < items.length; i++) {
						IRpoElement next = (IRpoElement) items[i];
						leftElements.remove(next);
						rightElements.add(next);
						monitor.worked(1);

					}
					monitor.done();
				}
			});
		} catch (InvocationTargetException | InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void addAll() {
		leftTable.selectAll();
		add();
	}

	private void addColumnSelectionListener(TableColumn column) {
		column.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				tableColumnClicked((TableColumn) e.widget);
			}
		});
	}

	private void addColumnSelectionListeners(TableViewer tableViewer) {
		Table table = tableViewer.getTable();
		TableColumn[] columns = table.getColumns();

		tableViewer.setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				return compareElements((TableViewer) viewer, e1, e2);
			}
		});

		for (int i = 0; i < columns.length; i++) {
			addColumnSelectionListener(columns[i]);
		}

	}

	private void addWizardContainerSelectionPageListener(final IPageChangeProvider wizardContainer) {
		wizardContainer.addPageChangedListener(new IPageChangedListener() {
			@Override
			public void pageChanged(final PageChangedEvent event) {
				if (event.getSelectedPage() instanceof PatchRPOPage) {
					IAppServerInfo server = attributes.getServer();
					String environment = attributes.getEnvironment();

					if (server != null && environment != null) {
						loadRpoElements();
					}
				}
			}
		});
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private int compareElements(TableViewer tableViewer, Object e1, Object e2) {
		Table table = tableViewer.getTable();
		int index = Arrays.asList(table.getColumns()).indexOf(table.getSortColumn());
		int result = 0;

		if (index != -1) {
			if (index == 0) {
				Comparable c1 = ((IRpoElement) e1).getName();
				Comparable c2 = ((IRpoElement) e2).getName();
				result = c1.compareTo(c2);
			} else {
				// comparação de Date, gera o erro
				// "Comparison method violates its general contract!"
				Comparable c1 = IRpoElement.SDF_COMPARE.format(((IRpoElement) e1).getDate());
				Comparable c2 = IRpoElement.SDF_COMPARE.format(((IRpoElement) e1).getDate());
				result = c1.compareTo(c2);
			}
		}

		return table.getSortDirection() == SWT.UP ? result : -result;

	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent
	 */
	@Override
	public void createControl(final Composite parent) {
		addWizardContainerSelectionPageListener((IPageChangeProvider) this.getContainer());

		ISelectionChangedListener addListener = new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				enableAddButtons(selection);
			}
		};

		ISelectionChangedListener removeListener = new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				enableRemoveButtons(selection);
			}
		};

		SelectionListener buttonSelectionListener = new SelectionListener() {
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (e.widget.equals(add)) {
					add();
				} else if (e.widget.equals(addAll)) {
					addAll();
				} else if (e.widget.equals(remove)) {
					remove();
				} else if (e.widget.equals(removeAll)) {
					removeAll();
				}
				listViewerRefresh();
			}
		};

		//
		Composite composite = new Composite(parent, SWT.NULL);
		setControl(composite);
		composite.setLayout(new GridLayout(1, false));

		filterAll = new ElementFilter();

		//
		Composite selectionComposite = new Composite(composite, SWT.NONE);
		selectionComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		GridLayout gl_selectionComposite = new GridLayout(3, false);
		selectionComposite.setLayout(gl_selectionComposite);

		Composite composite_1 = new Composite(selectionComposite, SWT.NONE);
		GridLayout gl_composite_1 = new GridLayout(2, false);
		gl_composite_1.verticalSpacing = 0;
		gl_composite_1.marginWidth = 0;
		gl_composite_1.horizontalSpacing = 0;
		gl_composite_1.marginHeight = 0;
		composite_1.setLayout(gl_composite_1);
		composite_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 3, 1));

		ToolBar toolBar = new ToolBar(composite_1, SWT.FLAT | SWT.RIGHT);
		toolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		tltmProgram = new ToolItem(toolBar, SWT.CHECK);
		tltmProgram.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				filterAll.setProgram(tltmProgram.getSelection());
				listViewerRefresh();
			}
		});
		tltmProgram.setSelection(true);
		tltmProgram.setText("Programa");

		tltmResource = new ToolItem(toolBar, SWT.CHECK);
		tltmResource.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				filterAll.setResource(tltmResource.getSelection());
				listViewerRefresh();
			}
		});
		tltmResource.setSelection(true);
		tltmResource.setText("Recurso");

		tltmTRes = new ToolItem(toolBar, SWT.CHECK);
		tltmTRes.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				loadRpoElements();
			}
		});
		tltmTRes.setSelection(false);
		tltmTRes.setText("Recurso TRes");

		Composite filterComposite = new Composite(composite_1, SWT.NONE);
		filterComposite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		GridLayout gl_filterComposite = new GridLayout(3, false);
		gl_filterComposite.marginHeight = 0;
		filterComposite.setLayout(gl_filterComposite);
		Label labelSearch = new Label(filterComposite, SWT.NONE);
		labelSearch.setText("Filtrar objeto");
		final Text filterText = new Text(filterComposite, SWT.BORDER);
		filterText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		filterText.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				filterAll.setTarget(((Text) e.widget).getText());
				listViewerRefresh();
			}
		});
		Button clearFilter = new Button(filterComposite, SWT.NONE);
		clearFilter.setText("Remover filtro");
		clearFilter.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {

				filterText.setText(""); //$NON-NLS-1$
			}
		});

		Label lblRpo = new Label(selectionComposite, SWT.NONE);
		lblRpo.setText("RPO");
		new Label(selectionComposite, SWT.NONE);

		Label lblPacote = new Label(selectionComposite, SWT.NONE);
		lblPacote.setText("Pacote");

		leftTableViewer = new TableViewer(selectionComposite, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI);
		leftTableViewer.setContentProvider(ArrayContentProvider.getInstance());
		leftTableViewer.setFilters(filterAll);
		leftTableViewer.addSelectionChangedListener(addListener);

		leftTable = leftTableViewer.getTable();
		leftTable.setLinesVisible(true);
		leftTable.setHeaderVisible(true);
		leftTable.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		TableViewerColumn tableViewerColumn = new TableViewerColumn(leftTableViewer, SWT.NONE);
		TableColumn tblclmnNome = tableViewerColumn.getColumn();
		tblclmnNome.setWidth(100);
		tblclmnNome.setText("Nome");
		tableViewerColumn.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				IRpoElement p = (IRpoElement) element;
				return p.getName();
			}
		});

		TableViewerColumn tableViewerColumn_1 = new TableViewerColumn(leftTableViewer, SWT.NONE);
		TableColumn tblclmnData = tableViewerColumn_1.getColumn();
		tblclmnData.setWidth(100);
		tblclmnData.setText("Data");
		tableViewerColumn_1.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				IRpoElement p = (IRpoElement) element;
				if (p.getDate() == null) {
					return "<invalida>";
				}

				return IRpoElement.SDF.format(p.getDate());
			}
		});

		Composite buttonComposite = new Composite(selectionComposite, SWT.NONE);
		buttonComposite.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, false));
		GridLayout bcgl = new GridLayout(1, true);
		bcgl.marginTop = 0;
		buttonComposite.setLayout(bcgl);
		GridData gd = new GridData();
		gd.widthHint = BTN_WIDTH;
		add = new Button(buttonComposite, SWT.NONE);
		add.setToolTipText("Adiciona o(s) item(ns) selecionado(s).");
		add.setText(">");
		add.setLayoutData(gd);
		add.setEnabled(false);
		add.addSelectionListener(buttonSelectionListener);

		GridData gd2 = new GridData();
		gd2.widthHint = BTN_WIDTH;
		addAll = new Button(buttonComposite, SWT.NONE);
		addAll.setToolTipText("Adiciona todos os itens.");
		addAll.setText(">>");
		addAll.setLayoutData(gd2);
		addAll.setEnabled(false);
		addAll.addSelectionListener(buttonSelectionListener);

		GridData gd3 = new GridData();
		gd3.widthHint = BTN_WIDTH;
		remove = new Button(buttonComposite, SWT.NONE);
		remove.setToolTipText("Remove o(s) item(ns) selecionado(s).");
		remove.setText("<");
		remove.setLayoutData(gd3);
		remove.setEnabled(false);
		remove.addSelectionListener(buttonSelectionListener);

		GridData gd4 = new GridData();
		gd4.widthHint = BTN_WIDTH;
		removeAll = new Button(buttonComposite, SWT.NONE);
		removeAll.setToolTipText("Removo todos os itens.");
		removeAll.setText("<<");
		removeAll.setLayoutData(gd4);
		removeAll.setEnabled(false);
		removeAll.addSelectionListener(buttonSelectionListener);

		rightTableViewer = new TableViewer(selectionComposite, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI);
		rightTableViewer.addSelectionChangedListener(removeListener);
		rightTableViewer.setContentProvider(ArrayContentProvider.getInstance());

		rightTable = rightTableViewer.getTable();
		rightTable.setLinesVisible(true);
		rightTable.setHeaderVisible(true);
		rightTable.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		TableViewerColumn tableViewerColumn_2 = new TableViewerColumn(rightTableViewer, SWT.NONE);
		TableColumn tableColumn = tableViewerColumn_2.getColumn();
		tableColumn.setWidth(100);
		tableColumn.setText("Nome");
		tableViewerColumn_2.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				IRpoElement p = (IRpoElement) element;

				return p.getName();
			}
		});

		TableViewerColumn tableViewerColumn_3 = new TableViewerColumn(rightTableViewer, SWT.NONE);
		TableColumn tableColumn_1 = tableViewerColumn_3.getColumn();
		tableColumn_1.setWidth(100);
		tableColumn_1.setText("Data");
		tableViewerColumn_3.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				IRpoElement p = (IRpoElement) element;
				if (p.getDate() == null) {
					return "<invalida>";
				}

				return IRpoElement.SDF.format(p.getDate());
			}
		});

		addColumnSelectionListeners(leftTableViewer);
		addColumnSelectionListeners(rightTableViewer);
	}

	private void dialogChanged() {
		//
		updateErrorMessage(null);
		//
		int size = rightElements.size();
		if (size == 0) {
			setErrorMessage("Selecione pelo menos um recurso para o pacote.");
		}

		Display.getCurrent().asyncExec(new Runnable() {

			@Override
			public void run() {
				List<String> resources = new ArrayList<String>();
				rightElements.stream().forEach(e -> resources.add(e.getName()));
				attributes.setResources(resources);
			}
		});
	}

	private void enableAddButtons(final IStructuredSelection selection) {
		add.setEnabled(selection != null && !selection.isEmpty());
		addAll.setEnabled(!leftElements.isEmpty());
	}

	private void enableButtons() {
		enableAddButtons(null);
		enableRemoveButtons(null);
	}

	private void enableRemoveButtons(final IStructuredSelection selection) {
		remove.setEnabled(selection != null && !selection.isEmpty());
		removeAll.setEnabled(!rightElements.isEmpty());
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	private void listViewerRefresh() {
		if (leftTableViewer != null) {
			rightTableViewer.refresh();
			leftTableViewer.refresh();

			enableButtons();
			dialogChanged();
		}
	}

	/**
	 * Loads everything from the RPO.<br>
	 * If the loadMapjob is already loading when this method is called, it is going
	 * to be canceled and scheduled again.<br>
	 * To use this method, the method "createControl" must have been called first
	 * otherwise the loadMapjob won't start.
	 *
	 * @param text        - The String that will be shown in the text fields.
	 * @param server      - The server
	 * @param environment - The environment
	 */
	private void loadRpoElements() {
		if (loadMapjob != null) {
			loadMapjob.cancel();
			loadMapjob = null;
		}

		loadMapjob = new LoadRpoMapJob("Carregando RPO", attributes.getServer(), attributes.getEnvironment(),
				tltmTRes.getSelection());
		loadMapjob.schedule();

		try {
			getContainer().run(true, true, new IRunnableWithProgress() {
				@Override
				public void run(final IProgressMonitor monitor) {
					try {
						monitor.beginTask("Lendo RPO", IProgressMonitor.UNKNOWN);
						setErrorMessage(null);
						// TODO: Melhorar processo de forma a eliminar o "join".
						while (loadMapjob.getState() == Job.RUNNING) {
							loadMapjob.join(2000, monitor);
							monitor.worked(1);
						}

						if (loadMapjob.getResult().isOK()) {
							leftElements = loadMapjob.getRpoMap();
						} else {
							leftElements = new ArrayList<IRpoElement>();
						}
						rightElements = new ArrayList<IRpoElement>();
					} catch (OperationCanceledException e) {
						setErrorMessage("Carga de RPO cancelada.");
					} catch (InterruptedException e) {
						setErrorMessage("Carga de RPO interronpido.");
					} finally {
						monitor.done();
					}
				}
			});
		} catch (InvocationTargetException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Geração de Pacote", e.getMessage(), e);
		} catch (InterruptedException e) {
			ServerActivator.logStatus(IStatus.CANCEL, "Geração de Pacote", e.getMessage());
		}

		NumberFormat nf = NumberFormat.getIntegerInstance();

		leftTableViewer.setInput(leftElements);
		rightTableViewer.setInput(rightElements);
		listViewerRefresh();

		if (leftElements.isEmpty()) {
			updateErrorMessage("não foi poss�vel ler RPO. Veja log para mais detalhes.");
		} else {
			updateErrorMessage(null);
			setMessage(String.format("Foram obtidos [%s] objetos do RPO.", nf.format(leftElements.size())),
					IMessageProvider.INFORMATION);
		}
	}

	private void remove() {
		try {
			final IStructuredSelection selection = (IStructuredSelection) rightTableViewer.getSelection();
			getContainer().run(true, true, new IRunnableWithProgress() {
				@Override
				public void run(final IProgressMonitor monitor) {
					Object[] items = selection.toArray();
					monitor.beginTask("Removendo seleção", items.length);
					for (int i = 0; i < items.length; i++) {
						IRpoElement next = (IRpoElement) items[i];
						rightElements.remove(next);
						leftElements.add(next);
						monitor.worked(1);
					}
					monitor.done();
				}
			});
		} catch (InvocationTargetException | InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void removeAll() {
		rightTable.selectAll();
		remove();
	}

	private void tableColumnClicked(TableColumn column) {
		Table table = column.getParent();

		if (column.equals(table.getSortColumn())) {
			table.setSortDirection(table.getSortDirection() == SWT.UP ? SWT.DOWN : SWT.UP);
		} else {
			table.setSortColumn(column);
			table.setSortDirection(SWT.UP);
		}

		listViewerRefresh();
	}

	private void updateErrorMessage(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
}
