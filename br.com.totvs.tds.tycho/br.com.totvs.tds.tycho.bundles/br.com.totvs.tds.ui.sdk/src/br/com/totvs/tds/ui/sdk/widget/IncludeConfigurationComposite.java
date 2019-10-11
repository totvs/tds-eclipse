package br.com.totvs.tds.ui.sdk.widget;

import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.ui.dialogs.PreferencesUtil;

import br.com.totvs.tds.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.SdkUIIcons;
import br.com.totvs.tds.ui.sdk.job.SearchIncludeFoldersJob;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

/**
 * Composite para includes de projeto.
 *
 * @author acandido
 */
public final class IncludeConfigurationComposite extends Composite {

	private TreeViewer treeIncludes;

	private Label lblInlude;
	private Button btnGlobal;
	private Button btnWorkspace;
	private Button btnInc;
	private Button btnDel;
	private Button btnUp;
	private Button btnDown;
	private Button btnSearch;
	private final Link lnkGlobal;

	private final Image imgUp = SdkUIIcons.getUp().createImage();
	private final Image imgDown = SdkUIIcons.getDown().createImage();
	private final Image imgAdd = SdkUIIcons.getAdd().createImage();
	private final Image imgDel = SdkUIIcons.getDel().createImage();
	private final Image imgWorkspace = SdkUIIcons.getWorkspace().createImage();
	private final Image imgGlobal = SdkUIIcons.getGlobal().createImage();
	private final Image imgSearch = SdkUIIcons.getSearch().createImage();

	private final List<IncludeDataModel> includeList = new ArrayList<IncludeDataModel>();
	private final List<IModifyIncludeListener> modifyIncludeListeners = new ArrayList<IModifyIncludeListener>(); // changeproperties

	/**
	 * Create the composite.
	 *
	 * @param parent , composite
	 * @param style  , estilo
	 */
	public IncludeConfigurationComposite(final Composite parent, final int style) {
		super(parent, style);

		final Composite container = this;
		container.setLayout(new GridLayout(2, false));

		createTree(container);
		createButtons(container);
		lnkGlobal = new Link(container, SWT.NONE);
		lnkGlobal.setLayoutData(new GridData(SWT.FILL, SWT.LEFT, false, false, 2, 1));
		lnkGlobal.setText("Acessar <a>Configuração Global</a>");

		actions();
		enabledButtons();
	}

	protected void controlResized(final ControlEvent e) {
	}

	/**
	 * Cria a botoneira.
	 *
	 * @param container
	 */
	private void createButtons(final Composite container) {
		final Composite composite = new Composite(container, SWT.NONE);
		composite.setLayout(new GridLayout());
		composite.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, true, 1, 1));

		btnGlobal = new Button(composite, SWT.PUSH);
		btnGlobal.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnGlobal.setToolTipText("Adiciona configuração global.");
		btnGlobal.setText("Global");
		btnGlobal.setImage(imgGlobal);

		btnWorkspace = new Button(composite, SWT.PUSH);
		btnWorkspace.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnWorkspace.setToolTipText("Adiciona a �rea de trabalho corrente");
		btnWorkspace.setText("�rea de Trabalho");
		btnWorkspace.setImage(imgWorkspace);

		btnInc = new Button(composite, SWT.PUSH);
		btnInc.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnInc.setToolTipText("Adiciona nova pasta.");
		btnInc.setText("Novo");
		btnInc.setImage(imgAdd);

		btnDel = new Button(composite, SWT.PUSH);
		btnDel.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnDel.setToolTipText("Remove itens selecionados.");
		btnDel.setText("Remover");
		btnDel.setImage(imgDel);

		btnUp = new Button(composite, SWT.PUSH);
		btnUp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnUp.setToolTipText("Modifica a ordem de busca.");
		btnUp.setText("Sobe");
		btnUp.setImage(imgUp);

		btnDown = new Button(composite, SWT.PUSH);
		btnDown.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnDown.setToolTipText("Modifica a ordem de busca.");
		btnDown.setText("Desce");
		btnDown.setImage(imgDown);

		btnSearch = new Button(composite, SWT.PUSH);
		btnSearch.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		btnSearch.setToolTipText("Pesquisar pastas com arquivos de definições.");
		btnSearch.setText("Pesquisar");
		btnSearch.setImage(imgSearch); // $NON-NLS-1$
	}

	/**
	 * Cria a �rvore para as definições.
	 *
	 * @param container
	 */
	private void createTree(final Composite container) {
		lblInlude = new Label(container, SWT.NULL);
		lblInlude.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, false, 2, 1));
		lblInlude.setText("Pastas para busca");

		treeIncludes = new TreeViewer(container,
				SWT.BORDER | SWT.SCROLL_PAGE | SWT.MULTI | SWT.FULL_SELECTION | SWT.FILL);
		final Tree tree = this.treeIncludes.getTree();
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 7));

		treeIncludes.setContentProvider(new IncludeListContentProvider());
		treeIncludes.setLabelProvider(new IncludeListLabelProvider());
		treeIncludes.setInput(includeList);
		treeIncludes.addSelectionChangedListener(event -> enabledButtons());
	}

	/**
	 * Definine as ações dos bot�es.
	 */
	private void actions() {
		btnWorkspace.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				workspaceInclude();
			}
		});

		btnGlobal.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				globalInclude();
			}
		});

		btnInc.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final String result = selectDirectory("Selecione uma pasta para busca de arquivos de definição.");

				if (!result.isEmpty()) {
					IncludeConfigurationComposite.this.addSelection(result);
				}

			}
		});

		btnDel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				deleteIncludeItem();
			}
		});

		btnUp.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				moveSelection(-1);
			}
		});

		btnDown.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				moveSelection(1);
			}
		});

		btnSearch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				search();
			}
		});

		lnkGlobal.addListener(SWT.Selection, event -> {
			final PreferenceDialog pref = PreferencesUtil.createPreferenceDialogOn(getShell(),
					ISDKPreferenceKeys.ID_INCLUDE_PAGE, new String[0], null);
			if (pref != null) {
				pref.open();
				treeIncludes.refresh();
			}
		});

		addDisposeListener(e -> IncludeConfigurationComposite.this.widgetDispose(e));

		addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent e) {
				IncludeConfigurationComposite.this.controlResized(e);
			}
		});
	}

	/**
	 * Movimenta o item selecionado para cima ou para baixo.
	 *
	 * @param direction Direção da movimentação (negativo para cima, positivo para
	 *                  baixo).
	 */
	protected void moveSelection(final int direction) {
		final TreeSelection selection = (TreeSelection) treeIncludes.getSelection();
		final IncludeDataModel target = (IncludeDataModel) selection.getFirstElement();
		int position = includeList.indexOf(target);

		position += direction;

		if ((position < 0) || (position >= includeList.size())) {
			return;
		}

		includeList.remove(target);
		includeList.add(position, target);

		notifyModifyIncludeListeners();
		treeIncludes.refresh();
	}

	/**
	 * Sseleção de diret�rios local.
	 */
	private String selectDirectory(final String message) {
		final String result = TDSUtil.directoryDialog(getParent().getShell(), message);

		return (result == null) ? "" : result.trim();
	}

	/**
	 * Adiciona um item na tree.
	 *
	 * @param pathselection
	 * @param selection
	 */
	private void addSelection(final String selection) {
		if (listContains(includeList, selection)) {
			return;
		}
		if (!selection.isEmpty()) {
			final IncludeDataModel id = new IncludeDataModel(selection);
			includeList.add(id);
			notifyModifyIncludeListeners();
			treeIncludes.refresh();
		}
	}

	private boolean listContains(final List<IncludeDataModel> includeList, final String selection) {
		boolean contains = false;
		for (final IncludeDataModel includeDataModel : includeList) {
			if (includeDataModel.getFolder().equals(selection)) {
				contains = true;
				break;
			}
		}
		return contains;
	}

	/**
	 * Adiciona uma pasta selecionada a partir da �rea de trabalho.
	 */
	private void workspaceInclude() {
		final ContainerSelectionDialog selectDialog = new ContainerSelectionDialog(getShell(), null, true,
				"Pasta com arquivos de definição");

		selectDialog.setTitle("Selecione a pasta com arquivos de definição");
		selectDialog.open();

		final Object[] result = selectDialog.getResult();
		if (result != null) {
			this.addSelection(IncludeDataModel.WORKSPACE + result[0].toString());
		}

	}

	/**
	 * Adiciona configuração global.
	 */
	private void globalInclude() {
		addSelection(IncludeDataModel.GLOBAL);
	}

	/*
	 * Remove o item selecionado.
	 */
	private void deleteIncludeItem() {
		final TreeSelection selection = (TreeSelection) treeIncludes.getSelection();

		for (final Object element : selection.toArray()) {
			if (element instanceof IIncludeDataModel) {
				final IIncludeDataModel key = (IIncludeDataModel) element;
				includeList.remove(key);
			}
		}

		notifyModifyIncludeListeners();
		treeIncludes.refresh();
		enabledButtons();
	}

	/**
	 * Libera o componente.
	 *
	 * @param e
	 */
	protected void widgetDispose(final DisposeEvent e) {
		lblInlude.dispose();
		btnWorkspace.dispose();
		btnInc.dispose();
		btnDel.dispose();
		btnUp.dispose();
		btnDown.dispose();
		btnSearch.dispose();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.swt.widgets.Composite#checkSubclass()
	 */
	@Override
	protected void checkSubclass() {
		// Disable the check that prevents subclassing of SWT components
	}

	/**
	 * Controla a disponibilidade dos bot�es.
	 */
	private void enabledButtons() {
		final boolean enabled = true;
		boolean lSelection = false;
		boolean lHaveParent = false;

		final ISelection selection = treeIncludes.getSelection();

		if (!selection.isEmpty()) {
			lSelection = true;
			final TreeSelection st = (TreeSelection) selection;
			for (final TreePath element : st.getPaths()) {
				lHaveParent = element.getSegmentCount() > 1;
				if (lHaveParent) {
					break;
				}
			}
		}

		btnInc.setEnabled(enabled);
		btnDel.setEnabled(enabled && lSelection && !lHaveParent);
		btnDown.setEnabled(enabled && lSelection && !lHaveParent);
		btnUp.setEnabled(enabled && lSelection && !lHaveParent);
		btnGlobal.setEnabled(!listContains(includeList, IncludeDataModel.GLOBAL));
	}

	/**
	 * @return pastas selecionadas
	 */
	public String[] getIncludeSelection() {
		final String[] list = new String[includeList.size()];
		int i = 0;
		for (final IncludeDataModel element : includeList) {
			list[i] = element.getFolder();
			i++;
		}
		return list;
	}

	/**
	 * Seta as pastas selecionadas.
	 */
	public void setIncludeSelection(final String[] includeSelection) {
		includeList.clear();
		treeIncludes.getTree().setRedraw(false);
		for (final String element : includeSelection) {
			addSelection(element);
		}
		treeIncludes.getTree().setRedraw(true);
		notifyModifyIncludeListeners();
	}

	public void setIncludeSelection(final String includes) {
		setIncludeSelection(includes.split(IWrapperManager.INCLUDES_SEPARATOR)); // $NON-NLS-1$
	}

	/**
	 * Controla a visibilidade de uso de configuração global.
	 *
	 * @param visible Visibilidade
	 */
	public void setGlobalVisible(final boolean visible) {
		btnGlobal.setVisible(visible);
		lnkGlobal.setVisible(visible);
	}

	public String getIncludeSelectionAsString() {
		final StringJoiner result = new StringJoiner(IWrapperManager.INCLUDES_SEPARATOR);

		for (final IncludeDataModel element : includeList) {
			result.add(element.getFolder());
		}

		return result.toString();
	}

	public void addModifyIncludeListener(final IModifyIncludeListener modifyIncludeListener) {
		modifyIncludeListeners.add(modifyIncludeListener);
	}

	private void notifyModifyIncludeListeners() {
		for (final IModifyIncludeListener modifyIncludeListener : modifyIncludeListeners) {
			modifyIncludeListener.modifyIncludes();
		}
	}

	private void search() {
		final String result = selectDirectory("Selecione a pasta inicial para a busca.");

		if (!result.isEmpty()) {
			SdkUIActivator.logStatus(IStatus.WARNING, "Procura",
					"Iniciando processo de busca de arquivos de definição. Ao final, voc� ser� notificado.\n\tSe desejar pode iniciar nova busca em paralelo.");
			final SearchIncludeFoldersJob job = new SearchIncludeFoldersJob(result);
			job.schedule();
		}
	}

}
