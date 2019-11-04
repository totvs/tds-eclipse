package br.com.totvs.tds.ui.server.dialog;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;

import com.ibm.icu.util.Calendar;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IRpoFunction;
import br.com.totvs.tds.server.interfaces.IRpoSource;
import br.com.totvs.tds.server.jobs.LoadRpoMapJob;
import br.com.totvs.tds.server.model.RpoTypeElement;
import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.providers.RpoObjectDetailsLabelProvider;
import br.com.totvs.tds.ui.server.providers.RpoObjectLabelProvider;

/**
 * Shows a list of items to the user with a text entry field for a string
 * pattern used to filter the list of items.
 */
public class RpoInspectorDialog extends FilteredItemsSelectionDialog implements IJobChangeListener {

	/**
	 * Filters resources using pattern and showDerived flag. It overrides
	 * ItemsFilter.
	 */
	protected class ElementFilter extends ItemsFilter {

		final private Set<RpoTypeElement> objectTypeSet;

		/**
		 * Creates new instance
		 *
		 * @param container
		 * @param showDerived flag which determine showing derived elements
		 * @param typeMask
		 */
		public ElementFilter(Set<RpoTypeElement> objectTypeSet) {
			super();

			this.objectTypeSet = objectTypeSet;
		}

		/**
		 * Checks whether the provided filter is equal to the current filter. The
		 * default implementation checks if <code>SearchPattern</code> from current
		 * filter is equal to the one from provided filter.
		 *
		 * @param filter filter to be checked, or <code>null</code>
		 * @return <code>true</code> if the given filter is equal to current filter,
		 *         <code>false</code> if given filter isn't equal to current one or if
		 *         it is <code>null</code>
		 *
		 * @see org.eclipse.ui.dialogs.SearchPattern#equalsPattern(org.eclipse.ui.dialogs.SearchPattern)
		 */
		@Override
		public boolean equalsFilter(ItemsFilter filter) {
			boolean result = super.equalsFilter(filter);

			if (result) {
				result = objectTypeSet.equals(((ElementFilter) filter).objectTypeSet);
			}

			return result;
		}

		@Override
		public boolean isConsistentItem(Object item) {

			return (item instanceof IRpoElement);
		}

		/**
		 * @param item Must be instance of IResource, otherwise <code>false</code> will
		 *             be returned.
		 * @see org.eclipse.ui.dialogs.FilteredItemsSelectionDialog.ItemsFilter#matchItem(java.lang.Object)
		 */
		@Override
		public boolean matchItem(Object item) {
			boolean result = false;

			if (isConsistentItem(item)) {
				IRpoElement element = (IRpoElement) item;
				RpoTypeElement objectType = element.getType();

				if (objectTypeSet.contains(RpoTypeElement.OBJECT) || objectTypeSet.contains(objectType)) {
					result = matchName(element);
				}
				element.setVisible(result);
			}

			return result;
		}

		private boolean matchName(IRpoElement rpoElement) {
			String name = rpoElement.getName();

			return matches(name);
		}

	}

	/**
	 * @author acandido
	 *
	 */
	public class FunctionFilter extends ViewerFilter {

		/*
		 * (non-Javadoc)
		 *
		 * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.
		 * Viewer, java.lang.Object, java.lang.Object)
		 */
		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {

			return (element instanceof IRpoFunction);
		}

	}

	/**
	 * Ações associadas ao diálogo de acordo com o tipo de objeto sendo filtrado.
	 *
	 * @author acandido
	 *
	 */
	private class ShowFunctionAction extends Action {

		/**
		 * Creates a new instance of the class.
		 */
		public ShowFunctionAction() {
			super("Apresenta funções", IAction.AS_CHECK_BOX);

			setImage(FUNCTION_ICON);
		}

		@Override
		public void run() {
			extendedArea.setVisible(isChecked());
			GridData gd = (GridData) extendedArea.getLayoutData();
			Rectangle ca = extendedArea.getParent().getClientArea();
			gd.heightHint = (int) (ca.width * 0.4);

			gd.exclude = !isChecked();
			extendedArea.getParent().layout();
		}
	}

	private class ShowResourceAction extends Action {

		/**
		 * Creates a new instance of the class.
		 */
		public ShowResourceAction() {
			super("Apresenta recursos", IAction.AS_CHECK_BOX);

			setImage(RESOURCE_ICON);
		}

		@Override
		public void run() {
			applyFilter();
		}
	}

	private class ShowSourceAction extends Action {
		/**
		 * Creates a new instance of the class.
		 */
		public ShowSourceAction() {
			super("Apresenta fontes", IAction.AS_CHECK_BOX);

			setImage(SOURCE_ICON);
		}

		@Override
		public void run() {
			applyFilter();
		}
	}

	private static final Image SOURCE_ICON = ServerUIIcons.getSource().createImage(true);
	private static final Image RESOURCE_ICON = ServerUIIcons.getResource().createImage(true);
	private static final Image FUNCTION_ICON = ServerUIIcons.getFunction().createImage(true);
	private static final String NEW_LINE = System.getProperty("line.separator"); //$NON-NLS-1$
	// leiaute do arquivo de exporta��o
	private static final String HEADER_01 = "---< Informações Geraris >-----------------------------" + NEW_LINE;
	private static final String HEADER_02 = "Servidor .: %s" + NEW_LINE;
	private static final String HEADER_04 = "Endereço .: %s" + NEW_LINE;
	private static final String HEADER_05 = "Ambiente .: %s" + NEW_LINE;

	private static final String HEADER_06 = "Build ....: %s" + NEW_LINE;
	private static final String HEADER_07 = "OS Type ..: %s" + NEW_LINE;

	private static final String HEADER_10 = "---< Lista de Objetos >--------------------------------" + NEW_LINE;
	private static final String HEADER_11 = "TIPO    DATA                NOME" + NEW_LINE;
	private static final String DETAIL_01 = "%-6.6s  %-18.18s  %s" + NEW_LINE;
	private static final String DETAIL_02 = "Elemento não é um objeto de RPO." + NEW_LINE + "  %s" + NEW_LINE;
	private static final String FOOTER_01 = "------------------------------------------------------" + NEW_LINE;
	private static final String FOOTER_02 = "Recursos .: %,6d (exportados) %s" + NEW_LINE;
	private static final String FOOTER_03 = "            %,6d (total)" + NEW_LINE;

	private static final String FOOTER_04 = "------------------------------------------------------" + NEW_LINE;

	private static final String FOOTER_05 = "Gerado pelo TDS %s" + NEW_LINE;

	private static final String FOOTER_06 = "em %1$td/%1$tm/%1$tY as %1$tT" + NEW_LINE;

	private static final String FOOTER_07 = "--< FIM >---------------------------------------------" + NEW_LINE;
	private static final String DIALOG_SETTINGS = "br.com.totvs.tds.server.ui.editor.pages.dialogs.RpoInspectorDialog";
	private static final String SHOW_FUNCTIONS = "showFunctions";

	private static final String EXTEND_AREA_HEIGHT = "extendAreaHeight";

	private RpoTypeElement objectType = RpoTypeElement.FUNCTION;

	private Composite extendedArea;
	private ShowFunctionAction showFunctions;
	private ShowSourceAction showSource;

	private ShowResourceAction showResource;
	private boolean enableGenerateTextButton;
	private IAppServerInfo server;
	private String environment;
	private Button btnSaveAll;

	private TableViewer lvFunctions;
	private LoadRpoMapJob loadFunctionInspectorJob;
	private List<IRpoElement> elements = Collections.emptyList();

	/**
	 * Creates a new instance of the class.
	 *
	 * @param shell       shell to parent the dialog on
	 * @param server
	 * @param environment
	 * @param multi       indicates whether dialog allows to select more than one
	 *                    position in its list of items
	 * @wbp.parser.constructor
	 */
	public RpoInspectorDialog(Shell shell, IAppServerInfo server, String environment) {
		super(shell, false);

		setListLabelProvider(new RpoObjectLabelProvider());
		setDetailsLabelProvider(new RpoObjectDetailsLabelProvider());
		setInitialPattern("?");
		this.server = server;
		this.environment = environment;

	}

	@Override
	public void aboutToRun(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	public void awake(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.CLIENT_ID + 1) {
			BusyIndicator.showWhile(getShell().getDisplay(), new Runnable() {

				@Override
				public void run() {
					geraTXT(elements);
				}
			});
		} else {
			super.buttonPressed(buttonId);
		}
	}

	@Override
	public boolean close() {
		this.loadFunctionInspectorJob.cancel();

		return super.close();
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		Button button = createButton(parent, IDialogConstants.CLIENT_ID + 1, "Gerar Texto", true);
		button.setEnabled(enableGenerateTextButton);

		super.createButtonsForButtonBar(parent);
	}

	@Override
	protected Control createExtendedContentArea(Composite parent) {

		extendedArea = new Composite(parent, SWT.FLAT);
		GridData gd_extendedArea = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd_extendedArea.exclude = true;
		extendedArea.setLayoutData(gd_extendedArea);
		GridLayout gl_extendedArea = new GridLayout(1, false);
		gl_extendedArea.marginHeight = 0;
		gl_extendedArea.marginWidth = 0;
		extendedArea.setLayout(gl_extendedArea);

		createFunctionList(extendedArea);

		btnSaveAll = new Button(parent, SWT.CHECK);
		btnSaveAll.setText("Exportar com filtro");
		btnSaveAll.setSelection(false);
		btnSaveAll.setToolTipText("Gera arquivo texto somente com os arquivos exibidos");
		btnSaveAll.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));

		return extendedArea;
	}

	@Override
	protected ItemsFilter createFilter() {
		Set<RpoTypeElement> objectTypeSet = Collections.synchronizedSet(EnumSet.noneOf(RpoTypeElement.class));

		if (getObjectType().equals(RpoTypeElement.OBJECT)) {
			if (showSource.isChecked()) {
				objectTypeSet.add(RpoTypeElement.PROGRAM);
			}
			if (showResource.isChecked()) {
				objectTypeSet.add(RpoTypeElement.RESOURCE);
			}
		} else {
			objectTypeSet.add(getObjectType());
		}

		return new ElementFilter(objectTypeSet);
	}

	/**
	 * Cria a lista de funções associadas ao programa, para visualização, se
	 * solicitado.
	 *
	 * @param parent
	 * @return
	 */
	private Composite createFunctionList(Composite parent) {
		lvFunctions = new TableViewer(parent, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.VIRTUAL);
		Table list = lvFunctions.getTable();
		list.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		lvFunctions.setLabelProvider(new RpoObjectLabelProvider());
		lvFunctions.setContentProvider(ArrayContentProvider.getInstance());

		return parent;
	}

	@Override
	public void done(IJobChangeEvent event) {
		if (event.getResult().isOK()) {
			this.elements = loadFunctionInspectorJob.getList();
			// forçar a atualização da lista de elementos
		}
	}

	@Override
	protected void fillContentProvider(final AbstractContentProvider contentProvider, ItemsFilter itemsFilter,
			IProgressMonitor progressMonitor) throws CoreException {
		progressMonitor.beginTask("Localizando", elements.size()); //$NON-NLS-1$

		for (IRpoElement rpoElement : elements) {
			contentProvider.add(rpoElement, itemsFilter);
			progressMonitor.worked(1);
		}

		progressMonitor.done();
	}

	@Override
	protected void fillViewMenu(IMenuManager menuManager) {
		if (getObjectType().equals(RpoTypeElement.OBJECT)) {
			showSource = new ShowSourceAction();
			showSource.setChecked(true);
			menuManager.add(showSource);

			showResource = new ShowResourceAction();
			showResource.setChecked(true);
			menuManager.add(showResource);

			menuManager.add(new Separator());

			showFunctions = new ShowFunctionAction();
			menuManager.add(showFunctions);
		}

		super.fillViewMenu(menuManager);
	}

	/**
	 * Gera arquivo texto com as informações dos objetos do RPO.
	 *
	 * @param elements
	 */

	// Exemplo do arquivo gerado:
	// ---< Informações Gerais >-----------------------------
	// Servidor .: Novo
	// Porta ....: 1234
	// Endere�o .: localhost
	// Ambiente .: P11
	// Build ....: ??????????????????????????
	// OS Type ..: Windows 8
	// Recursos .: 999.999 (exportados)
	// ____________999.999 (total)
	// ---< Lista de Recursos >-------------------------------
	// TIPO__- DATA ____________ - NOME
	// Fonte - 05/02/13 10:47:03 - 0000_T2KC_PACK.PRW
	// Fonte - 30/05/17 15:54:10 - A.PRW
	// Fonte - 29/07/16 14:57:47 - A370JCTB.PRW
	// Fonte - 17/04/07 17:46:21 - ABREEXCL.PRG
	// Fonte - 23/08/16 17:11:44 - ABSENT.PRX
	// Fonte - 23/06/05 14:06:27 - AC060DLB.PRW
	// Fonte - 27/08/04 10:21:30 - AC540MSG.PRW
	// ....
	// Recurso - 13/08/09 10:41:13 - AC680MSG.PRW
	// Recurso - 27/04/04 14:18:57 - AC680PRF.PRW
	// ------------------------------------------------------
	//

	private void geraTXT(final List<IRpoElement> elements) {
		DateFormat formatter = new SimpleDateFormat("yyyyMMddhhmmss"); //$NON-NLS-1$
		String fileName = "TDS_InspObj_" + formatter.format(Calendar.getInstance().getTime()) + ".txt"; //$NON-NLS-1$ //$NON-NLS-2$

		String pathFile = TDSUtil.fileSaveDialog(getShell(), new String[] { "*.txt" },
				new String[] { "Rela\u00E7\u00E3o de Objetos (*.txt)" }, fileName);

		if (pathFile == null) {
			ServerUIActivator.logStatus(IStatus.CANCEL, "Geração do arquivo com lista de objetos cancelado.");
			return;
		}

		File file = new File(pathFile);
		if (file.exists()) {
			boolean response = MessageDialog.openConfirm(getShell(), "Exportação RPO",
					"O arquivo indicado já existe.\nQuer sobrescrever?");

			if (!response) {
				ServerUIActivator.logStatus(IStatus.CANCEL,
						"Geração do arquivo com lista de objetos cancelado, pois já existe o arquivo\n\tArquivo: %s.",
						pathFile);
				return;
			}
		}

		//
		// inicia a geração
		//
		File logFile = new File(pathFile);
		try {
			logFile.createNewFile();
			FileWriter fileWriter = new FileWriter(logFile);

			// cabeçalho do arquivo
			fileWriter.write(HEADER_01);
			fileWriter.write(String.format(HEADER_02, server.getName()));
			fileWriter.write(String.format(HEADER_04, server.getAddress().toString()));
			fileWriter.write(String.format(HEADER_05, environment));

			fileWriter.write(String.format(HEADER_06, server.getVersion()));
			fileWriter.write(String.format(HEADER_07, server.getServerOsType().getOsName()));
			fileWriter.write(HEADER_10);
			fileWriter.write(HEADER_11);

			int proc = 0;
			for (Object object : elements) {
				if (object instanceof IRpoElement) {
					IRpoElement rpoObject = (IRpoElement) object;
					if (!btnSaveAll.getSelection() || rpoObject.isVisible()) {
						proc++;
						String nome = rpoObject.getName();
						String date = rpoObject.getDate().toString();
						String tipo = rpoObject.getType().getTitle();

						fileWriter.write(String.format(DETAIL_01, tipo, date, nome));
					}
				} else {
					fileWriter.write(String.format(DETAIL_02, object.toString()));
				}
			}

			fileWriter.write(FOOTER_01);
			fileWriter.write(String.format(FOOTER_02, proc,
					btnSaveAll.getSelection() ? "Filtro: " + ((Text) getPatternControl()).getText() : ""));
			fileWriter.write(String.format(FOOTER_03, elements.size()));
			fileWriter.write(FOOTER_04);
			fileWriter.write(
					String.format(FOOTER_05, ServerUIActivator.getDefault().getBundle().getVersion().toString()));
			fileWriter.write(String.format(FOOTER_06, Calendar.getInstance().getTime()));
			fileWriter.write(FOOTER_07);

			fileWriter.close();
		} catch (IOException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		ServerUIActivator.logStatus(IStatus.OK, "Arquivo gerado com sucesso.\n\tArquivo: %s", pathFile);

	}

	@Override
	protected IDialogSettings getDialogSettings() {
		IDialogSettings settings = ServerUIActivator.getDefault().getDialogSettings().getSection(DIALOG_SETTINGS);

		if (settings == null) {
			ServerUIActivator.getDefault().getDialogSettings().addNewSection(DIALOG_SETTINGS);
		}

		return settings;
	}

	@Override
	public String getElementName(Object item) {

		return ((IRpoElement) item).getName();
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Comparator getItemsComparator() {
		return new Comparator<IRpoElement>() {

			@Override
			public int compare(IRpoElement arg0, IRpoElement arg1) {

				return arg0.getName().compareToIgnoreCase(arg1.getName());
			}
		};
	}

	/**
	 * @return the objectType
	 */
	public RpoTypeElement getObjectType() {
		return objectType;
	}

	/**
	 * Trata item selecionado.
	 *
	 * @param selection
	 */
	@Override
	protected void handleSelected(StructuredSelection selection) {
		ArrayList<IRpoFunction> functions = new ArrayList<IRpoFunction>();

		for (Object element : selection.toList()) {
			if (element instanceof IRpoSource) {
				functions.addAll(((IRpoSource) element).getFunctionList());
			}
		}

		lvFunctions.setInput(functions);

		super.handleSelected(selection);
	}

	@Override
	protected void restoreDialog(IDialogSettings settings) {
		if (settings != null) {
			if (showFunctions != null) {
				showFunctions.setChecked(false);
				// showFunctions.setChecked(settings.getBoolean(SHOW_FUNCTIONS));

				if (settings.get(EXTEND_AREA_HEIGHT) != null) {
					GridData gd = (GridData) extendedArea.getLayoutData();
					gd.heightHint = settings.getInt(EXTEND_AREA_HEIGHT);
					gd.exclude = !showFunctions.isChecked();
				}
			}

			super.restoreDialog(settings);
		}

	}

	@Override
	public void running(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	public void scheduled(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	public void setEnableGenerateTextButton(boolean enableGenerateTextButton) {
		this.enableGenerateTextButton = enableGenerateTextButton;
	}

	/**
	 * @param objectType the objectType to set
	 */
	public void setObjectType(RpoTypeElement objectType) {
		this.objectType = objectType;
		setTitle(String.format("Inspeção de RPO: %s", this.objectType.getTitle()));

		loadFunctionInspectorJob = new LoadRpoMapJob(server, environment, true, objectType);
		loadFunctionInspectorJob.addJobChangeListener(this);
		loadFunctionInspectorJob.schedule();

		try { // TODO: melhorar processo, (ver done) colocando após o aparecimento do diálogo
			loadFunctionInspectorJob.join();
			this.elements = loadFunctionInspectorJob.getList();

		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	@Override
	public void sleeping(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void storeDialog(IDialogSettings settings) {

		if (showFunctions != null) {
			settings.put(SHOW_FUNCTIONS, showFunctions.isChecked());
			GridData gd = (GridData) extendedArea.getLayoutData();
			settings.put(EXTEND_AREA_HEIGHT, gd.heightHint);
		}

		super.storeDialog(settings);
	}

	@Override
	protected IStatus validateItem(Object item) {

		return Status.OK_STATUS;
	}

}
