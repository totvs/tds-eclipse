package br.com.totvs.tds.ui.server.wizards.patch;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import br.com.totvs.tds.server.model.SourceInformation;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.internal.filter.PatchTableFilter;

/**
 * Dialog para exibir os detalhes do log de patchs.
 */
public class PatchLogDetailsDialog extends Dialog {

	/**
	 * Content Provider para os detalhes de patch.
	 */
	private final class PatchContentProvider implements IStructuredContentProvider {

		@Override
		public void dispose() {
		}

		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(final Object inputElement) {
			List<SourceInformation> sourceInformations = (List<SourceInformation>) inputElement;
			return sourceInformations.toArray();
		}

		@Override
		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		}
	}

	/**
	 * Label Provider para os detalhes de patch.
	 */
	private final class PatchLabelProvider implements ITableLabelProvider {

		private final Image imgPgmAdvpl = ServerUIIcons.getPgmAdvpl().createImage(true);

		@Override
		public void addListener(final ILabelProviderListener listener) {
		}

		@Override
		public void dispose() {
		}

		@Override
		public Image getColumnImage(final Object element, final int columnIndex) {
			switch (columnIndex) {
			case 0:
				return imgPgmAdvpl;
			case 1:
				return null;
			default:
				break;
			}
			return null;
		}

		@Override
		public String getColumnText(final Object element, final int columnIndex) {
			SourceInformation itemInfo = (SourceInformation) element;
			switch (columnIndex) {
			case 0:
				return itemInfo.getName();
			case 1:
				return itemInfo.getDate();
			default:
				break;
			}
			return null;
		}

		@Override
		public boolean isLabelProperty(final Object element, final String property) {
			return false;
		}

		@Override
		public void removeListener(final ILabelProviderListener listener) {
		}
	}

	private Table programsTable;

	private TableViewer tableViewer;

	private final List<SourceInformation> detalhesDoPatch;

	/**
	 * Cria o dialog para exibir os detalhes do log de aplica��o de patch.
	 *
	 * @param parentShell     shell pai
	 * @param detalhesDoPatch lista de informa��es sobre os fontes do patch
	 */
	public PatchLogDetailsDialog(final Shell parentShell, final List<SourceInformation> detalhesDoPatch) {
		super(parentShell);
		this.detalhesDoPatch = detalhesDoPatch;
	}

	@Override
	protected void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		if (detalhesDoPatch != null) {
			newShell.setText("Detalhes do Pacote do Atualização");
		} else {
			newShell.setText("Versão não suportada");
		}
	}

	@Override
	protected void createButtonsForButtonBar(final Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, "OK", true);
	}

	@Override
	protected Control createDialogArea(final Composite parent) {

		Composite container = (Composite) super.createDialogArea(parent);

		// Layout do Composite principal
		FormLayout formlayout = new FormLayout();
		container.setLayout(formlayout);

		if (detalhesDoPatch != null) {
			createPathInfoWindow(container);
		} else {
			createNotSupportedWindow(container);
		}

		return container;
	}

	private void createNotSupportedWindow(final Composite container) {

		FormData formdataSelConf = new FormData();
		formdataSelConf.top = new FormAttachment(0, 0);
		formdataSelConf.left = new FormAttachment(0, 5);
		formdataSelConf.bottom = new FormAttachment(100, -5);
		formdataSelConf.right = new FormAttachment(100, -5);

		Composite compInformation = new Composite(container, SWT.NONE);
		compInformation.setLayoutData(formdataSelConf);

		// Layout q ajusta os Groups no composite
		GridLayout layoutItens = new GridLayout(1, false);
		compInformation.setLayout(layoutItens);

		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessVerticalSpace = false;

		Label label = new Label(compInformation, SWT.NONE);
		// label.setBackground(new Color(null, 255, 0, 0));
		label.setText("Funcionalidade suportada apenas em servidores 7.00.131227A ou maior.");
		label.setLayoutData(gridData);
	}

	private void createPathInfoWindow(final Composite container) {
		FormData formdataSelConf = new FormData();
		formdataSelConf.top = new FormAttachment(0, 0);
		formdataSelConf.left = new FormAttachment(0, 5);
		formdataSelConf.bottom = new FormAttachment(100, -5);
		formdataSelConf.right = new FormAttachment(100, -5);

		Composite compPathInfo = new Composite(container, SWT.NONE);
		compPathInfo.setLayoutData(formdataSelConf);

		// Layout q ajusta os Groups no composite
		GridLayout layoutItens = new GridLayout(1, false);
		compPathInfo.setLayout(layoutItens);

		// GridData da tabela esticando na vertical
		GridData grdTablePrograms = new GridData(GridData.FILL_BOTH);
		grdTablePrograms.grabExcessVerticalSpace = true;
		grdTablePrograms.verticalSpan = 3;

		final String[] colNames = new String[] { "Programa", "Data" };

		programsTable = new Table(compPathInfo, SWT.BORDER | SWT.FULL_SELECTION);

		TableColumn tcProgram = new TableColumn(programsTable, SWT.LEFT);
		tcProgram.setText(colNames[0]);

		TableColumn tcDate = new TableColumn(programsTable, SWT.LEFT);
		tcDate.setText(colNames[1]);

		tcProgram.setWidth(220);
		tcDate.setWidth(200);

		tableViewer = new TableViewer(programsTable);

		tableViewer.getTable().setLinesVisible(true);
		tableViewer.getTable().setHeaderVisible(true);
		tableViewer.getTable().setLayoutData(grdTablePrograms);

		// Sets the content provider.
		tableViewer.setContentProvider(new PatchContentProvider());
		tableViewer.addFilter(new PatchTableFilter());

		// Sets the label provider.
		tableViewer.setLabelProvider(new PatchLabelProvider());

		tableViewer.setInput(detalhesDoPatch);
	}

	@Override
	protected Point getInitialSize() {
		if (detalhesDoPatch != null) {
			return new Point(450, 606);
		}

		return super.getInitialSize();
	}

}