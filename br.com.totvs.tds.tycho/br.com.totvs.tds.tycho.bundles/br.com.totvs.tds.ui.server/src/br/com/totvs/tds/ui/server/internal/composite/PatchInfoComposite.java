package br.com.totvs.tds.ui.server.internal.composite;

import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.server.interfaces.IPatchRpoInfo;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.internal.filter.PatchTableFilter;
import br.com.totvs.tds.ui.server.nl.Messages;

public class PatchInfoComposite extends Composite implements ISelectionChangedListener {

	private Label lblForced;
	private Label lbMessagePatchType;
	private TableViewer tableViewerSources;
	private Table tblPrograms;
	private Text txtAppBuild;
	private Text txtAppData;
	private Text txtGerBuild;

	private Text txtGerData;
	private Text txtRpoBuild;
	private Text txtRpoData;

	public PatchInfoComposite(Composite parent, int style) {
		super(parent, style);
		initialize();
	}

	/**
	 * Limpa os dados da tela.
	 */
	private void clearRpoInfoUI() {
		txtRpoData.setText(""); //$NON-NLS-1$
		txtRpoBuild.setText(""); //$NON-NLS-1$
		txtGerData.setText(""); //$NON-NLS-1$
		txtGerBuild.setText(""); //$NON-NLS-1$
		txtAppData.setText(""); //$NON-NLS-1$
		txtAppBuild.setText(""); //$NON-NLS-1$
		tblPrograms.removeAll();
		lblForced.setVisible(false);
		lbMessagePatchType.setVisible(false);
	}

	public void handleSelectionChanged(final SelectionChangedEvent event) {
		final ISelection selection = event.getSelection();
		final Object obj = ((IStructuredSelection) selection).getFirstElement();
		if (obj != null && obj instanceof IPatchRpoInfo) {
			IPatchRpoInfo patchItem = (IPatchRpoInfo) obj;
			if (patchItem.getTypePatch() == 99) {
				clearRpoInfoUI();
			} else {
				clearRpoInfoUI();
				txtRpoData.setText(patchItem.getRpoInfo().getDateGeneration());
				txtRpoBuild.setText(patchItem.getRpoInfo().getRpoVersion());
				txtGerData.setText(patchItem.getDateFileGeneration());
				txtGerBuild.setText(patchItem.getBuildFileGerenation());
				txtAppData.setText(patchItem.getDateFileApplication());
				txtAppBuild.setText(patchItem.getBuildFileApplication());

				Map<String, String> programs = patchItem.getAllPrograms();
				tableViewerSources.setInput(programs);
				if (!patchItem.getSkipOld()) { // Not Skip Old == Forced
					lblForced.setVisible(true);
				}
				lbMessagePatchType.setText(takePatchTypeMessage(patchItem));
				lbMessagePatchType.setVisible(true);
			}
		}
	}

	private void initialize() {

		GridLayout layoutGroupLegenda = new GridLayout(6, false);
		layoutGroupLegenda.verticalSpacing = 10;

		Group groupLegenda = new Group(this, SWT.NULL);
		groupLegenda.setText("   Tipos de patch   "); //$NON-NLS-1$
		groupLegenda.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		groupLegenda.setLayout(layoutGroupLegenda);

		Label lblImgCorrecao = new Label(groupLegenda, SWT.None);
		lblImgCorrecao.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));
		lblImgCorrecao.setImage(ServerUIIcons.getPatchCorrection().createImage(true));

		Label lblCorrecao = new Label(groupLegenda, SWT.None);
		lblCorrecao.setText(Messages.PatchInfoComposite_ptm_extension_file);
		lblCorrecao.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));

		// pacote *.pak
		Label lblImgPacote = new Label(groupLegenda, SWT.None);
		lblImgPacote.setImage(ServerUIIcons.getPatchPackage().createImage(true));
		lblImgPacote.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));

		Label lblPacote = new Label(groupLegenda, SWT.None);
		lblPacote.setText(Messages.PatchInfoComposite_apply_patch);
		lblPacote.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));

		// update *.upd
		Label lblImgUpdate = new Label(groupLegenda, SWT.None);
		lblImgUpdate.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));
		lblImgUpdate.setImage(ServerUIIcons.getPatchUpdate().createImage(true));

		Label lblUpdate = new Label(groupLegenda, SWT.None);
		lblUpdate.setText(Messages.PatchInfoComposite_upd_extension);

		lblUpdate.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));

		GridLayout layoutGroupRPO = new GridLayout(6, false);
		layoutGroupRPO.verticalSpacing = 10;

		lbMessagePatchType = new org.eclipse.swt.widgets.Label(this, SWT.BORDER_SOLID);
		lbMessagePatchType.setText(""); //$NON-NLS-1$

		lbMessagePatchType.setForeground(getShell().getDisplay().getSystemColor(SWT.COLOR_BLACK));
		lbMessagePatchType.setSize(10, 30);
		lbMessagePatchType.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		lbMessagePatchType.setVisible(false);

		Group groupRpo = new Group(this, SWT.NULL);
		groupRpo.setText("RPO"); //$NON-NLS-1$
		groupRpo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		groupRpo.setLayout(layoutGroupRPO);

		Label lblRpoData = new Label(groupRpo, SWT.NONE);
		lblRpoData.setText(Messages.PatchInfoComposite_date);
		lblRpoData.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtRpoData = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtRpoData.grabExcessHorizontalSpace = true;
		grdDatatxtRpoData.horizontalSpan = 2;

		txtRpoData = new Text(groupRpo, SWT.SINGLE | SWT.BORDER);
		txtRpoData.setLayoutData(grdDatatxtRpoData);
		txtRpoData.setEditable(false);
		Label lblRpoBuild = new Label(groupRpo, SWT.NONE);
		lblRpoBuild.setText(Messages.PatchInfoComposite_build);
		lblRpoBuild.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtRpoBuild = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtRpoBuild.grabExcessHorizontalSpace = true;
		grdDatatxtRpoBuild.horizontalSpan = 2;

		txtRpoBuild = new Text(groupRpo, SWT.SINGLE | SWT.BORDER);
		txtRpoBuild.setLayoutData(grdDatatxtRpoBuild);
		txtRpoBuild.setEditable(false);

		GridLayout layoutGroupGer = new GridLayout(6, false);
		layoutGroupGer.verticalSpacing = 10;

		Group groupGer = new Group(this, SWT.NULL);
		groupGer.setText(Messages.PatchInfoComposite_file_generate);
		groupGer.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		groupGer.setLayout(layoutGroupGer);
		Label lblGerData = new Label(groupGer, SWT.NONE);
		lblGerData.setText(Messages.PatchInfoComposite_date);
		lblGerData.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtGerData = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtGerData.grabExcessHorizontalSpace = true;
		grdDatatxtGerData.horizontalSpan = 2;

		txtGerData = new Text(groupGer, SWT.SINGLE | SWT.BORDER);
		txtGerData.setLayoutData(grdDatatxtGerData);
		txtGerData.setEditable(false);
		Label lblGerBuild = new Label(groupGer, SWT.NONE);
		lblGerBuild.setText(Messages.PatchInfoComposite_build);
		lblGerBuild.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtGerBuild = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtGerBuild.grabExcessHorizontalSpace = true;
		grdDatatxtGerBuild.horizontalSpan = 2;

		txtGerBuild = new Text(groupGer, SWT.SINGLE | SWT.BORDER);
		txtGerBuild.setLayoutData(grdDatatxtGerBuild);
		txtGerBuild.setEditable(false);

		GridLayout layoutGroupApp = new GridLayout(6, false);
		layoutGroupApp.verticalSpacing = 10;

		Group groupApp = new Group(this, SWT.NULL);
		groupApp.setText(Messages.PatchInfoComposite_qpply_file);
		groupApp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		groupApp.setLayout(layoutGroupApp);
		Label lblAppData = new Label(groupApp, SWT.NONE);
		lblAppData.setText(Messages.PatchInfoComposite_date);
		lblAppData.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtAppData = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtAppData.grabExcessHorizontalSpace = true;
		grdDatatxtAppData.horizontalSpan = 2;

		txtAppData = new Text(groupApp, SWT.SINGLE | SWT.BORDER);
		txtAppData.setLayoutData(grdDatatxtAppData);
		txtAppData.setEditable(false);
		Label lblAppBuild = new Label(groupApp, SWT.NONE);
		lblAppBuild.setText(Messages.PatchInfoComposite_build);
		lblAppBuild.setLayoutData(new GridData(GridData.END));

		GridData grdDatatxtAppBuild = new GridData(GridData.FILL_HORIZONTAL);
		grdDatatxtAppBuild.grabExcessHorizontalSpace = true;
		grdDatatxtAppBuild.horizontalSpan = 2;

		txtAppBuild = new Text(groupApp, SWT.SINGLE | SWT.BORDER);
		txtAppBuild.setLayoutData(grdDatatxtAppBuild);
		txtAppBuild.setEditable(false);

		// LABEL COM MENSAGEM DE AVISO SOBRE PATCH FOR�aDO !!!!
		Color myRed = new Color(null, 231, 96, 97);
		lblForced = new Label(this, SWT.BORDER);
		lblForced.setText(Messages.PatchInfoComposite_overwrite_warning);
		Font fontPad = lblForced.getFont();
		FontData[] fontData = fontPad.getFontData();
		for (int i = 0; i < fontData.length; i++) {
			fontData[i].setHeight(12);
		}
		Font newFont = new Font(getShell().getDisplay(), fontData);
		lblForced.setFont(newFont);
		lblForced.setBackground(myRed);
		lblForced.setForeground(getShell().getDisplay().getSystemColor(SWT.COLOR_WHITE));
		lblForced.setSize(10, 30);
		lblForced.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		lblForced.setVisible(false);

		// GridData da tabela esticando na vertical
		GridData grdTablePrograms = new GridData(GridData.FILL_BOTH);
		grdTablePrograms.grabExcessVerticalSpace = true;
		grdTablePrograms.verticalSpan = 3;

		final String[] colNames = new String[] { Messages.PatchInfoComposite_program, Messages.PatchInfoComposite_date };

		tblPrograms = new Table(this, SWT.BORDER);

		TableColumn tcProgram = new TableColumn(tblPrograms, SWT.LEFT);
		tcProgram.setText(colNames[0]);

		TableColumn tcDate = new TableColumn(tblPrograms, SWT.LEFT);
		tcDate.setText(colNames[1]);

		tcProgram.setWidth(220);
		tcDate.setWidth(200);

		tableViewerSources = new TableViewer(tblPrograms);

		tableViewerSources.getTable().setLinesVisible(true);
		tableViewerSources.getTable().setHeaderVisible(true);
		tableViewerSources.getTable().setLayoutData(grdTablePrograms);

		tableViewerSources.addFilter(new PatchTableFilter());

		tableViewerSources.setContentProvider(new IStructuredContentProvider() {
			@Override
			public void dispose() {
			}

			@Override
			@SuppressWarnings({ "unchecked" })
			public Object[] getElements(final Object inputElement) {
				Object[] array = ((Map<String, String>) inputElement).entrySet().toArray();
				return array;
			}

			@Override
			public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
			}
		});

		tableViewerSources.setLabelProvider(new ITableLabelProvider() {
			@Override
			public void addListener(final ILabelProviderListener listener) {
			}

			@Override
			public void dispose() {
			}

			@Override
			public Image getColumnImage(final Object element, final int columnIndex) {
				if (columnIndex == 0) {
					return ServerUIIcons.getTemplate().createImage(true);
				}
				return null;
			}

			@SuppressWarnings("unchecked")
			@Override
			public String getColumnText(final Object element, final int columnIndex) {
				Entry<String, String> entry = (Entry<String, String>) element;
				if (columnIndex == 0) {
					return entry.getKey();
				} else if (columnIndex == 1) {
					return entry.getValue();
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
		});
	}

	@Override
	public void selectionChanged(final SelectionChangedEvent event) {
		handleSelectionChanged(event);
	}

	/**
	 * Obtem a mensagem do recurso
	 */
	private String takePatchTypeMessage(final IPatchRpoInfo tmp) {

		String messageTypePatch = ""; //$NON-NLS-1$

		switch (tmp.getTypePatch()) {
		case 1:
			messageTypePatch = Messages.PatchInfoComposite_apply_upd_message; // UPDATE Verde = 1
			break;
		case 2:
			messageTypePatch = Messages.PatchInfoComposite_apply_patch_message; // Pak Amarelo = 2
			break;
		case 3:
			messageTypePatch = Messages.PatchInfoComposite_16; // Branco Patch = 3
			break;

		default:
			messageTypePatch = ""; //$NON-NLS-1$
			break;
		}

		return messageTypePatch;

	}
}
