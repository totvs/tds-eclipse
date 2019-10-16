package br.com.totvs.tds.ui.server.wizards.patch;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchMode;

/**
 * Exibe detalhes da valida��o do Patch.
 *
 * @author leo.watanabe
 *
 */
public class PatchValidationDetailsDialog extends Dialog {

	private List<String[]> oldPrograms;
	private ApplyPatchMode applyMode = ApplyPatchMode.VALIDATE_PATCH;

	private TableViewer tableViewer;

	private Button btnApplyAll;
	private Button btnApplyValidOnly;
	private Button btnSave;
	private boolean isSaveAction;

	/**
	 * @wbp.parser.constructor
	 */
	protected PatchValidationDetailsDialog(final Shell parentShell, final List<String[]> oldPrograms,
			ApplyPatchFileReturn applyPatchFileReturn) {
		super(parentShell);
		this.oldPrograms = oldPrograms;
	}

	@Override
	protected void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Validação do Pacote de Atualização");
	}

	private boolean containsOldPrograms() {
		return oldPrograms != null && !oldPrograms.isEmpty();
	}

	@Override
	protected Control createButtonBar(final Composite parent) {
		Control buttonBar = super.createButtonBar(parent);
		getButton(OK).setEnabled(false);
		return buttonBar;
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		Composite container = (Composite) super.createDialogArea(parent);
		container.setLayout(new GridLayout(2, false));

		Label lblAtencao = new Label(container, SWT.NONE);
		lblAtencao.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 2, 1));
		lblAtencao.setAlignment(SWT.CENTER);
		lblAtencao.setText("ATENÇÃO");

		if (containsOldPrograms()) {
			Label lblNoPacoteDe = new Label(container, SWT.NONE);
			lblNoPacoteDe.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 2, 1));
			lblNoPacoteDe.setText(
					"No pacote de atualização selecionado, há fontes mais antigos que os fontes contidos no RPO.");
		}

		Label lblAplicacaoDeste = new Label(container, SWT.NONE);
		lblAplicacaoDeste.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 2, 1));
		lblAplicacaoDeste.setText("A aplicação deste pacote pode comprometer o funcionamento do seu sistema.");

		if (containsOldPrograms()) {
			createProgramTableViewer(container);
		}

		SelectionListener listener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent arg0) {
				dialogChanged();
			}
		};

		if (containsOldPrograms()) {
			Composite composite = new Composite(container, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 2, 1));
			composite.setLayout(new GridLayout(2, false));

			btnApplyAll = new Button(composite, SWT.RADIO);
			btnApplyAll.setLayoutData(new GridData(SWT.LEFT, SWT.BOTTOM, false, false, 1, 1));
			btnApplyAll.setText("Aceito a aplicação do pacote");
			btnApplyAll.setSelection(false);
			btnApplyAll.addSelectionListener(listener);

			btnApplyValidOnly = new Button(composite, SWT.RADIO);
			btnApplyValidOnly.setLayoutData(new GridData(SWT.LEFT, SWT.BOTTOM, false, false, 1, 1));
			btnApplyValidOnly.setText("Aceito somente os fontes atualizados");
			btnApplyValidOnly.setSelection(false);
			btnApplyValidOnly.addSelectionListener(listener);

			btnSave = new Button(composite, SWT.CHECK);
			btnSave.setText("Salvar");
			btnSave.setSelection(false);
			btnSave.addSelectionListener(listener);
		}

		// XXX SOURCESTAMP: Projeto do Source Stamp interrompido por tempo indeterminado
		// if (enablePatchSignedOption && !patchSigned) {
		// btnApplySignedPatch = new Button(container, SWT.CHECK);
		// btnApplySignedPatch.setLayoutData(new GridData(SWT.LEFT, SWT.BOTTOM, false,
		// false, 1, 1));
		// btnApplySignedPatch.setText(Messages.PatchValidationDetailsDialog_7);
		// btnApplySignedPatch.setSelection(false);
		// btnApplySignedPatch.addSelectionListener(listener);
		// }

		return container;
	}

	private void createProgramTableViewer(final Composite composite) {
		tableViewer = new TableViewer(composite, SWT.BORDER | SWT.FULL_SELECTION);
		Table table = tableViewer.getTable();
		table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		TableViewerColumn tblclmnPrograma = new TableViewerColumn(tableViewer, SWT.NONE);
		tblclmnPrograma.getColumn().setWidth(150);
		tblclmnPrograma.getColumn().setText("Programa");
		tblclmnPrograma.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				String[] strings = (String[]) element;
				return strings[0];
			}
		});

		TableViewerColumn tblclmnDataNoPacote = new TableViewerColumn(tableViewer, SWT.NONE);
		tblclmnDataNoPacote.getColumn().setWidth(200);
		tblclmnDataNoPacote.getColumn().setText("Data no Pacote");
		tblclmnDataNoPacote.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				String[] strings = (String[]) element;
				return strings[1];
			}
		});

		TableViewerColumn tblclmnDataNoRpo = new TableViewerColumn(tableViewer, SWT.NONE);
		tblclmnDataNoRpo.getColumn().setWidth(200);
		tblclmnDataNoRpo.getColumn().setText("Data no RPO");
		tblclmnDataNoRpo.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				String[] strings = (String[]) element;
				return strings[2];
			}
		});

		tableViewer.setContentProvider(ArrayContentProvider.getInstance());
		tableViewer.setInput(oldPrograms);
	}

	/**
	 * Handler do evento de mudan�as na tela.
	 */
	private void dialogChanged() {
		boolean enabled = false;
		applyMode = ApplyPatchMode.VALIDATE_PATCH;
		//
		// XXX SOURCESTAMP: Projeto do Source Stamp interrompido por tempo indeterminado
		// if (!enablePatchSignedOption || patchSigned ||
		// btnApplySignedPatch.getSelection()) {
		if (btnApplyAll != null && btnApplyAll.getSelection()) {
			applyMode = ApplyPatchMode.APPLY_ALL;
			enabled = true;
		} else if (btnApplyValidOnly != null && btnApplyValidOnly.getSelection()) {
			applyMode = ApplyPatchMode.APPLY_NEWEST_ONLY;
			enabled = true;
		}
		// XXX SOURCESTAMP: Projeto do Source Stamp interrompido por tempo indeterminado
		// else if (btnApplySignedPatch != null && btnApplySignedPatch.getSelection()) {
		// applyMode = ApplyPatchMode.UNSIGNED;
		// enabled = true;
		// }

		if (btnSave != null) {
			isSaveAction = btnSave.getSelection();
		}
		// }
		//
		getButton(OK).setEnabled(enabled);
	}

	public ApplyPatchMode getApplyMode() {
		return applyMode;
	}

	@Override
	protected boolean isResizable() {
		return true;
	}

	public boolean isSaveAction() {
		return isSaveAction;
	}

}
