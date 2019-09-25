package br.com.totvs.tds.ui.sdk.preference.compilation;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wb.swt.FieldLayoutPreferencePage;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

/**
 * Classe CompilePreferencePage.
 * 
 * @author leo.watanabe
 *
 */
public class CompilePreferencePage extends FieldLayoutPreferencePage implements IWorkbenchPreferencePage {

	private Composite composite;
	private Label lblCompilao;
	private Composite composite_1;
	private Composite composite_3;

	final String[][] LOG_LEVEL_OPTIONS = new String[][] { { "Erros", "0" }, { "Avisos", "1" }, { "Informativo", "2" },
			{ "Detalhado", "3" }, { "Depuração", "4" } }; 

	final String[][] MULT_OPTIONS = new String[][] { { "1", "1" }, { "2", "2" }, { "3", "3" }, { "4", "4" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
			{ "5", "5" }, { "6", "6" }, { "7", "7" }, { "8", "8" } }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$

	private Composite composite_4;
	private Label lblNewLabel;
	private Composite composite_5;
	private Composite composite_6;
	private Composite composite_7;
	private final FormToolkit formToolkit = new FormToolkit(Display.getDefault());
	private Label lblNewLabel_1;
	private Composite composite_8;
	private Label lblNmeroDeProcessos;

	/**
	 * Construtor.
	 */
	public CompilePreferencePage() {
		super();

		setPreferenceStore(SdkUIActivator.getDefault().getPreferenceStore());
		setDescription("Configuração do processo de compilação");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(final IWorkbench workbench) {
	}

	@Override
	protected Control createPageContents(final Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		container.setLayout(new FillLayout(SWT.VERTICAL));

		this.composite = new Composite(container, SWT.NONE);
		this.composite.setLayout(new GridLayout(2, false));

		this.lblCompilao = new Label(this.composite, SWT.NONE);
		this.lblCompilao.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		this.lblCompilao.setBounds(0, 0, 55, 15);
		this.lblCompilao.setText("Processo");
		new Label(this.composite, SWT.NONE);

		this.composite_3 = new Composite(this.composite, SWT.NONE);
		this.composite_3.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new ComboFieldEditor(ISDKPreferenceKeys.LOG_LEVEL, "N�vel de detalhe", LOG_LEVEL_OPTIONS,
				this.composite_3));
		new Label(composite, SWT.NONE);

		this.composite_1 = new Composite(this.composite, SWT.NONE);
		this.composite_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new BooleanFieldEditor(ISDKPreferenceKeys.LOG_TO_FILE,
				"Gravar ocorr�ncias da execução em arquivo", BooleanFieldEditor.DEFAULT, this.composite_1));
		new Label(this.composite, SWT.NONE);

		this.composite_4 = new Composite(this.composite, SWT.NONE);
		this.composite_4.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new DirectoryFieldEditor(ISDKPreferenceKeys.FOLDER_LOG_FILE, "Local", this.composite_4));

		this.lblNewLabel_1 = new Label(this.composite, SWT.NONE);
		formToolkit.adapt(this.lblNewLabel_1, true, true);
		new Label(this.composite, SWT.NONE);

		this.lblNewLabel = new Label(this.composite, SWT.NONE);
		this.lblNewLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		this.lblNewLabel.setText("Arquivos a serem compilados");
		new Label(this.composite, SWT.NONE);

		this.composite_5 = new Composite(this.composite, SWT.NONE);
		this.composite_5.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new BooleanFieldEditor(ISDKPreferenceKeys.DESIRE_NO_COMPILE_TAG,
				"Marcar fontes não compilados", BooleanFieldEditor.DEFAULT, composite_5));
		new Label(this.composite, SWT.NONE);

		this.composite_6 = new Composite(this.composite, SWT.NONE);
		this.composite_6.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new StringFieldEditor(ISDKPreferenceKeys.NO_COMPILE_TAG, "Identificar com", composite_6));
		new Label(this.composite, SWT.NONE);

		this.composite_7 = new Composite(this.composite, SWT.NONE);
		this.composite_7.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new BooleanFieldEditor(ISDKPreferenceKeys.DESIRE_NO_COMPILE_IMAGE, "Mostra �cone",
				BooleanFieldEditor.DEFAULT, composite_7));
		new Label(composite, SWT.NONE);

		lblNmeroDeProcessos = new Label(composite, SWT.NONE);
		formToolkit.adapt(lblNmeroDeProcessos, true, true);

		this.lblNewLabel = new Label(this.composite, SWT.NONE);
		this.lblNewLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		this.lblNewLabel.setText("N�mero de processos simult�neos");
		new Label(this.composite, SWT.NONE);

		this.composite_8 = new Composite(this.composite, SWT.NONE);
		this.composite_8.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new ComboFieldEditor(ISDKPreferenceKeys.MULT_PROCESS, "Quantidade", MULT_OPTIONS,
				this.composite_8));

		this.lblNewLabel = new Label(this.composite, SWT.NONE);
		this.lblNewLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		this.lblNewLabel.setText("Console");
		new Label(this.composite, SWT.NONE);

		this.composite_5 = new Composite(this.composite, SWT.NONE);
		this.composite_5.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		addField(new BooleanFieldEditor(ISDKPreferenceKeys.CLEAR_CONSOLE_LOG,
				"Limpar o Console antes de compilar", BooleanFieldEditor.DEFAULT, composite_5));
		new Label(this.composite, SWT.NONE);

		return container;
	}

	@Override
	public boolean isValid() {
		return true;
	}
}
