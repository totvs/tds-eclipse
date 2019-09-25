package br.com.totvs.tds.ui.debug.launcher.tab;

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.ui.debug.DebugUIActivator;
import br.com.totvs.tds.ui.debug.helper.LaunchParameters;
import br.com.totvs.tds.ui.debug.launcher.IDebugLauncherAttributes;

/**
 *
 * TabConfiguration que representa a tela de configuração do SmartClient para
 * execução, depuração e cobertura de arquivos.
 *
 * @author acandido
 *
 */
public class SmartClientLaunchTab extends AbstractLaunchConfigurationTab {
	private static final IDebugLauncherAttributes.SCLanguages portuguese = IDebugLauncherAttributes.SCLanguages.PORTUGUESE;
	private static final IDebugLauncherAttributes.SCLanguages spanish = IDebugLauncherAttributes.SCLanguages.SPANISH;
	private static final IDebugLauncherAttributes.SCLanguages english = IDebugLauncherAttributes.SCLanguages.ENGLISH;

	private static final String[] scLanguages = { "", portuguese.toString(), english.toString(), spanish.toString() }; //$NON-NLS-1$

	private Text txtMainFunction;
	private Text txtParameters;

	// argumentos
	private Button chkMultiSelection;
	// private Button chLayers;
	private Button chkSplash;
	private Button chkLanguage;
	private Combo cmbLanguage;
	private Button chkAccess;
	private Button chkEnableProfile;

	private DefaultSelectionListener defaultSelectionListener = new DefaultSelectionListener();

	private static final String TAB_NAME = "TOTVS SmartClient"; //$NON-NLS-1$
	private Label lblNewLabel_1;
	private Button chkMultiThread;
	private Button chkStopAtFirstKnowSource;
	private Label lblpOuSuperior;
	private Button chkShowCommandLine;

	/**
	 * Action de seleção.
	 *
	 * @author daniel.yampolschi
	 *
	 */
	private class DefaultSelectionListener extends SelectionAdapter {
		@Override
		public void widgetSelected(final SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}

		@Override
		public void widgetDefaultSelected(final SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}
	}

	/**
	 * The constructor.
	 *
	 * @param mode The execution mode. Eg.: Run, Debug, Coverage, etc.
	 */
	public SmartClientLaunchTab(final String mode) {
	}

	/**
	 * @wbp.parser.entryPoint
	 */
	@Override
	public final void createControl(final Composite parent) {
		final Composite comp = new Composite(parent, SWT.FILL);
		setControl(comp);

		final GridLayout glComp = new GridLayout(1, false);
		comp.setLayout(glComp);

		final Group group = new Group(comp, SWT.NONE);
		group.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		final GridLayout glGroup = new GridLayout(2, false);
		group.setLayout(glGroup);
		group.setText("SmartClient"); //$NON-NLS-1$

		final Label lblDirRemote = new Label(group, SWT.NONE);
		lblDirRemote.setText("Execut\u00E1vel");

		this.lblNewLabel_1 = new Label(group, SWT.WRAP);
		// lblNewLabel_1.setBackground(SWTResourceManager.getColor(SWT.COLOR_INFO_BACKGROUND));
		lblNewLabel_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		// this.lblNewLabel_1.setFont(SWTResourceManager.getFont("Segoe UI", 8,
		// SWT.ITALIC));
		this.lblNewLabel_1.setText("*Ser\u00E1 utilizado o SmartClient associado ao servidor.");
		new Label(group, SWT.NONE);
		new Label(group, SWT.NONE);

		final Label lblNewLabel = new Label(group, SWT.NONE);
		lblNewLabel.setText("(-P) Função principal");
		txtMainFunction = new Text(group, SWT.SINGLE | SWT.BORDER);
		txtMainFunction.setToolTipText("Função que será utilizada para iniciar o processo.");
		txtMainFunction.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		final Label lblParametros = new Label(group, SWT.NONE);
		lblParametros.setToolTipText("Informe os parâmetros para a função principal.");
		lblParametros.setText("(-A) Parâmetros");
		txtParameters = new Text(group, SWT.SINGLE | SWT.BORDER);
		txtParameters.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		final Group grpArgumentos = new Group(comp, SWT.NONE);
		grpArgumentos.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		final GridLayout glGrpArgumentos = new GridLayout(2, false);
		grpArgumentos.setLayout(glGrpArgumentos);
		grpArgumentos.setText("Argumentos");

		chkMultiSelection = new Button(grpArgumentos, SWT.CHECK);
		chkMultiSelection.setToolTipText("Permite a execução de uma ou mais instâncias do TOTVS SmartClient.");
		chkMultiSelection.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkMultiSelection.setText("(-M) múltiplas sessões");
		chkMultiSelection.addSelectionListener(defaultSelectionListener);

		chkAccess = new Button(grpArgumentos, SWT.CHECK);
		chkAccess.setToolTipText("Ativa o módulo de acessibilidade.");
		chkAccess.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkAccess.setText("(-AC) Módulo de acessibilidade");
		chkAccess.addSelectionListener(defaultSelectionListener);

		chkSplash = new Button(grpArgumentos, SWT.CHECK);
		chkSplash.setToolTipText("Não apresenta a imagem de abertura.");
		chkSplash.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkSplash.setText("(-Q) não apresentar 'splash'");
		chkSplash.addSelectionListener(defaultSelectionListener);
		new Label(grpArgumentos, SWT.NONE);

		chkLanguage = new Button(grpArgumentos, SWT.CHECK);
		chkLanguage.setToolTipText("Idioma a ser utilizado durante a execução.");
		chkLanguage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkLanguage.setText("(-L) Idioma");

		cmbLanguage = new Combo(grpArgumentos, SWT.NONE);
		cmbLanguage.setItems(scLanguages);
		cmbLanguage.setData(portuguese.toString(), portuguese);
		cmbLanguage.setData(english.toString(), english);
		cmbLanguage.setData(spanish.toString(), spanish);
		cmbLanguage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		final Group grpOptions = new Group(comp, SWT.NONE);
		grpOptions.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		final GridLayout glGrpOpes = new GridLayout(3, false);
		grpOptions.setLayout(glGrpOpes);
		grpOptions.setText("Opções");
		chkEnableProfile = new Button(grpOptions, SWT.CHECK);
		chkEnableProfile.setText("Perfil de execução");

		chkMultiThread = new Button(grpOptions, SWT.CHECK);
		chkMultiThread.setText("Multi-thread");

		chkStopAtFirstKnowSource = new Button(grpOptions, SWT.CHECK);
		chkStopAtFirstKnowSource.setText("*Parar no primeiro fonte conhecido");

		chkShowCommandLine = new Button(grpOptions, SWT.CHECK);
		chkShowCommandLine.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		chkShowCommandLine.setSelection(true);
		chkShowCommandLine.setText("Apresentar linha de comando");

		lblpOuSuperior = new Label(grpOptions, SWT.WRAP);
		lblpOuSuperior.setText("*P17 ou superior");
		// lblpOuSuperior.setFont(SWTResourceManager.getFont("Segoe UI", 8,
		// SWT.ITALIC));
		chkMultiThread.addSelectionListener(defaultSelectionListener);
		chkShowCommandLine.addSelectionListener(defaultSelectionListener);
		chkEnableProfile.addSelectionListener(defaultSelectionListener);
		chkStopAtFirstKnowSource.addSelectionListener(defaultSelectionListener);

		initializeActions();

	}

	/*
	 * Define ações dos componentes da tela.
	 */
	private void initializeActions() {

		txtMainFunction.addModifyListener(e -> updateLaunchConfigurationDialog());
		txtParameters.addModifyListener(e -> updateLaunchConfigurationDialog());
		chkLanguage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				cmbLanguage.select(chkLanguage.getSelection() ? 1 : 0);
				updateLaunchConfigurationDialog();
			}
		});
		cmbLanguage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				internalWidgetSelected(e);
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				internalWidgetSelected(e);
			}

			private void internalWidgetSelected(final SelectionEvent e) {
				chkLanguage.setSelection(cmbLanguage.getSelectionIndex() > 0);
				updateLaunchConfigurationDialog();
			}
		});
		cmbLanguage.addModifyListener(e -> {
			chkLanguage.setSelection(!cmbLanguage.getText().isEmpty());
			updateLaunchConfigurationDialog();
		});
	}

	@Override
	public final boolean isValid(final ILaunchConfiguration launchConfig) {
		final boolean isValid = getErrorMessage() == null;

		return isValid;
	}

	@SuppressWarnings("unchecked")
	@Override
	public final void initializeFrom(final ILaunchConfiguration configuration) {

		try {
			final LaunchParameters lp = new LaunchParameters();
			final Map<String, Object> attributes = configuration.getAttributes();
			lp.fromMap((Map<String, Object>) attributes.getOrDefault(IServerConstants.LAUNCH_PARAMETERS, null));
			setWidgtesValue(lp);
		} catch (final IllegalArgumentException e) {
			DebugUIActivator.showStatus(IStatus.ERROR, "Executor", e.getMessage());
		} catch (final IllegalAccessException e) {
			DebugUIActivator.showStatus(IStatus.ERROR, "Executor", e.getMessage());
		} catch (final CoreException e) {
			DebugUIActivator.showStatus(IStatus.ERROR, "Executor", e.getMessage());
		}
	}

	@Override
	public final void setDefaults(final ILaunchConfigurationWorkingCopy configuration) {
		final LaunchParameters lp = new LaunchParameters();

		try {
			configuration.getAttributes().put(IServerConstants.LAUNCH_PARAMETERS, lp);
		} catch (final CoreException e) {
			DebugUIActivator.logStatus(IStatus.ERROR, "Executor", e.getMessage());
		}

		// setWidgtesValue(lp);
	}

	private void setWidgtesValue(final LaunchParameters launchParameters) {

		txtMainFunction.setText(launchParameters.getMainProgram());
		chkMultiThread.setSelection(launchParameters.isEnableMultiThread());
		chkShowCommandLine.setSelection(launchParameters.isShowCommandLine());
		chkEnableProfile.setSelection(launchParameters.isEnableProfile());
		chkMultiThread.setSelection(launchParameters.isMultiSession());
		chkAccess.setSelection(launchParameters.isAccessibilityMode());
		chkSplash.setSelection(launchParameters.isNotShowSplash());
		cmbLanguage.setText(launchParameters.getLanguage());

		final String mode = "";
		if (mode == "run") {
			// chkStopAtFirstKnowSource.setSelection(launchParameters.isStopAtFirtLine());
//			chkTableSync.setSelection(launchParameters.isEnableTableSync());
//			chkTrace.setSelection(launchParameters.isTrace());
//			chkLogFile.setSelection(launchParameters.isLogFile());
//			chkI.setSelection(launchParameters.isIgnoreFilesNotInWS());
		}
	}

	@Override
	public final void performApply(final ILaunchConfigurationWorkingCopy configuration) {
		final LaunchParameters lp = new LaunchParameters();

		lp.setMainProgram(txtMainFunction.getText().trim());
		lp.setEnableMultiThread(chkMultiThread.getSelection());
		lp.setShowCommandLine(chkShowCommandLine.getSelection());
		lp.setEnableProfile(chkEnableProfile.getSelection());
		lp.setMultiSession(chkMultiSelection.getSelection());
		lp.setAccessibilityMode(chkAccess.getSelection());
		lp.setDoNotShowSplash(chkSplash.getSelection());
		lp.setLanguage(cmbLanguage.getText().trim());
		lp.setTrace(false);
		lp.setLogFile("");
		lp.setIgnoreFilesNotInWS(false);
		lp.setEnableTableSync(false);

		if (chkStopAtFirstKnowSource != null) {
			// lp.setStopAtFirtLine(chkStopAtFirstKnowSource.getSelection());
		}

		try {
			configuration.setAttribute(IServerConstants.LAUNCH_PARAMETERS, lp.toMap());
		} catch (final IllegalArgumentException e) {
			DebugUIActivator.showStatus(IStatus.WARNING, "Executor", e.getMessage(), e);
		} catch (final IllegalAccessException e) {
			DebugUIActivator.showStatus(IStatus.WARNING, "Executor", e.getMessage(), e);
		}
	}

	@Override
	public final String getName() {
		return TAB_NAME;
	}

}