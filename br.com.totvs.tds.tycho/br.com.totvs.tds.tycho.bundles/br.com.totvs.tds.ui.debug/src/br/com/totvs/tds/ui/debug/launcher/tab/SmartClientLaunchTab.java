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
	private Button chkMultiSession;
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
		lblDirRemote.setText(Messages.SmartClientLaunchTab_Executable);

		this.lblNewLabel_1 = new Label(group, SWT.WRAP);
		// lblNewLabel_1.setBackground(SWTResourceManager.getColor(SWT.COLOR_INFO_BACKGROUND));
		lblNewLabel_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		// this.lblNewLabel_1.setFont(SWTResourceManager.getFont("Segoe UI", 8,
		// SWT.ITALIC));
		this.lblNewLabel_1.setText(Messages.SmartClientLaunchTab_Server_associated_SmartClient);
		new Label(group, SWT.NONE);
		new Label(group, SWT.NONE);

		final Label lblNewLabel = new Label(group, SWT.NONE);
		lblNewLabel.setText(Messages.SmartClientLaunchTab_Main_Function);
		txtMainFunction = new Text(group, SWT.SINGLE | SWT.BORDER);
		txtMainFunction.setToolTipText(Messages.SmartClientLaunchTab_Function_start_process);
		txtMainFunction.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		final Label lblParametros = new Label(group, SWT.NONE);
		lblParametros.setToolTipText(Messages.SmartClientLaunchTab_Parameters_for_main_function);
		lblParametros.setText(Messages.SmartClientLaunchTab_Parameters);
		txtParameters = new Text(group, SWT.SINGLE | SWT.BORDER);
		txtParameters.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		final Group grpArgumentos = new Group(comp, SWT.NONE);
		grpArgumentos.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		final GridLayout glGrpArgumentos = new GridLayout(2, false);
		grpArgumentos.setLayout(glGrpArgumentos);
		grpArgumentos.setText(Messages.SmartClientLaunchTab_Arguments);

		chkMultiSession = new Button(grpArgumentos, SWT.CHECK);
		chkMultiSession.setToolTipText(Messages.SmartClientLaunchTab_Allows_running_more_instances_SmartClient);
		chkMultiSession.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkMultiSession.setText(Messages.SmartClientLaunchTab_Multiple_sessions);
		chkMultiSession.addSelectionListener(defaultSelectionListener);

		chkAccess = new Button(grpArgumentos, SWT.CHECK);
		chkAccess.setToolTipText(Messages.SmartClientLaunchTab_Activates_accessibility_module);
		chkAccess.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkAccess.setText(Messages.SmartClientLaunchTab_Accessibility_module);
		chkAccess.addSelectionListener(defaultSelectionListener);

		chkSplash = new Button(grpArgumentos, SWT.CHECK);
		chkSplash.setToolTipText(Messages.SmartClientLaunchTab_Not_display_splash);
		chkSplash.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkSplash.setText(Messages.SmartClientLaunchTab_Does_not_display_splash);
		chkSplash.addSelectionListener(defaultSelectionListener);
		new Label(grpArgumentos, SWT.NONE);

		chkLanguage = new Button(grpArgumentos, SWT.CHECK);
		chkLanguage.setToolTipText(Messages.SmartClientLaunchTab_Language_use_during_execution);
		chkLanguage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkLanguage.setText(Messages.SmartClientLaunchTab_Language);

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
		grpOptions.setText(Messages.SmartClientLaunchTab_Options);
		chkEnableProfile = new Button(grpOptions, SWT.CHECK);
		chkEnableProfile.setText(Messages.SmartClientLaunchTab_Execution_Profile);
		new Label(grpOptions, SWT.NONE);

		chkStopAtFirstKnowSource = new Button(grpOptions, SWT.CHECK);
		chkStopAtFirstKnowSource.setEnabled(false);
		chkStopAtFirstKnowSource.setText(Messages.SmartClientLaunchTab_Stop_first_known_source);

		chkMultiThread = new Button(grpOptions, SWT.CHECK);
		chkMultiThread.setText(Messages.SmartClientLaunchTab_Multi_thread);
		chkMultiThread.addSelectionListener(defaultSelectionListener);
		new Label(grpOptions, SWT.NONE);

		lblpOuSuperior = new Label(grpOptions, SWT.WRAP);
		lblpOuSuperior.setEnabled(false);
		lblpOuSuperior.setText(Messages.SmartClientLaunchTab_P17_or_higher);

		chkShowCommandLine = new Button(grpOptions, SWT.CHECK);
		chkShowCommandLine.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		chkShowCommandLine.setSelection(true);
		chkShowCommandLine.setText(Messages.SmartClientLaunchTab_Display_command_line);
		new Label(grpOptions, SWT.NONE);
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
			DebugUIActivator.showStatus(IStatus.ERROR, e.getMessage());
		} catch (final IllegalAccessException e) {
			DebugUIActivator.showStatus(IStatus.ERROR, e.getMessage());
		} catch (final CoreException e) {
			DebugUIActivator.showStatus(IStatus.ERROR, e.getMessage());
		}
	}

	@Override
	public final void setDefaults(final ILaunchConfigurationWorkingCopy configuration) {
		final LaunchParameters lp = new LaunchParameters();

		try {
			configuration.getAttributes().put(IServerConstants.LAUNCH_PARAMETERS, lp);
		} catch (final CoreException e) {
			DebugUIActivator.logStatus(IStatus.ERROR, Messages.SmartClientLaunchTab_Performer, e.getMessage());
		}

		// setWidgtesValue(lp);
	}

	private void setWidgtesValue(final LaunchParameters launchParameters) {

		txtMainFunction.setText(launchParameters.getMainProgram());
		chkMultiThread.setSelection(launchParameters.isEnableMultiThread());
		chkShowCommandLine.setSelection(launchParameters.isShowCommandLine());
		chkEnableProfile.setSelection(launchParameters.isEnableProfile());
		chkMultiSession.setSelection(launchParameters.isMultiSession());
		chkAccess.setSelection(launchParameters.isAccessibilityMode());
		chkSplash.setSelection(launchParameters.isNotShowSplash());
		cmbLanguage.setText(launchParameters.getLanguage());

		final String mode = ""; //$NON-NLS-1$
		if (mode == "run") { //$NON-NLS-1$
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
		lp.setMultiSession(chkMultiSession.getSelection());
		lp.setAccessibilityMode(chkAccess.getSelection());
		lp.setDoNotShowSplash(chkSplash.getSelection());
		lp.setLanguage(cmbLanguage.getText().trim());
		lp.setTrace(true);
		lp.setEnableLaunchPauseToDebug(false);
		lp.setLogFile("r:\\debug.log"); //$NON-NLS-1$
		lp.setIgnoreFilesNotInWS(false);
		lp.setEnableTableSync(false);

		if (chkStopAtFirstKnowSource != null) {
			// lp.setStopAtFirtLine(chkStopAtFirstKnowSource.getSelection());
		}

		try {
			configuration.setAttribute(IServerConstants.LAUNCH_PARAMETERS, lp.toMap());
		} catch (final IllegalArgumentException e) {
			DebugUIActivator.showStatus(IStatus.WARNING, e.getMessage(), e);
		} catch (final IllegalAccessException e) {
			DebugUIActivator.showStatus(IStatus.WARNING, e.getMessage(), e);
		}
	}

	@Override
	public final String getName() {
		return TAB_NAME;
	}

}