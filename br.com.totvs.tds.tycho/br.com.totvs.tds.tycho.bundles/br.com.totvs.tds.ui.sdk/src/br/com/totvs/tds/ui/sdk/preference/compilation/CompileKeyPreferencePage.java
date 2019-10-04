package br.com.totvs.tds.ui.sdk.preference.compilation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Properties;
import java.util.StringJoiner;

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.sdk.SdkUIActivator;

/**
 * Classe CompileKeyPreferencePage.
 *
 * @author leo.watanabe
 * @author matheus.sales(07/2018)
 *
 */
public class CompileKeyPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	public final static String PAGE_ID = "br.com.totvs.tds.ui.sdk.preference.compilation.compileKeyPreferencePage";

	private Text txtIdLocal;
	private Text txtIdFile;
	private Text txtGeneratedAt;
	private Text txtValidUntil;
	private Button cbOverridePermission;
	private Text txtAuthorizationCode;
	private Text txtAuthorizationFile;
	private Button btnSelectFile;
	private String machineId;

	/**
	 * Create the preference page.
	 */
	public CompileKeyPreferencePage() {
		setTitle("Chave de Compila\u00E7\u00E3o");
	}

	/**
	 * Initialize the preference page.
	 */
	@Override
	public void init(final IWorkbench workbench) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		machineId = lsService.getMachineId();
	}

	/**
	 * Create contents of the preference page.
	 *
	 * @param parent
	 */
	@Override
	public Control createContents(final Composite parent) {
		//
		final Composite container = new Composite(parent, SWT.NULL);
		container.setLayout(new GridLayout(3, false));

		final Label lblIdlocal = new Label(container, SWT.NONE);
		lblIdlocal.setText("ID Local");
		txtIdLocal = new Text(container, SWT.BORDER);
		txtIdLocal.setEnabled(false);
		txtIdLocal.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtIdLocal.setText(machineId);
		new Label(container, SWT.NONE);

		final Label lblArquivoDeAutorizao = new Label(container, SWT.NONE);
		lblArquivoDeAutorizao.setText("Arquivo de Autoriza\u00E7\u00E3o");

		txtAuthorizationFile = new Text(container, SWT.BORDER);
		txtAuthorizationFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtAuthorizationFile.setEditable(false);
		btnSelectFile = new Button(container, SWT.NONE);
		btnSelectFile.setEnabled(true);
		btnSelectFile.setText("...");
		btnSelectFile.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(final SelectionEvent e) {
				setMessage(null);
				final String[] filter = { "*.AUT" }; //$NON-NLS-1$ //$NON-NLS-2$
				final String[] filterNames = { "Chave de Compilação (*.aut)" };

				final String autfile = TDSUtil.fileDialog(new Shell(), filter, filterNames);
				if (autfile != null) {
					BusyIndicator.showWhile(Display.getCurrent(), () -> {
						final File file = new File(autfile);
						if (!file.exists()) {
							setErrorMessage("Arquivo informado não localizado ou inválido.");
						} else {
							try {
								final InputStream inStream = Files.newInputStream(file.toPath(),
										StandardOpenOption.READ);

								final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
								final ILanguageServerService lsService = serviceLocator
										.getService(ILanguageServerService.class);
								final Properties props = lsService.validKey(inStream);

								if (props.isEmpty()) {
									setErrorMessage("Arquivo de autenticação inválido.");
								} else {
									txtAuthorizationFile.setText(autfile);
									txtIdFile.setText(props.getOrDefault("ID", "").toString());
									txtGeneratedAt.setText(props.getOrDefault("GENERATION", "").toString());
									txtValidUntil.setText(props.getOrDefault("VALIDATION", "").toString());
									txtAuthorizationCode
											.setText(addBreakLine(props.getOrDefault("KEY", "").toString()));
									cbOverridePermission
											.setSelection(props.getOrDefault("PERMISSION", "0").toString().equals("1"));
								}
								if (txtAuthorizationCode.getText().isEmpty()) {
									setErrorMessage("Autorização inválida.");
								}
							} catch (final IOException e1) {
								SdkUIActivator.logStatus(ERROR, "Chave de Compilação", e1.getMessage(), e);
								setErrorMessage(e1.getMessage());
							}

						}
					});
				}
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}
		});

		final Label lblIdFile = new Label(container, SWT.NONE);
		lblIdFile.setText("ID Autoriza\u00E7\u00E3o");
		txtIdFile = new Text(container, SWT.BORDER);
		txtIdFile.setEnabled(false);
		txtIdFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		new Label(container, SWT.NONE);

		final Label lblDate = new Label(container, SWT.NONE);
		lblDate.setText("Gerado em");
		txtGeneratedAt = new Text(container, SWT.BORDER);
		txtGeneratedAt.setEnabled(false);
		txtGeneratedAt.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(container, SWT.NONE);

		final Label lblValidade = new Label(container, SWT.NONE);
		lblValidade.setText("Válido at\u00E9");
		txtValidUntil = new Text(container, SWT.BORDER);
		txtValidUntil.setEnabled(false);
		txtValidUntil.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(container, SWT.NONE);

		cbOverridePermission = new Button(container, SWT.CHECK);
		cbOverridePermission.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		cbOverridePermission.setText("Pode substituir fontes originais Microsiga");
		cbOverridePermission.setEnabled(false);

		final Label lblCodeAuth = new Label(container, SWT.NONE);
		lblCodeAuth.setText("Autoriza\u00E7\u00E3o");
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);
		txtAuthorizationCode = new Text(container, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.MULTI);
		txtAuthorizationCode.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1));
		txtAuthorizationCode.setEditable(false);

		loadData();

		return container;
	}

	private void loadData() {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		if (securePreference.nodeExists(ICompileKeyConstants.SECURE_NODE)) {
			final ISecurePreferences secureNode = securePreference.node(ICompileKeyConstants.SECURE_NODE);

			try {
				txtAuthorizationFile.setText(secureNode.get(ICompileKeyConstants.AUTHORIZATION_FILE, ""));
				txtIdFile.setText(secureNode.get(ICompileKeyConstants.ID_FILE, ""));
				txtGeneratedAt.setText(secureNode.get(ICompileKeyConstants.GENERATED_AT, ""));
				txtValidUntil.setText(secureNode.get(ICompileKeyConstants.VALID_UNTIL, ""));
				cbOverridePermission
						.setSelection(secureNode.getBoolean(ICompileKeyConstants.OVERRIDE_PERMISSION, false));
				txtAuthorizationCode.setText(addBreakLine(secureNode.get(ICompileKeyConstants.AUTHORIZATION_CODE, "")));
			} catch (final StorageException e) {
				SdkUIActivator.logStatus(ERROR, "Chave de Compilação", e.getMessage(), e);
				setErrorMessage("Não foi possível recuperar dados. Vela log para detalhes.");
			}
		} else {
			txtAuthorizationFile.setText("");
			txtIdFile.setText("");
			txtGeneratedAt.setText("");
			txtValidUntil.setText("");
			cbOverridePermission.setSelection(false);
			txtAuthorizationCode.setText("");
		}
	}

	@Override
	protected void performDefaults() {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences secureNode = securePreference.node(ICompileKeyConstants.SECURE_NODE);

		secureNode.removeNode();
		try {
			securePreference.flush();
		} catch (final IOException e) {
			SdkUIActivator.logStatus(ERROR, "Chave de Compilação", e.getMessage(), e);
		}

		loadData();
		super.performDefaults();
		setMessage(null);
	}

	@Override
	public boolean performOk() {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences secureNode = securePreference.node(ICompileKeyConstants.SECURE_NODE);
		setErrorMessage(null);

		try {
			secureNode.put(ICompileKeyConstants.AUTHORIZATION_FILE, txtAuthorizationFile.getText(), false);
			secureNode.put(ICompileKeyConstants.ID_FILE, txtIdFile.getText(), false);
			secureNode.put(ICompileKeyConstants.GENERATED_AT, txtGeneratedAt.getText(), false);
			secureNode.put(ICompileKeyConstants.VALID_UNTIL, txtValidUntil.getText(), false);
			secureNode.put(ICompileKeyConstants.AUTHORIZATION_CODE, txtAuthorizationCode.getText().replace("\n", ""),
					false);
			secureNode.put(ICompileKeyConstants.OVERRIDE_PERMISSION,
					String.valueOf(cbOverridePermission.getSelection()), false);

			secureNode.flush();
		} catch (final StorageException e) {
			SdkUIActivator.logStatus(ERROR, "Chave de Compilação", e.getMessage(), e);
			setErrorMessage("Chave não aplicada. Veja log para detalhes.");
		} catch (final IOException e) {
			SdkUIActivator.logStatus(ERROR, "Chave de Compilação", e.getMessage(), e);
			setErrorMessage("Chave não aplicada. Veja log para detalhes.");
		}

		return getErrorMessage() == null;
	}

	private String addBreakLine(final String text) {
		final StringJoiner sb = new StringJoiner("\n");
		final int len = 40;

		for (int i = 0; i < text.length(); i += len) {
			sb.add(text.substring(i, i + len));
		}

		return sb.toString();
	}

}
