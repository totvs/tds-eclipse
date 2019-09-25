package br.com.totvs.tds.ui.server.tools;

import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * diálogo para identificação (login) em aplicação servidora.
 *
 * @author acandido
 */
public class ProtheusLoginDialog extends LoginDialog implements ILoginDialog {

	private Combo cmbEnvironment;
	private Label lblEnvironment;
	private Label lblPassword;

	private Label lblUsername;
	private Text txtPassword;

	private Text txtUsername;

	/**
	 * Create the dialog.
	 *
	 * @param shell
	 * @wbp.parser.constructor
	 */
	public ProtheusLoginDialog(Shell shell) {
		super(shell);
	}

	@Override
	protected Control createDataArea(final Composite container) {
		super.createDataArea(container);

		createEnvironment(container);
		createUsername(container);
		createPassword(container);

		return container;
	}

	private void createEnvironment(final Composite container) {
		lblEnvironment = new Label(container, SWT.NONE);
		lblEnvironment.setText(Messages.ProtheusLoginDialog_environment);

		getPreferenceStore().setDefault(ID_BTN_USESECURESTORAGE, false);

		cmbEnvironment = new Combo(container, SWT.NONE);
		cmbEnvironment.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbEnvironment.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
	}

	private void createPassword(final Composite container) {
		lblPassword = new Label(container, SWT.NONE);
		lblPassword.setText(Messages.ProtheusLoginDialog_password);

		txtPassword = new Text(container, SWT.BORDER | SWT.PASSWORD);
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtPassword.setText(""); //$NON-NLS-1$
		txtPassword.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
	}

	private void createUsername(final Composite container) {
		lblUsername = new Label(container, SWT.NONE);
		lblUsername.setText(Messages.ProtheusLoginDialog_user);

		txtUsername = new Text(container, SWT.BORDER);
		txtUsername.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtUsername.setText(""); //$NON-NLS-1$
		txtUsername.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.ui.server.tools.LoginDialog#doDialogChanged()
	 */
	@Override
	protected void doDialogChanged() {
		String enviroment = cmbEnvironment.getText().trim();
		String username = txtUsername.getText().trim();
		String password = txtPassword.getText().trim();

		if (enviroment.isEmpty()) {
			setErrorMessage(Messages.ProtheusLoginDialog_environment_required);
		} else if (username.isEmpty()) {
			setErrorMessage(Messages.ProtheusLoginDialog_user_required);
		} else {
			getDataMap().put(ENVIRONMENT, enviroment);
			getDataMap().put(USERNAME, username);
			getDataMap().put(PASSWORD, password);
		}
	}

	private IPreferenceStore getPreferenceStore() {
		return ServerUIActivator.getDefault().getPreferenceStore();
	}

	@Override
	protected boolean hasUseSecureStorageButton() {
		return true;
	}

	@Override
	public void initialize(final String title, final Map<String, Object> inputData) {
		this.title = title;

		this.getDataMap().clear();
		this.getDataMap().putAll(inputData);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.ui.server.tools.ILoginDialog#isValid()
	 */
	@Override
	public boolean isValid() {
		boolean result = getErrorMessage() == null;
		Map<String, Object> inputData = getDataMap();

		if (result) {
			result = ((inputData.containsKey(USERNAME) && !((String) inputData.get(USERNAME)).isEmpty())
					&& (inputData.containsKey(ENVIRONMENT) && !((String) inputData.get(ENVIRONMENT)).isEmpty()));
		}

		return result;
	}

	/**
	 * Carrega os dados nos campos.
	 */
	@Override
	protected void loadData() {
		String[] environments = (String[]) getDataMap().getOrDefault(LAST_ENVIRONMENTS, new String[0]);
		try {
			this.cmbEnvironment.setItems(environments);
		} catch (Exception e) {
			this.cmbEnvironment.setItems(new String[0]);
		}
		this.cmbEnvironment.setText((String) getDataMap().getOrDefault(ENVIRONMENT, Messages.EMPTY_STRING));
		txtUsername.setText((String) getDataMap().getOrDefault(USERNAME, Messages.EMPTY_STRING)); // $NON-NLS-1$
		txtPassword.setText((String) getDataMap().getOrDefault(PASSWORD, Messages.EMPTY_STRING));
		txtServer.setText((String) getDataMap().getOrDefault(SERVER_ADDRESS, Messages.EMPTY_STRING));

		setSaveSecure(true);
	}

}
