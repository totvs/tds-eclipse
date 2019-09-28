package br.com.totvs.tds.ui.server.tools;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Diálogo para identificação (login).
 *
 * @author acandido
 */
public abstract class LoginDialog extends TitleAreaDialog implements ILoginDialog {
	public static final String ID_BTN_USESECURESTORAGE = LoginDialog.class.getName().concat("btnUseSecureStorage"); //$NON-NLS-1$

	private Map<String, Object> dataMap = new HashMap<String, Object>();
	private String initialMessage = null;
	private boolean saveSecure;

	protected Button btnOk;
	protected Button btnUseSecureStorage;

	protected String title;

	protected Text txtServer;

	/**
	 * Create the dialog.
	 *
	 * @param shell
	 * @wbp.parser.constructor
	 */
	public LoginDialog(Shell shell) {
		super(shell);
	}

	@Override
	protected void configureShell(Shell newShell) {
		newShell.setText(Messages.LoginDialog_identify_yourself);

		super.configureShell(newShell);
	}

	/**
	 * Create contents of the button bar.
	 *
	 * @param parent
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		btnOk = createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/**
	 * Area de dados adicionais.
	 *
	 * @param parent
	 * @return
	 */
	protected Control createDataArea(Composite parent) {
		createServer(parent);

		return parent;
	};

	/**
	 * Create contents of the dialog.
	 *
	 * @param parent
	 */
	@Override
	protected final Control createDialogArea(Composite parent) {
		setTitle(title);
		setTitleImage(ServerUIIcons.getUser64().createImage());

		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayout(new GridLayout(2, false));
		container.setLayoutData(new GridData(GridData.FILL_BOTH));

		createDataArea(container);

		if (hasUseSecureStorageButton()) {
			new Label(container, SWT.NONE);
			btnUseSecureStorage = new Button(container, SWT.CHECK);
			btnUseSecureStorage.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, true, 1, 1));
			btnUseSecureStorage.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					dialogChanged();
				}
			});
			btnUseSecureStorage.setText(Messages.LoginDialog_save_Data_in_secure_location);
			btnUseSecureStorage.setSelection(isSafeSave());
		}

		area.pack(true);

		loadData();

		if ((initialMessage != null) && (!initialMessage.isEmpty())) {
			setErrorMessage(initialMessage);
		}

		return area;
	}

	private void createServer(final Composite container) {
		Label lblServer = new Label(container, SWT.NONE);
		lblServer.setText("Servidor");

		txtServer = new Text(container, SWT.BORDER);
		txtServer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtServer.setText(""); //$NON-NLS-1$
		txtServer.setEnabled(false);

	}

	/**
	 * Responde as modificações efetuadas no formul�rio.
	 */
	final protected void dialogChanged() {
		setErrorMessage(null);

		boolean selection = btnUseSecureStorage.getSelection();
		dataMap.put(USE_SECURE_STORAGE, selection);

		doDialogChanged();

		if (btnOk != null) {
			btnOk.setEnabled(getErrorMessage() == null);
		}
	}

	/**
	 * <p>
	 * Utilize este m�todo para validar e salvar os dados customizados, conforme
	 * exemplo.
	 * </p>
	 *
	 * <pre>
	 * getDataMap().put("username", txtUsername.getText();
	 * // add as necessary
	 * </pre>
	 */
	abstract protected void doDialogChanged();

	@Override
	public Map<String, Object> getDataMap() {
		return dataMap;
	}

	/**
	 * @return the initialMessage
	 */
	@Override
	public String getInitialMessage() {
		return initialMessage;
	}

	/**
	 * Return the initial size of the dialog.
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(450, 300);
	}

	/**
	 * @return indicação de uso ou não da opção de salva segura.
	 */
	abstract protected boolean hasUseSecureStorageButton();

	@Override
	public void initialize(final String title, final Map<String, Object> inputData) {
		this.title = title;

		this.getDataMap().clear();
		this.getDataMap().putAll(inputData);

	}

	@Override
	public boolean isSafeSave() {
		return saveSecure;
	}

	/**
	 * Inicializa os valores dos campos do diálogo
	 */
	abstract protected void loadData();

	/**
	 * @param initialMessage the initialMessage to set
	 */
	@Override
	public void setInitialMessage(String initialMessage) {
		this.initialMessage = initialMessage;
	}

	/**
	 *
	 * @param isSaveSecure
	 */
	public void setSaveSecure(boolean isSaveSecure) {
		this.saveSecure = isSaveSecure;
	}

}
