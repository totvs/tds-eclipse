package br.com.totvs.tds.ui.sdk.advpl;

import java.util.Date;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * Classe CompileKeyPreferencePage.
 * 
 * @author leo.watanabe
 * @author matheus.sales(07/2018)
 *
 */
public class CompileKeyPreferencePage extends PreferencePage
		implements IWorkbenchPreferencePage, IPropertyChangeListener {

	// private AuthKey localAuthKey = new
	// AuthKey(TotvsLicense.getInstance().getAuthKey());

	private Text txtIdLocal;
	private Text txtIdFile;
	private Text txtGeneration;
	private Text txtValidity;
	private Button checkButtonPermission;
	private Text txtCodeAuth;
	private Text txtFile;
	private Button btnSelectFile;

	/**
	 * Create the preference page.
	 */
	public CompileKeyPreferencePage() {
	}

	/**
	 * Initialize the preference page.
	 */
	public void init(final IWorkbench workbench) {
	}

	/**
	 * Create contents of the preference page.
	 *
	 * @param parent
	 */
	@Override
	public Control createContents(final Composite parent) {
		MouseListener dateMouseListener = new MouseAdapter() {
			private String oldValue = ""; //$NON-NLS-1$

			@Override
			public void mouseDown(final MouseEvent e) {
				Text source = (Text) e.getSource();
				oldValue = source.getText();
			}

			@Override
			public void mouseUp(final MouseEvent e) {
				Text source = (Text) e.getSource();
				String value = source.getText();
				if (!value.matches("([0-9]{1,2})(/[0-9]{0,2}(/[0-9]{0,4})?)?")) { //$NON-NLS-1$
					source.setText(oldValue);
				}
				valueChanged();
			}
		};

		KeyListener idKeyListener = new KeyAdapter() {
			@Override
			public void keyReleased(final KeyEvent e) {
				valueChanged();
			}

			@Override
			public void keyPressed(final KeyEvent e) {
				Text source = (Text) e.getSource();
				String value = source.getText();
				char ch = e.character;
				if (ch == SWT.BS || ch == SWT.DEL || ch == SWT.CR || ch == 0) {
					e.doit = true;
				} else if ((value.length() > 9) || (!Character.valueOf(ch).toString().matches("[a-zA-Z0-9\\-]")) //$NON-NLS-1$
						|| (!(value + ch).matches("([a-zA-Z0-9]{1,4})(\\-[a-zA-Z0-9]{0,4})?"))) { //$NON-NLS-1$
					e.doit = false;
				} else {
					e.doit = true;
					valueChanged();
				}
			}
		};

		KeyListener dateKeyListener = new KeyAdapter() {
			@Override
			public void keyReleased(final KeyEvent e) {
				valueChanged();
			}

			@Override
			public void keyPressed(final KeyEvent e) {
				Text source = (Text) e.getSource();
				String value = source.getText();
				char ch = e.character;
				if (ch == SWT.BS || ch == SWT.DEL || ch == SWT.CR || ch == 0) {
					e.doit = true;
				} else if ((value.length() > 10) || (!Character.valueOf(ch).toString().matches("[0-9/]")) //$NON-NLS-1$
						|| (!(value + ch).matches("([0-9]{1,2})(/[0-9]{0,2}(/[0-9]{0,4})?)?"))) { //$NON-NLS-1$
					e.doit = false;
				} else {
					e.doit = true;
					valueChanged();
				}
			}
		};

		FocusListener listener = new FocusAdapter() {
			@Override
			public void focusLost(final FocusEvent e) {
				valueChanged();
			}
		};

		//
		Composite container = new Composite(parent, SWT.NULL);
		container.setLayout(new FillLayout(SWT.HORIZONTAL));

		TabFolder folderIdentidade = new TabFolder(container, SWT.NONE);

		TabItem tabChave = new TabItem(folderIdentidade, SWT.NONE);
		tabChave.setText("Chave de Compilação");

		Composite compositeChave = new Composite(folderIdentidade, SWT.NONE);
		tabChave.setControl(compositeChave);
		compositeChave.setLayout(new GridLayout(1, false));

		Group groupLocal = new Group(compositeChave, SWT.SHADOW_ETCHED_IN);
		groupLocal.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		groupLocal.setText("Diret�rio local");
		groupLocal.setLayout(new GridLayout(3, false));

		Label lblIdlocal = new Label(groupLocal, SWT.NONE);
		lblIdlocal.setText("ID:"); //$NON-NLS-1$
		txtIdLocal = new Text(groupLocal, SWT.BORDER);
		txtIdLocal.setEditable(false);
		new Label(groupLocal, SWT.NONE);
		new Label(compositeChave, SWT.NONE);

		Group groupAuthKey = new Group(compositeChave, SWT.SHADOW_ETCHED_IN);
		groupAuthKey.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		groupAuthKey.setText("Chave atual");
		groupAuthKey.setLayout(new GridLayout(3, false));

		Label lblIdFile = new Label(groupAuthKey, SWT.NONE);
		lblIdFile.setText("ID:"); //$NON-NLS-1$
		txtIdFile = new Text(groupAuthKey, SWT.BORDER);
		new Label(groupAuthKey, SWT.NONE);

		Label lblDate = new Label(groupAuthKey, SWT.NONE);
		lblDate.setText("Data de Geração:");
		txtGeneration = new Text(groupAuthKey, SWT.BORDER);
		new Label(groupAuthKey, SWT.NONE);

		Label lblValidade = new Label(groupAuthKey, SWT.NONE);
		lblValidade.setText("Validade:");
		txtValidity = new Text(groupAuthKey, SWT.BORDER);
		new Label(groupAuthKey, SWT.NONE);

		checkButtonPermission = new Button(groupAuthKey, SWT.CHECK);
		checkButtonPermission.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		checkButtonPermission.setText("Pes�oss�o para substituição de fontes originais Microsiga");

		Label lblCodeAuth = new Label(groupAuthKey, SWT.NONE);
		lblCodeAuth.setText("C�digo de Autorização:");
		txtCodeAuth = new Text(groupAuthKey, SWT.BORDER);
		// gdTxtCodeAuth.widthHint = 193;
		txtCodeAuth.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));

		txtIdFile.addFocusListener(listener);
		txtIdFile.addKeyListener(idKeyListener);
		txtGeneration.addFocusListener(listener);
		txtGeneration.addKeyListener(dateKeyListener);
		txtGeneration.addMouseListener(dateMouseListener);
		txtValidity.addFocusListener(listener);
		txtValidity.addKeyListener(dateKeyListener);
		txtValidity.addMouseListener(dateMouseListener);
		checkButtonPermission.addFocusListener(listener);
		txtCodeAuth.addFocusListener(listener);
		new Label(compositeChave, SWT.NONE);

		Group groupImportar = new Group(compositeChave, SWT.SHADOW_ETCHED_IN);
		groupImportar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		groupImportar.setText("Importar Chave de Compilação");
		groupImportar.setLayout(new GridLayout(2, false));

		txtFile = new Text(groupImportar, SWT.BORDER);
		txtFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtFile.setEditable(false);
		btnSelectFile = new Button(groupImportar, SWT.NONE);
		btnSelectFile.setEnabled(true);
		btnSelectFile.setText("Importar...");
		btnSelectFile.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(final SelectionEvent e) {
				FileDialog dialog = new FileDialog(new Shell());
				BusyIndicator.showWhile(getShell().getDisplay(), new Runnable() {

					@Override
					public void run() {
						String[] filter = { "*.AUT" }; //$NON-NLS-1$ //$NON-NLS-2$
						dialog.setFilterExtensions(filter);
						String[] filterNames = { "Chave de Compilação (*.aut)" };
						dialog.setFilterNames(filterNames);

//						dialog.setRememberKey("compileKeyPreferences");
						String path = dialog.open();

						if (path != null) {
							BusyIndicator.showWhile(Display.getCurrent(), new Runnable() {

								@Override
								public void run() {
									System.out.println(
											"CompileKeyPreferencePage.createContents(...).new SelectionListener() {...}.widgetSelected(...).new Runnable() {...}.run().new Runnable() {...}.run()");
//									try {
////										localAuthKey.loadAuthKeyFile(path);
//										setFieldsValues();
//									} catch (IOException | ParseException ex) {
//										ex.printStackTrace();
//									}
								}
							});
						}
					}
				});
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}
		});

		validInputs();

		return container;
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		// limpa a chave de autorizacao
		// localAuthKey = new AuthKey();

		setMessage(null);
		// TotvsLicense.getInstance().replaceLicenseAuthKey(localAuthKey);
	}

	@Override
	public boolean performOk() {
		return applyAutorization();
	}

//	private AuthKey readFields() {
//		AuthKey key = null;
//		String id = txtIdFile.getText();
//		Date generationDate = createDateType(txtGeneration.getText());
//		Date validityDate = createDateType(txtValidity.getText());
//		boolean selection = checkButtonPermission.getSelection();
//		String codeAuth = txtCodeAuth.getText();
//		String fileName = txtFile.getText();
//
//		if (!id.isEmpty() && generationDate != null && validityDate != null && !codeAuth.isEmpty()) {
//			key = new AuthKey(id, generationDate, validityDate, selection, codeAuth, fileName);
//		}
//
//		return key;
//	}

	private void valueChanged() {
		String msg = null;

		if (txtIdFile.getText().isEmpty() || txtIdFile.getText().equals("0000-0000")) { //$NON-NLS-1$
			msg = "Informe o ID.";
		} else if (!txtIdFile.getText().equals("XXXXXXX")) { // TotvsLicense.getInstance().getIdLocal())) {
			msg = "ID inv�lido.";
		} else if (txtGeneration.getText().isEmpty() || txtGeneration.getText().length() < 10
				|| createDateType(txtGeneration.getText()) == null) {
			msg = "Informe uma data de geração v�lida (DD/MM/YYYY).";
			// txtGeneration.setText("");
		} else if (txtValidity.getText().isEmpty() || txtValidity.getText().length() < 10
				|| createDateType(txtValidity.getText()) == null) {
			msg = "Informe uma data v�lida (DD/MM/YYYY).";
			// txtValidity.setText("");
		} else if (txtCodeAuth.getText().isEmpty()) {
			msg = "Informe o c�digo de autorização.";
		} else {
			System.out.println("CompileKeyPreferencePage.valueChanged()");
//			AuthKey authKey = readFields();
//			if (authKey != null && !authKey.equals(localAuthKey)) {
//				boolean isValidKey = isValidKey(authKey);
//				if (!isValidKey) {
//					msg = "Chave de compilação inv�lida.";
//				} else {
//					localAuthKey = authKey;
//				}
//			}
		}
		//
		setValid(msg == null);
		setMessage(msg, ERROR);
	}

	private void validInputs() {
		setMessage(null);
	}

	private Date createDateType(final String dateText) {
		Date date = null;
		if (dateText != null && !dateText.trim().isEmpty()) {
//			try {
//				date = AuthKey.AUTH_SDF.parse(dateText);
//			} catch (ParseException e) {
//			}
		}
		return date;
	}

	private boolean applyAutorization() {
		boolean applyAutorization = false;
		//
//		AuthKey authKey = readFields();
//		if (authKey != null) {
//			if (isValidKey(authKey)) {
//				TotvsLicense.getInstance().replaceLicenseAuthKey(authKey);
//				applyAutorization = true;
//			} else {
//				UiUtils.openMessageDialog(new Shell(), "Chave de compilação não aplicada",
//						"Chave de Compilação inv�lida. Remova-a ou altere para uma chave v�lida.", MessageDialog.ERROR, //$NON-NLS-1$
//						new String[] { "OK" });
//			}
//		} else {
//			TotvsLicense.getInstance().clearAuthKey();
//			applyAutorization = true;
//		}

		return applyAutorization;
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
	}

}
