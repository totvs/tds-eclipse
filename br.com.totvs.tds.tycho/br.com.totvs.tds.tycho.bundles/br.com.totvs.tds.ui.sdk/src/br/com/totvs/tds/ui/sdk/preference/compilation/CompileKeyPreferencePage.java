package br.com.totvs.tds.ui.sdk.preference.compilation;

import java.util.StringJoiner;

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
import org.eclipse.wb.swt.SWTResourceManager;

import br.com.totvs.tds.server.interfaces.IAuthorizationKey;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.TDSUtil;

/**
 * Classe CompileKeyPreferencePage.
 *
 * @author leo.watanabe
 * @author matheus.sales(07/2018)
 *
 */
public class CompileKeyPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	public final static String PAGE_ID = "br.com.totvs.tds.ui.sdk.preference.compilation.compileKeyPreferencePage"; //$NON-NLS-1$

	private Text txtIdLocal;
	private Text txtIdFile;
	private Text txtGeneratedAt;
	private Text txtValidUntil;
	private Button cbOverridePermission;
	private Text txtAuthorizationCode;
	private Text txtAuthorizationFile;
	private Button btnSelectFile;

	private IAuthorizationKey authorizationKey;

	/**
	 * Create the preference page.
	 */
	public CompileKeyPreferencePage() {
		setTitle(Messages.CompileKeyPreferencePage_Compile_key);
	}

	/**
	 * Initialize the preference page.
	 */
	@Override
	public void init(final IWorkbench workbench) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final IServerManager serverManager = serviceLocator.getService(IServerManager.class);

		authorizationKey = serverManager.getAuthorizationKey();
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
		lblIdlocal.setText(Messages.CompileKeyPreferencePage_Local_id);
		txtIdLocal = new Text(container, SWT.BORDER);
		txtIdLocal.setEnabled(false);
		txtIdLocal.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtIdLocal.setText(authorizationKey.getMachineId());
		new Label(container, SWT.NONE);

		final Label lblArquivoDeAutorizao = new Label(container, SWT.NONE);
		lblArquivoDeAutorizao.setText(Messages.CompileKeyPreferencePage_Authorization_File);

		txtAuthorizationFile = new Text(container, SWT.BORDER);
		txtAuthorizationFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtAuthorizationFile.setEditable(false);
		btnSelectFile = new Button(container, SWT.NONE);
		btnSelectFile.setEnabled(true);
		btnSelectFile.setText("..."); //$NON-NLS-1$
		btnSelectFile.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(final SelectionEvent e) {
				setMessage(null);
				final String[] filter = { "*.AUT" }; //$NON-NLS-1$ //$NON-NLS-2$
				final String[] filterNames = { Messages.CompileKeyPreferencePage_Compilation_key_file };

				final String autfile = TDSUtil.fileDialog(new Shell(), filter, filterNames);
				if (autfile != null) {
					BusyIndicator.showWhile(Display.getCurrent(), () -> {
						authorizationKey.setAuthorizationFile(autfile);

						if (authorizationKey.isValid()) {
							txtAuthorizationFile.setText(autfile);
							txtIdFile.setText(authorizationKey.getIdFile());
							txtGeneratedAt.setText(authorizationKey.getGeneratedAt());
							txtValidUntil.setText(authorizationKey.getValidUntil());
							txtAuthorizationCode.setText(addBreakLine(authorizationKey.getAuthorizationCode()));
							cbOverridePermission.setSelection(authorizationKey.isOverridePermission());
						} else {
							setErrorMessage(authorizationKey.getErrorMessage());
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
		lblIdFile.setText(Messages.CompileKeyPreferencePage_Authorization_ID);
		txtIdFile = new Text(container, SWT.BORDER);
		txtIdFile.setEnabled(false);
		txtIdFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		new Label(container, SWT.NONE);

		final Label lblDate = new Label(container, SWT.NONE);
		lblDate.setText(Messages.CompileKeyPreferencePage_Generated_in);
		txtGeneratedAt = new Text(container, SWT.BORDER);
		txtGeneratedAt.setEnabled(false);
		txtGeneratedAt.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(container, SWT.NONE);

		final Label lblValidade = new Label(container, SWT.NONE);
		lblValidade.setText(Messages.CompileKeyPreferencePage_Valid_until);
		txtValidUntil = new Text(container, SWT.BORDER);
		txtValidUntil.setEnabled(false);
		txtValidUntil.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(container, SWT.NONE);

		cbOverridePermission = new Button(container, SWT.CHECK);
		cbOverridePermission.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		cbOverridePermission.setText(Messages.CompileKeyPreferencePage_Can_replace_Microsiga_fonts);
		cbOverridePermission.setEnabled(false);

		final Label lblCodeAuth = new Label(container, SWT.NONE);
		lblCodeAuth.setText(Messages.CompileKeyPreferencePage_Authorization);
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);
		txtAuthorizationCode = new Text(container, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.MULTI);
		txtAuthorizationCode.setFont(SWTResourceManager.getFont("Courier", 9, SWT.NORMAL));
		txtAuthorizationCode.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1));
		txtAuthorizationCode.setEditable(false);

		loadData();

		return container;

	}

	private void loadData() {
		txtAuthorizationFile.setText(authorizationKey.getAuthorizationFile());
		txtIdFile.setText(authorizationKey.getIdFile());
		txtGeneratedAt.setText(authorizationKey.getGeneratedAt());
		txtValidUntil.setText(authorizationKey.getValidUntil());
		cbOverridePermission.setSelection(authorizationKey.isOverridePermission());
		txtAuthorizationCode.setText(addBreakLine(authorizationKey.getAuthorizationCode()));

		setErrorMessage(authorizationKey.getErrorMessage());
	}

	@Override
	protected void performDefaults() {

		authorizationKey.reset();

		loadData();
		super.performDefaults();
		setMessage(null);
	}

	@Override
	public boolean performOk() {

		if (authorizationKey.apply()) {
			setMessage(Messages.CompileKeyPreferencePage_Compilation_key_successfully_applied, INFORMATION);
		} else {
			setErrorMessage(authorizationKey.getErrorMessage());
		}

		return getErrorMessage() == null;
	}

	private String addBreakLine(final String text) {
		final StringJoiner sb = new StringJoiner("\n"); //$NON-NLS-1$
		int i = 0;

		while (i < text.length()) {
			if ((i + 40) > text.length()) {
				sb.add(text.substring(i, text.length()));
			} else {
				sb.add(text.substring(i, i + 40));
			}

			i += 40;
		}

		return sb.toString();
	}

}
