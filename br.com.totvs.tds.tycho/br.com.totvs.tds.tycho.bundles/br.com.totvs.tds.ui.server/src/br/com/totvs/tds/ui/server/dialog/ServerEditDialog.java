package br.com.totvs.tds.ui.server.dialog;

import java.net.URI;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wb.swt.ResourceManager;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.internal.ServerUtils;
import br.com.totvs.tds.ui.server.nl.Messages;

public class ServerEditDialog extends EditTitleAreaDialog {

	private IAppServerInfo serverInfo;

	private Text txtParentName;
	private Text txtSmartClientPath;
	private Text txtServerName;
	private Text txtAddress;
	private Text txtPort;
	private Text txtVersion;
	private Text txtAppServerPath;

	private Button btnLocalServer;

	/**
	 * Create the dialog.
	 *
	 * @param parentShell
	 */
	public ServerEditDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	public void create() {
		super.create();

		setTitle(String.format(Messages.ServerEditDialog_Server_name, serverInfo.getName()));
		setMessage(Messages.ServerEditDialog_Attributes_edit, IMessageProvider.INFORMATION);

		updateInput();
	}

	/**
	 * Create contents of the dialog.
	 *
	 * @param parent
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		setTitleImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/server_dialog.png")); //$NON-NLS-1$ //$NON-NLS-2$

		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayout(new GridLayout(3, false));
		container.setLayoutData(new GridData(GridData.FILL_BOTH));

		ModifyListener modifyListener = new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				validadeInput();
			}
		};

		ModifyListener modifySmartClientListener = new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				if (txtAddress.getText().isEmpty()) {
					String text = ((Text) e.widget).getText();
					String[] result = ServerUtils.doProcessSmartClientIni(text);
					txtAddress.setText(result[0]);
					txtPort.setText(result[1]);
				}

				validadeInput();
			}
		};

		ModifyListener modifyAddressListener = new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				txtVersion.setText(""); //$NON-NLS-1$
				validadeInput();
			}
		};

		Label label = new Label(container, SWT.NONE);
		label.setText(Messages.ServerEditDialog_Target);

		txtParentName = new Text(container, SWT.BORDER);
		txtParentName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		txtParentName.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtParentName.setEnabled(false);
		txtParentName.setEditable(false);
		new Label(container, SWT.NONE);

		Label label_1 = new Label(container, SWT.NONE);
		label_1.setText(Messages.ServerEditDialog_SmartClient);

		txtSmartClientPath = new Text(container, SWT.BORDER);
		txtSmartClientPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtSmartClientPath.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtSmartClientPath.addModifyListener(modifySmartClientListener);

		Button button = new Button(container, SWT.NONE);
		button.setText("..."); //$NON-NLS-1$

		Label label_2 = new Label(container, SWT.NONE);
		label_2.setText(Messages.ServerEditDialog_Name);

		txtServerName = new Text(container, SWT.BORDER);
		txtServerName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		txtServerName.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtServerName.addModifyListener(modifyListener);

		new Label(container, SWT.NONE);

		Label label_3 = new Label(container, SWT.NONE);
		label_3.setText(Messages.ServerEditDialog_Address);

		Composite composite = new Composite(container, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_composite = new GridLayout(3, false);
		gl_composite.marginWidth = 0;
		gl_composite.marginHeight = 0;
		composite.setLayout(gl_composite);

		txtAddress = new Text(composite, SWT.BORDER);
		txtAddress.setToolTipText(Messages.ServerEditDialog_Address_hint);
		txtAddress.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtAddress.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtAddress.addModifyListener(modifyAddressListener);

		Label label_4 = new Label(composite, SWT.NONE);
		label_4.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		label_4.setText(Messages.ServerEditDialog_Port);

		txtPort = new Text(composite, SWT.BORDER);
		txtPort.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtPort.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtPort.addModifyListener(modifyListener);
		new Label(container, SWT.NONE);

		Label label_5 = new Label(container, SWT.NONE);
		label_5.setText(Messages.ServerEditDialog_Version);

		txtVersion = new Text(container, SWT.BORDER | SWT.READ_ONLY);
		txtVersion.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		txtVersion.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtVersion.setEnabled(false);

		Button button_1 = new Button(container, SWT.NONE);
		button_1.setText(Messages.ServerEditDialog_Validate);
		button_1.setEnabled(false);
		new Label(container, SWT.NONE);

		btnLocalServer = new Button(container, SWT.CHECK);
		btnLocalServer.setText(Messages.ServerEditDialog_Local_server);
		new Label(container, SWT.NONE);

		Label lblAppserver = new Label(container, SWT.NONE);
		lblAppserver.setText(Messages.ServerEditDialog_AppServer);
		lblAppserver.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));

		txtAppServerPath = new Text(container, SWT.BORDER);
		txtAppServerPath.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		txtAppServerPath.setText(Messages.ServerEditDialog_EMPTY_STRING);
		txtAppServerPath.addModifyListener(modifyListener);

		return area;
	}

	@Override
	protected void doValidadeInput() {
		// TODO Auto-generated method stub
		getButton(IDialogConstants.OK_ID).setEnabled(false);
	}

	@Override
	public IItemInfo getItemInfo() {

		return serverInfo;
	}

	@Override
	protected void saveInput() {
		serverInfo.setName(txtServerName.getText());
		URI uri = (URI.create("//" + txtAddress.getText() + ":" + txtPort.getText())); //$NON-NLS-1$ //$NON-NLS-2$
		serverInfo.setAddress(uri);
		serverInfo.setSmartClientPath(txtSmartClientPath.getText());
		serverInfo.setVersion(txtVersion.getText());
		if (btnLocalServer.getSelection()) {
			serverInfo.setAppServerPath(txtAppServerPath.getText());
		} else {
			serverInfo.setAppServerPath(Messages.ServerEditDialog_EMPTY_STRING);
		}
	}

	@Override
	public void setItemInfo(IItemInfo itemInfo) {
		serverInfo = (IAppServerInfo) itemInfo;
	}

	@Override
	protected void updateInput() {
		txtParentName.setText(serverInfo.getParent().getName());
		txtSmartClientPath.setText(serverInfo.getSmartClientPath());
		txtServerName.setText(serverInfo.getName());
		txtAddress.setText(serverInfo.getAddress().getHost());
		txtPort.setText(String.valueOf(serverInfo.getAppServerPort()));
		txtVersion.setText(serverInfo.getVersion());
		txtAppServerPath.setText(serverInfo.getAppServerPath());
		btnLocalServer.setEnabled(serverInfo.isAppServerLocal());
	}
}
