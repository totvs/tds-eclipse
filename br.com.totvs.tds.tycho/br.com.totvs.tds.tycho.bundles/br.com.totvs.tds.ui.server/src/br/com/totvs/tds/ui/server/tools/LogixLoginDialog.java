package br.com.totvs.tds.ui.server.tools;

import java.util.Map;

import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * diálogo para identificação (login) em aplicação servidora.
 * 
 * @author acandido
 */
public class LogixLoginDialog extends LoginDialog implements ILoginDialog {

	public static final String ID_CMB_ENVIRONMENT = LogixLoginDialog.class.getName().concat("cmbEnvironment"); //$NON-NLS-1$

	private Combo cmbEnvironment;

	public LogixLoginDialog(final Shell parent) {
		super(parent);
	}

	@Override
	public void initialize(final String title, final Map<String, Object> inputData) {
		this.title = title;

		this.getDataMap().clear();
		this.getDataMap().putAll(inputData);
	}

	@Override
	public boolean isValid() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void loadData() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected Control createDataArea(Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void doDialogChanged() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected boolean hasUseSecureStorageButton() {
		// TODO Auto-generated method stub
		return false;
	}

//	/*
//	 * (non-Javadoc)
//	 * 
//	 * @see br.com.totvs.tds.ui.server.tools.LoginDialog#createDataArea(org.eclipse .swt.widgets.Composite)
//	 */
//	@Override
//	protected Control createDataArea(final Composite container) {
//		Label lblAmbiente = new Label(container, SWT.NONE);
//		lblAmbiente.setText(Messages.LogixLoginDialog_1);
//
//		cmbEnvironment = new Combo(container, SWT.NONE);
//		cmbEnvironment.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
//		cmbEnvironment.setItems((String[]) getDataMap().getOrDefault(LAST_ENVIRONMENTS, new String[0]));
//
//		cmbEnvironment.addModifyListener(new ModifyListener() {
//			@Override
//			public void modifyText(final ModifyEvent e) {
//				doDialogChanged();
//			}
//		});
//
//		if (dataMap != null) {
//			String environmentName = (String) dataMap.get("environment"); //$NON-NLS-1$
//
//			if (environmentName != null && !environmentName.isEmpty()) {
//				cmbEnvironment.setText(environmentName);
//				dataMap.put(ENVIRONMENT, environmentName);
//			}
//
//			String[] environments = (String[]) dataMap.get(LAST_ENVIRONMENTS);
//			if (environments != null) {
//				for (String environment : environments) {
//					cmbEnvironment.add(environment);
//				}
//			}
//		}
//
//		container.pack(true);
//		return container;
//	}
//
//	/*
//	 * (non-Javadoc)
//	 * 
//	 * @see br.com.totvs.tds.ui.server.tools.LoginDialog#doDialogChanged()
//	 */
//	@Override
//	protected void doDialogChanged() {
//		dataMap.put(ENVIRONMENT, cmbEnvironment.getText());
//		dataMap.put(LAST_ENVIRONMENTS, dataMap.get(LAST_ENVIRONMENTS));
//		isValid();
//	}
//
//	/*
//	 * (non-Javadoc)
//	 * 
//	 * @see br.com.totvs.tds.ui.server.tools.ILoginDialog#isValid()
//	 */
//	@Override
//	public boolean isValid() {
//		boolean is = (dataMap.containsKey(ENVIRONMENT) && !((String)dataMap.get(ENVIRONMENT)).isEmpty());
//		if (btnOk != null) {
//			btnOk.setEnabled(is);
//		}
//		return is;
//	}
//
//	@Override
//	protected boolean hasUseSecureStorageButton() {
//		return false;
//	}

}
