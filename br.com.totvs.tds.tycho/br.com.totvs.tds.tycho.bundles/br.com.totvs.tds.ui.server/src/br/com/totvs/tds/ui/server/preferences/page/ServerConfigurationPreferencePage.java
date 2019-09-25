package br.com.totvs.tds.ui.server.preferences.page;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * P�gina de prefer�ncias da configuração de servidor.
 *
 * @author eriky.kashivagui
 *
 */
public class ServerConfigurationPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	/**
	 * Informa se deve ser aberto as informações do servidor ao conectar.
	 */
	private BooleanFieldEditor chkOpenServerInfo;

	/**
	 * Informa se deve ser ativada a verificação peri�dica de conex�o com o
	 * servidor.
	 */
	private BooleanFieldEditor chkPulseServer;

	/**
	 * Verifica que os fontes que estou gerando patch estão no RPO;
	 */
	private BooleanFieldEditor chkVerifyRPO;

	/**
	 * Informa se os servidores devem ser reconectados ao abrir o TDS.
	 */
	private ComboFieldEditor cmbReconectServer;

	/**
	 * Construtor.
	 */
	public ServerConfigurationPreferencePage() {
		super(GRID);

		setPreferenceStore(ServerUIActivator.getDefault().getPreferenceStore());
		setDescription(Messages.ServerConfigurationPreferencePage_server_configurations);
	}

	@Override
	public void createFieldEditors() {
		chkOpenServerInfo = new BooleanFieldEditor(IServerConstants.OPEN_SERVER_INFORMATION,
				Messages.ServerConfigurationPreferencePage_open_server_info, getFieldEditorParent());
		chkOpenServerInfo.setPreferenceName(IServerConstants.OPEN_SERVER_INFORMATION);
		addField(chkOpenServerInfo);

		chkPulseServer = new BooleanFieldEditor(IServerConstants.PULSE_SERVER,
				Messages.ServerConfigurationPreferencePage_keep_connection, getFieldEditorParent());
		chkPulseServer.setPreferenceName(IServerConstants.PULSE_SERVER);
		addField(chkPulseServer);

		chkVerifyRPO = new BooleanFieldEditor(IServerConstants.VERIFY_RPO,
				Messages.ServerConfigurationPreferencePage_source_exist_in_rpo_build_patch, BooleanFieldEditor.DEFAULT,
				getFieldEditorParent());
		chkPulseServer.setPreferenceName(IServerConstants.VERIFY_RPO);
		addField(chkVerifyRPO);

		cmbReconectServer = new ComboFieldEditor(IServerConstants.RECONNECT_POLICIES, Messages.ServerConfigurationPreferencePage_reconnect_on_startup_tds,
				new String[][] { new String[] { Messages.ServerConfigurationPreferencePage_reconnect_on_startup_tds_none, IServerConstants.RECONNECT_NONE_SERVER },
						new String[] { Messages.ServerConfigurationPreferencePage_reconnect_on_startup_tds_selected_only, IServerConstants.RECONNECT_ONLY_SELECTED_SERVERS },
						new String[] { Messages.ServerConfigurationPreferencePage_reconnect_on_startup_tds_all, IServerConstants.RECONNECT_ALL_SERVERS }, },
				getFieldEditorParent());
		addField(cmbReconectServer);
	}

	@Override
	public void init(final IWorkbench workbench) {
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
		super.propertyChange(event);
		if (event.getProperty().equals(FieldEditor.VALUE)) {
			checkState();
			FieldEditor fieldEditor = (FieldEditor) event.getSource();
			String preferenceName = fieldEditor.getPreferenceName();
			Object newValue = event.getNewValue();
			ServerActivator.getDefault().setProperty(preferenceName, newValue);
		}
	}

}