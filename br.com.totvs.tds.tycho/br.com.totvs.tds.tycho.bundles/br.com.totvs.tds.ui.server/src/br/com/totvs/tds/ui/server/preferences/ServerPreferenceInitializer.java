package br.com.totvs.tds.ui.server.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Inicializador de prefer�ncias do plugin de servidores.
 *
 * @author eriky.kashivagui
 *
 */
public class ServerPreferenceInitializer extends AbstractPreferenceInitializer {

	/**
	 * Construtor.
	 */
	public ServerPreferenceInitializer() {

	}

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = ServerUIActivator.getDefault().getPreferenceStore();

		store.setDefault(IServerConstants.SHOW_ADVPLASP_CONTENT_PROGRESSIVELY, false);
		store.setDefault(IServerConstants.GENERATE_PPO_FILE_SERVER, false);
		store.setDefault(IServerConstants.INCLUDE_DEBUG_INFORAMATION, false);
		store.setDefault(IServerConstants.SHOW_PRECOMPILATION_RESULTS, false);
		store.setDefault(IServerConstants.REPOSITORY_OPTIMIZATION, IServerConstants.REPOSITORY_OPTMIZATION_FOR_SPEED);
		store.setDefault(IServerConstants.OPEN_SERVER_INFORMATION, false);
		store.setDefault(IServerConstants.PULSE_SERVER, false);
		store.setDefault(IServerConstants.VERIFY_RPO, true);
		store.setDefault(IServerConstants.RECONNECT_POLICIES, IServerConstants.RECONNECT_ONLY_SELECTED_SERVERS);
	}

}
