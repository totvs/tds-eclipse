/**
 * 
 */
package br.com.totvs.tds.ui;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * @author acandido
 *
 */
public class TDSPreferenceInitializer extends AbstractPreferenceInitializer implements ITDSPreferenceKeys {

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = TDSUIActivator.getDefault().getPreferenceStore();

		// Pessoal
		store.setDefault(USER_BIRTH_DAY, 0);
		store.setDefault(USER_BIRTH_MONTH, 0);
	}

}
