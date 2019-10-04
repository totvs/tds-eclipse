package br.com.totvs.tds.ui.sdk.preference;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;

/**
 * Class used to initialize default preference values.
 */
public class SDKPreferenceInitializer extends AbstractPreferenceInitializer implements ISDKPreferenceKeys {

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		final IPreferenceStore ps = SdkUIActivator.getDefault().getPreferenceStore();

		ps.setDefault(GLOBAL_INCLUDE, ""); //$NON-NLS-1$

		ps.setDefault(DESIRE_NO_COMPILE_IMAGE, true);
		ps.setDefault(DESIRE_NO_COMPILE_TAG, false);
		ps.setDefault(NO_COMPILE_TAG, "[NC]"); //$NON-NLS-1$
		ps.setDefault(LOG_LEVEL, "2"); //$NON-NLS-1$
		ps.setDefault(LOG_TO_FILE, false);
		ps.setDefault(FOLDER_LOG_FILE, "${workspace_loc}/log"); //$NON-NLS-1$
		ps.setDefault(MULT_PROCESS, "1"); //$NON-NLS-1$
		ps.setDefault(CLEAR_CONSOLE_LOG, false);

		ps.setDefault(EXCLUSION_PATTERNS, "(\\.\\w*)$,(#.*#)$,(%.*%)$,(CVS)$,(SCCS)$,(.*\\.PDB)$"); //$NON-NLS-1$

		ps.setDefault(NAME_DELRESOURCE_OPER_DEL, true);
		ps.setDefault(NAME_DELFROMRPO_OPER_DEL, false);
		ps.setDefault(NAME_SELECTEDONLY_OPER_DEL, true);
		ps.setDefault(NAME_ALLENVS_OPER_DEL, false);

		ps.setDefault(NAME_DELRESOURCE_OPER_DELRPO, false);
		ps.setDefault(NAME_DELFROMRPO_OPER_DELRPO, true);
		ps.setDefault(NAME_SELECTEDONLY_OPER_DELRPO, true);
		ps.setDefault(NAME_ALLENVS_OPER_DELRPO, false);

		ps.setDefault(NAME_RENAME, true);
		ps.setDefault(NAME_DELFROMRPO_OPER_RENAME, false);
		ps.setDefault(NAME_SELECTEDONLY_OPER_RENAME, true);
		ps.setDefault(NAME_ALLENVS_OPER_RENAME, false);
		ps.setDefault(NAME_COMPILERESOURCEWITHNEWNAME, false);
		ps.setDefault(NAME_SELECTEDONLY_OPER_COMPILE, true);
		ps.setDefault(NAME_ALLENVS_OPER_COMPILE, false);
	}

}
