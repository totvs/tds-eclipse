package br.com.totvs.tds.ui.debug.launcher.tab;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.debug.ui.sourcelookup.SourceLookupTab;

/**
 * Agrupa as preferencias de execução, depuração e coverage de aplicações TOTVS.
 *
 * @author acandido
 *
 */
public class TDSLaunchTabGroup extends AbstractLaunchConfigurationTabGroup {

	@Override
	public final void createTabs(final ILaunchConfigurationDialog dialog, final String mode) {
		final List<ILaunchConfigurationTab> tabList = new ArrayList<ILaunchConfigurationTab>();

		tabList.add(new SmartClientLaunchTab(mode));
		if (mode == "debug") { //$NON-NLS-1$
			tabList.add(new SourceLookupTab());
		}
		tabList.add(new CommonTab());

		final ILaunchConfigurationTab[] tabs = tabList.toArray(new ILaunchConfigurationTab[tabList.size()]);
		setTabs(tabs);
	}

}
