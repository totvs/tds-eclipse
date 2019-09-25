package br.com.totvs.tds.ui.sdk.preference.include;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;
import br.com.totvs.tds.ui.sdk.widget.IncludeConfigurationComposite;
import br.com.totvs.tds.ui.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

/**
 * Define a PreferencePage de Includes de projetos.
 * 
 * @author Audrin
 * @author acandido
 */
public class IncludePreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	private IncludeConfigurationComposite cpProjIncludePath;

	/**
	 * Construtor.
	 */
	public IncludePreferencePage() {
		super();

		setPreferenceStore(SdkUIActivator.getDefault().getPreferenceStore());
		setDescription("Pastas de busca de arquivos de definição");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(final Composite parent) {
		String globalIncludes = getPreferenceStore().getString(ISDKPreferenceKeys.GLOBAL_INCLUDE);

		Composite container = new Composite(parent, SWT.NONE);
		container.setLayout(new FillLayout(SWT.HORIZONTAL));
		cpProjIncludePath = new IncludeConfigurationComposite(container, SWT.NONE);
		cpProjIncludePath.setGlobalVisible(false);
		cpProjIncludePath.setIncludeSelection(globalIncludes);

		return container;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performApply()
	 */
	@Override
	protected void performApply() {
		saveData();

		super.performApply();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		saveData();

		return super.performOk();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(final IWorkbench workbench) {

	}

	/**
	 * Salva configuração global.
	 */
	private void saveData() {
		String includeList = cpProjIncludePath.getIncludeSelectionAsString();

		getPreferenceStore().setValue(ISDKPreferenceKeys.GLOBAL_INCLUDE, includeList);
		WrapperManager.getInstance().setGlobalList(includeList.split(IWrapperManager.INCLUDES_SEPARATOR)); //$NON-NLS-1$
	}
}
