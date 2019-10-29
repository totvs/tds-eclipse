package br.com.totvs.tds.ui.sdk.property;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;

import br.com.totvs.tds.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.sdk.wrapper.WrapperManager;
import br.com.totvs.tds.ui.sdk.widget.IncludeConfigurationComposite;

/**
 * Propriedade do Projeto - definição da lista de busca de includes.
 *
 * @author acandido
 */
public class IncludePropertyPage extends PropertyPage implements IWorkbenchPropertyPage {

	public static final String PROPERTY_PAGE_ID = "br.com.totvs.tds.sdk.ui.IncludePropertyPage"; //$NON-NLS-1$

	private IncludeConfigurationComposite includeConfiguration;

	/*
	 * Construtor.
	 */
	public IncludePropertyPage() {

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.
	 * widgets.Composite)
	 */
	@Override
	protected Control createContents(final Composite parent) {
		final Composite container = new Composite(parent, SWT.NONE);
		container.setLayout(new GridLayout(1, false));

		this.includeConfiguration = new IncludeConfigurationComposite(container, getElement());
		this.includeConfiguration.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		loadData();

		return container;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.jface.preference.PreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {

		saveData();

		return super.performOk();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.jface.preference.PreferencePage#performApply()
	 */
	@Override
	protected void performApply() {

		saveData();

		super.performApply();
	}

	/**
	 * Salva a lista de pastas para busca no projeto.
	 */
	private void saveData() {
		try {
			final IAdaptable element = getElement();
			final IProjectWrapper wrapperProject = (IProjectWrapper) WrapperManager.getInstance().getWrapper(element);
			wrapperProject.setIncludeSearchList(includeConfiguration.getIncludeSelection());
		} catch (final CoreException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Carrega a lista de pastas para busca do projeto.
	 */
	private void loadData() {
		try {
			final IAdaptable element = getElement();
			final IProjectWrapper wrapperProject = (IProjectWrapper) WrapperManager.getInstance().getWrapper(element);
			includeConfiguration.setIncludeSelection(wrapperProject.getIncludeSearchList());
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
