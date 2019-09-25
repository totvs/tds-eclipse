package br.com.totvs.tds.ui.sdk.preference.workarea;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import br.com.totvs.tds.ui.TDSUIActivator;
import br.com.totvs.tds.ui.sdk.SdkUIIcons;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

public class WorkareaPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	private List<Button> buttonsList = new ArrayList<Button>();

	/**
	 * @wbp.parser.constructor
	 */
	public WorkareaPreferencePage() {
		super();
		setImageDescriptor(SdkUIIcons.getLogoTotvs());
		setPreferenceStore(TDSUIActivator.getDefault().getPreferenceStore());
	}

	public WorkareaPreferencePage(final String title) {
		super(title);
		setImageDescriptor(SdkUIIcons.getLogoTotvs());
		setPreferenceStore(TDSUIActivator.getDefault().getPreferenceStore());
	}

	public WorkareaPreferencePage(final String title, final ImageDescriptor image) {
		super(title, image);
		setPreferenceStore(TDSUIActivator.getDefault().getPreferenceStore());
	}

	@Override
	public void init(final IWorkbench workbench) {
		// Esse m�todo não � usado pois ele � chamado antes de criar o conte�do da tela
		// (m�todo createContents)
	}

	@Override
	protected Control createContents(final Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		container.setLayout(new GridLayout(1, false));
		createDefaultDeleteOptions(container);
		createDeleteRPOOptions(container);
		createRenameOptions(container);
		initialize();
		return container;
	}

	private void createDefaultDeleteOptions(final Composite container) {
		Group group = createGroup(container, "Apagar recurso");
		createDeleteResourceButton(group, ISDKPreferenceKeys.NAME_DELRESOURCE_OPER_DEL);
		createDeleteFromRPOButton(group, ISDKPreferenceKeys.NAME_DELFROMRPO_OPER_DEL);
		createSelectedOnlyButton(group, ISDKPreferenceKeys.NAME_SELECTEDONLY_OPER_DEL);
		createAllEnvironmentsButton(group, ISDKPreferenceKeys.NAME_ALLENVS_OPER_DEL);
	}

	private void createDeleteRPOOptions(final Composite container) {
		Group group = createGroup(container, "Mudan�as a serem feitas para operação: Excluir do RPO");
		createDeleteResourceButton(group, ISDKPreferenceKeys.NAME_DELRESOURCE_OPER_DELRPO);
		createDeleteFromRPOButton(group, ISDKPreferenceKeys.NAME_DELFROMRPO_OPER_DELRPO);
		createSelectedOnlyButton(group, ISDKPreferenceKeys.NAME_SELECTEDONLY_OPER_DELRPO);
		createAllEnvironmentsButton(group, ISDKPreferenceKeys.NAME_ALLENVS_OPER_DELRPO);
	}

	private void createRenameOptions(final Composite container) {
		Group group = createGroup(container, "Mudan�as a serem feitas para operação: Renomear");
		createCheckBox(group, "Remoção do recurso com nome antigo do RPO",
				ISDKPreferenceKeys.NAME_DELFROMRPO_OPER_RENAME);
		createSelectedOnlyButton(group, ISDKPreferenceKeys.NAME_SELECTEDONLY_OPER_RENAME);
		createAllEnvironmentsButton(group, ISDKPreferenceKeys.NAME_ALLENVS_OPER_RENAME);
		Composite comp = new Composite(group, SWT.NONE);
		comp.setLayout(new GridLayout(1, false));
		comp.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		createCheckBox(comp, "Compilar recurso com novo nome", ISDKPreferenceKeys.NAME_COMPILERESOURCEWITHNEWNAME);
		createSelectedOnlyButton(comp, ISDKPreferenceKeys.NAME_SELECTEDONLY_OPER_COMPILE);
		createAllEnvironmentsButton(comp, ISDKPreferenceKeys.NAME_ALLENVS_OPER_COMPILE);
	}

	private Group createGroup(final Composite parent, final String text) {
		Group group = new Group(parent, SWT.SHADOW_OUT);
		group.setText(text);
		group.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		group.setLayout(new GridLayout(1, false));
		return group;
	}

	private Button createDeleteResourceButton(final Composite parent, final String name) {
		String text = "Apagar recurso";
		return createCheckBox(parent, text, name);
	}

	private Button createDeleteFromRPOButton(final Composite parent, final String name) {
		String text = "Remoção de objetos do RPO";
		;
		return createCheckBox(parent, text, name);
	}

	private Button createSelectedOnlyButton(final Composite parent, final String name) {
		String text = "Somente os ambientes selecionados";
		return createRadioBox(parent, text, name);
	}

	private Button createAllEnvironmentsButton(final Composite parent, final String name) {
		String text = "Todos os ambientes";
		return createRadioBox(parent, text, name);
	}

	private Button createRadioBox(final Composite parent, final String text, final String name) {
		Button radioBox = new Button(parent, SWT.RADIO);
		GridData gd_allEnvs1 = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_allEnvs1.horizontalIndent = 43;
		radioBox.setLayoutData(gd_allEnvs1);
		radioBox.setText(text);
		radioBox.setData("NAME", name); //$NON-NLS-1$
		buttonsList.add(radioBox);
		return radioBox;
	}

	private Button createCheckBox(final Composite parent, final String text, final String name) {
		Button checkBox = new Button(parent, SWT.CHECK);
		GridData gd_delRes1 = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_delRes1.horizontalIndent = 20;
		checkBox.setLayoutData(gd_delRes1);
		checkBox.setText(text);
		checkBox.setData("NAME", name); //$NON-NLS-1$
		buttonsList.add(checkBox);
		return checkBox;
	}

	private void initialize() {
		for (Button button : buttonsList) {
			String name = (String) button.getData("NAME"); //$NON-NLS-1$
			boolean selection = getPreferenceStore().getBoolean(name);
			button.setSelection(selection);
		}
	}

	@Override
	public boolean performOk() {
		for (Button button : buttonsList) {
			getPreferenceStore().setValue((String) button.getData("NAME"), button.getSelection()); //$NON-NLS-1$
		}
		return super.performOk();
	}

	@Override
	protected void performDefaults() {
		for (Button button : buttonsList) {
			String name = (String) button.getData("NAME"); //$NON-NLS-1$
			boolean defaultBoolean = getPreferenceStore().getDefaultBoolean(name);
			button.setSelection(defaultBoolean);
		}
		super.performDefaults();
	}

}
