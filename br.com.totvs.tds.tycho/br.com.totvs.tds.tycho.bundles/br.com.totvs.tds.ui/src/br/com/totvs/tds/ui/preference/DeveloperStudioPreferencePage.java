package br.com.totvs.tds.ui.preference;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PreferencesUtil;

import br.com.totvs.tds.ui.ITDSPreferenceKeys;
import br.com.totvs.tds.ui.TDSUIActivator;
import br.com.totvs.tds.ui.nl.Messages;

/**
 * Prefer�ncias do Developer Studio.
 *
 * @author acandido
 *
 */
public class DeveloperStudioPreferencePage extends PreferencePage
		implements IWorkbenchPreferencePage, IWorkbenchPropertyPage {

	public static final String TDS = "TOTVS Developer Studio"; //$NON-NLS-1$

	private Spinner spnMonth;
	private Spinner spnDay;

	/**
	 * Construtor.
	 */
	public DeveloperStudioPreferencePage() {
	}

	/**
	 * Construtor.
	 *
	 * @param title T�tulo da página.
	 * @wbp.parser.constructor
	 * @wbp.eval.method.parameter title "TOTVS Developer Studio"
	 */
	public DeveloperStudioPreferencePage(final String title) {
		super(title);

		setDescription(Messages.DeveloperStudioPreferencePage_tds_preferences_description_page);
		setTitle(TDS);
	}

	/**
	 * Construtor.
	 *
	 * @param title T�tulo da página.
	 * @param image �cone.
	 */
	public DeveloperStudioPreferencePage(final String title, final ImageDescriptor image) {
		super(title, image);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(final IWorkbench workbench) {
		setPreferenceStore(TDSUIActivator.getDefault().getPreferenceStore());
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
		final Composite top = new Composite(parent, SWT.LEFT);
		top.setLayout(new GridLayout(3, false));

		final Link link = new Link(top, SWT.NONE);
		link.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		link.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(final MouseEvent e) {
				final PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(getShell(),
						"org.eclipse.ui.font_preference_page_context", //$NON-NLS-1$
						new String[] { "org.eclipse.ui.font_preference_page_context" }, null); //$NON-NLS-1$
				dialog.open();
			}
		});
		link.setText(Messages.DeveloperStudioPreferencePage_link_text);
		new Label(top, SWT.NONE);
		new Label(top, SWT.NONE);

		final Label lblPessoalusoEstritamente = new Label(top, SWT.NONE);
		lblPessoalusoEstritamente.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		lblPessoalusoEstritamente.setText(Messages.DeveloperStudioPreferencePage_lblPessoalusoEstritamente_text);
		new Label(top, SWT.NONE);

		final Label lblSeuAniversrio = new Label(top, SWT.NONE);
		lblSeuAniversrio.setText(Messages.DeveloperStudioPreferencePage_lblSeuAniversrio_text);

		final Composite composite = new Composite(top, SWT.NONE);
		composite.setLayout(new GridLayout(4, false));

		final Label lblMs = new Label(composite, SWT.NONE);
		lblMs.setText(Messages.DeveloperStudioPreferencePage_lblMs_text);

		spnMonth = new Spinner(composite, SWT.BORDER);
		spnMonth.setMaximum(12);

		final Label lblDia = new Label(composite, SWT.NONE);
		lblDia.setText(Messages.DeveloperStudioPreferencePage_lblDia_text);

		spnDay = new Spinner(composite, SWT.BORDER);
		spnDay.setMaximum(31);

		updateAttributes();

		return top;
	}

	@Override
	public IAdaptable getElement() {
		return null;
	}

	/*
	 *
	 */
	@Override
	public void setElement(final IAdaptable element) {
	}

	/*
	 *
	 */
	@Override
	public boolean performOk() {
		final boolean result = super.performOk();

		if (result) {
			final IPreferenceStore ps = getPreferenceStore();

			ps.setValue(ITDSPreferenceKeys.USER_BIRTH_DAY, spnDay.getSelection());
			ps.setValue(ITDSPreferenceKeys.USER_BIRTH_MONTH, spnMonth.getSelection());
		}

		return result;
	}

	@Override
	protected void performDefaults() {
		final IPreferenceStore ps = getPreferenceStore();

		ps.setToDefault(ITDSPreferenceKeys.USER_BIRTH_DAY);
		ps.setToDefault(ITDSPreferenceKeys.USER_BIRTH_MONTH);

		super.performDefaults();

		updateAttributes();
	}

	private void updateAttributes() {
		final IPreferenceStore ps = TDSUIActivator.getDefault().getPreferenceStore();

		spnDay.setSelection(ps.getInt(ITDSPreferenceKeys.USER_BIRTH_DAY));
		spnMonth.setSelection(ps.getInt(ITDSPreferenceKeys.USER_BIRTH_MONTH));
	}
}
