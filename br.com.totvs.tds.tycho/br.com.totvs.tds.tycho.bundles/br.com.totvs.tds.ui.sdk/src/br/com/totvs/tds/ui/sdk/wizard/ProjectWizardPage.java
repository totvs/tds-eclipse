package br.com.totvs.tds.ui.sdk.wizard;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.ui.sdk.widget.IModifyIncludeListener;
import br.com.totvs.tds.ui.sdk.widget.IncludeConfigurationComposite;
import br.com.totvs.tds.ui.sdk.wrapper.ProjectVO;

/**
 * Implementation to wizard project page.
 * 
 * @author Audrin
 */
public class ProjectWizardPage extends WizardPage {

	private final ProjectVO voProject;
	private Text txtNameProject;

	private final int NUM_COLUMNS = 2;
	private final int VERTICAL_SPACING = 9;

	private IncludeConfigurationComposite containerIncludePath;

	/**
	 * P�gina principal do Wizard para criar um projeto.
	 * 
	 * @param pageName
	 */
	public ProjectWizardPage(final ISelection selection, final ProjectVO voProject) {
		super("wizardPage"); //$NON-NLS-1$
		setTitle(Messages.ProjectWizardPage_Totvs_project_wizard);
		setDescription(Messages.ProjectWizardPage_Wizard_creates_TOTVS_project);
		this.voProject = voProject;
	}

	/**
	 * @see IDialogPage#createControl(Composite).
	 */
	@Override
	public final void createControl(final Composite parent) {

		Composite container = new Composite(parent, SWT.FILL);
		GridLayout layout = new GridLayout();
		container.setLayout(layout);
		layout.numColumns = NUM_COLUMNS;
		layout.verticalSpacing = VERTICAL_SPACING;

		Label labelProject = new Label(container, SWT.NULL);
		labelProject.setText(Messages.ProjectWizardPage_Name);
		txtNameProject = new Text(container, SWT.BORDER | SWT.SINGLE);
		txtNameProject.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		txtNameProject.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		new Label(container, SWT.NULL);
		new Label(container, SWT.NULL);

		GridData gridDataPathList = new GridData(GridData.FILL_BOTH);
		gridDataPathList.grabExcessHorizontalSpace = true;
		gridDataPathList.grabExcessVerticalSpace = true;
		gridDataPathList.horizontalSpan = 2;

		containerIncludePath = new IncludeConfigurationComposite(container, SWT.NONE);
		containerIncludePath.setLayoutData(gridDataPathList);
		containerIncludePath.addModifyIncludeListener(new IModifyIncludeListener() {
			@Override
			public void modifyIncludes() {
				updateIncludes();
			}
		});

		initialize();
		dialogChanged();
		setControl(container);

	}

	/**
	 * Tests if the current workbench selection is a suitable container to use.
	 */
	private void initialize() {
		txtNameProject.setText(Messages.ProjectWizardPage_New_Project);
	}

	private void dialogChanged() {
		voProject.projectName = txtNameProject.getText();
		if (voProject.projectName.length() == 0) {
			updateStatus(Messages.ProjectWizardPage_Project_name_required);
			return;
		}
		if (voProject.projectName.replace('\\', '/').indexOf('/', 1) > 0) {
			updateStatus(Messages.ProjectWizardPage_Project_name_invalid);
			return;
		}
		updateStatus(null);
	}

	private void updateIncludes() {
		String[] includeSelection = containerIncludePath.getIncludeSelection();
		voProject.includes = new ArrayList<String>(Arrays.asList(includeSelection));
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}