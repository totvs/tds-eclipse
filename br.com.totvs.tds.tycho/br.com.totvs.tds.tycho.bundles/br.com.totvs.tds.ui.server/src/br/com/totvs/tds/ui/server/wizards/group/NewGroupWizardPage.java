package br.com.totvs.tds.ui.server.wizards.group;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.NewGroupVO;

/**
 * P�gina de novo grupo.
 *
 * @author acandido
 */
public class NewGroupWizardPage extends WizardPage {
	private final NewGroupVO newGroup;
	private Text txtGroupName;
	private Text txtParent;

	/**
	 * Construtor.
	 *
	 * @param newGroup VO do novo grupo.
	 */
	public NewGroupWizardPage(final NewGroupVO newGroup) {
		super("newGroupWizardPage", "Novo Grupo", ServerUIIcons.getWizardGroup()); //$NON-NLS-1$ //$NON-NLS-2$
		this.newGroup = newGroup;
	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent container da página.
	 */
	@Override
	public void createControl(final Composite parent) {
		Composite topLevel = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		layout.verticalSpacing = 9;
		topLevel.setLayout(layout);

		Label lblNewLabel = new Label(topLevel, SWT.NONE);
		lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblNewLabel.setText(Messages.NewGroupWizardPage_target);

		txtParent = new Text(topLevel, SWT.BORDER);

		txtParent.setEnabled(false);
		txtParent.setEditable(false);
		txtParent.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		Label l1 = new Label(topLevel, SWT.NULL);
		l1.setText(Messages.NewGroupWizardPage_name);

		txtGroupName = new Text(topLevel, SWT.NULL);
		txtGroupName.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		txtGroupName.setText(newGroup.group.getName());

		if (newGroup.parent != null) {
			txtParent.setText(newGroup.parent.getName());
		}

		txtGroupName.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		setControl(topLevel);
		dialogChanged();
	}

	/**
	 * Responde a mudan�as no diálogo.
	 */
	private void dialogChanged() {
		updateStatus(null);
		//
		String name = txtGroupName.getText();

		if (name.isEmpty()) {
			updateStatus(Messages.NewGroupWizardPage_required_name_warning);
			return;
		}

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IGroupInfo group = serverManager.getGroup(name);
		if (group != null) {
			updateStatus(Messages.NewGroupWizardPage_group_name_already_exist);
			return;
		}

		newGroup.group.setName(name);

		try {
			newGroup.group.isValid();
		} catch (Exception e) {
			updateStatus(String.format(e.getMessage()));
		}
	}

	@Override
	public void performHelp() {
		PlatformUI.getWorkbench().getHelpSystem()
				.displayHelp("br.com.totvs.tds.ui.server.help.04-assistente_novo_grupo"); //$NON-NLS-1$
	}

	/**
	 * Atualiza o status do diálogo.
	 *
	 * @param message mensagem a ser apresentada ou null.
	 */
	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
}
