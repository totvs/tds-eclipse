package br.com.totvs.tds.ui.server.wizards.group;

import org.eclipse.jface.wizard.Wizard;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.NewGroupVO;
import br.com.totvs.tds.ui.server.wizards.INewItemWizard;

/**
 * Assistente de novo grupo.
 *
 * @author acandido
 */
public final class NewGroupWizard extends Wizard implements INewItemWizard {

	private final NewGroupVO newGroup = new NewGroupVO();
	private NewGroupWizardPage newGroupPage;

	/**
	 * Construtor.
	 */
	public NewGroupWizard() {
		super();

		setWindowTitle(Messages.NewGroupWizard_new_group_wizard_title);
		setDefaultPageImageDescriptor(ServerUIIcons.getWizardGroup());

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		newGroup.group = serverManager.newGroup(Messages.NewGroupWizard_name);
	}

	@Override
	public void addPages() {
		newGroupPage = new NewGroupWizardPage(newGroup);

		addPage(newGroupPage);
	}

	@Override
	public IItemInfo getNewItem() {
		return newGroup.group;
	}

	@Override
	public boolean performFinish() {
		boolean ret = false;
		try {
			newGroup.parent.addChild(newGroup.group);
			ret = true;
		} catch (RuntimeException e) {
			e.printStackTrace();
			newGroupPage.setErrorMessage(e.getMessage());
		}
		return ret;
	}

	@Override
	public void setParentItem(IGroupInfo parent) {
		newGroup.parent = parent;
	}

}
