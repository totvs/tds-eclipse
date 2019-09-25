package br.com.totvs.tds.ui.server.wizards.server;

import org.eclipse.jface.wizard.Wizard;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.wizards.INewItemWizard;

/**
 * Assistente de seleção de servidor.
 *
 * @author acandido
 */
public class ServerSelectionWizard extends Wizard implements INewItemWizard {

	// Elemento pai.
	private IGroupInfo parentItem;
	private ServerSelectionPage serverSelectionPage;

	/**
	 * Construtor.
	 *
	 * @param parent elemento pai.
	 */
	public ServerSelectionWizard() {
		setWindowTitle(Messages.ServerSelectionWizard_new_server);
	}

	@Override
	public void addPages() {
		serverSelectionPage = new ServerSelectionPage();
		serverSelectionPage.setParentItem(parentItem);
		addPage(serverSelectionPage);
		setForcePreviousAndNextButtons(true);
	}

	@Override
	public IItemInfo getNewItem() {
		return serverSelectionPage.getNewItem();
	}

	@Override
	public boolean performFinish() {
		return false;
	}

	@Override
	public void setParentItem(final IGroupInfo parent) {
		parentItem = parent;
	}

}
