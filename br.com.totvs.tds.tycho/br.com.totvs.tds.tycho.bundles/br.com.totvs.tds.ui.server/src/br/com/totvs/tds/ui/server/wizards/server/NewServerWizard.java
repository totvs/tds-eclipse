package br.com.totvs.tds.ui.server.wizards.server;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.custom.BusyIndicator;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.NewServerVO;
import br.com.totvs.tds.ui.server.wizards.INewItemWizard;

/**
 * Assistente de novo grupo.
 *
 * @author acandido
 */
public final class NewServerWizard extends Wizard implements INewItemWizard {

	private boolean finish;
	private NewServerVO newServer;
	private NewServerWizardPage newServerPage;

	/**
	 * Construtor.
	 *
	 * @param subTitle Sub-título indicando o tipo de servidor.
	 * @param serverVO VO com informações para uso pelo assistente.
	 */
	/**
	 * Construtor.
	 *
	 * @param newServerVO
	 * @param name
	 */
	public NewServerWizard(NewServerVO newServerVO) {
		super();

		setWindowTitle(Messages.NewServerWizard_new_server_wizard_title);
		setDefaultPageImageDescriptor(ServerUIIcons.getWizardServer());
		newServer = newServerVO;
	}

	@Override
	public void addPages() {
		newServerPage = new NewServerWizardPage(newServer);
		addPage(newServerPage);
	}

	@Override
	public boolean canFinish() {

		return (super.canFinish() && newServerPage.isPageComplete());
	}

	@Override
	public IItemInfo getNewItem() {
		return newServer.getServer();
	}

	@Override
	public boolean performFinish() {
		finish = false;
		//
		BusyIndicator.showWhile(getShell().getDisplay(), new Runnable() {

			@Override
			public void run() {
				IServerInfo server = newServer.getServer();

				try {
					if (newServer.isImmediateConnection()) {
						server.setProperty(IServerConstants.IMMEDIATE_CONNECTION, true);
					}

					newServer.getParent().addChild(server);
					finish = true;

					ServerUIActivator.logStatus(IStatus.INFO, Messages.NewServerWizard_server,
							Messages.NewServerWizard_server_added_warning, server.getName(), server.getAddress());

				} catch (RuntimeException e) {
					ServerUIActivator.logStatus(IStatus.ERROR, Messages.NewServerWizard_server, e.getMessage(), e);
					newServerPage.setErrorMessage(e.getMessage());
					server.setProperty(IServerConstants.IMMEDIATE_CONNECTION, false);
				}
			}
		});
		//
		return finish;
	}

	@Override
	public void setParentItem(final IGroupInfo parent) {
		newServer.setParent(parent);
	}

}
