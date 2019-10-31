package br.com.totvs.tds.ui.server.wizards.server;

import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardNode;
import org.eclipse.swt.graphics.Point;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.ServerType;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.vo.NewServerVO;
import br.com.totvs.tds.ui.server.wizards.INewItemWizard;

/**
 * informações para a seleção de assistente espec�fico.
 *
 * @author acandido
 */
public class ServerWizardNode implements IWizardNode {

	/**
	 * VO para uso do assistente.
	 */
	private final NewServerVO newServer;

	/**
	 * This is the wizard that this IWizardNode represents. One reason to keep this
	 * is so we can actuall check if the wizard is created at the isContentCreated
	 * method.
	 */
	private INewItemWizard wizard;

	/**
	 * Construtor.
	 *
	 * @param name                , nome a ser apresentado.
	 * @param type                , tipo de servidor.
	 * @param serverSelectionPage ,elemento pai
	 */
	public ServerWizardNode(final ServerType serverType, final ServerSelectionPage serverSelectionPage) {
		this.newServer = new NewServerVO();
		this.newServer.setParent(serverSelectionPage.getParentElement());
		this.newServer.setServerType(serverType);
	}

	/**
	 * Disposes the wizard managed by this node. Does nothing if the wizard has not
	 * been created. This is the last message that should ever be sent to this node.
	 */
	@Override
	public void dispose() {

	}

	/**
	 * Returns the extent of the wizard for this node. If the content has not yet
	 * been created, calling this method does not trigger the creation of the
	 * wizard. This allows this node to suggest an extent in advance of actually
	 * creating the wizard.
	 *
	 * @return posição
	 */
	@Override
	public Point getExtent() {
		return new Point(-1, -1);
	}

	/**
	 * @return nome a ser apresentado
	 */
	public String getName() {
		return this.newServer.getServerType().getTitle();
	}

	public IItemInfo getNewItem() {
		return newServer.getServer();
	}

	/**
	 * Returns the wizard this node stands for. If the content has not been created
	 * beforehand, calling this method triggers the creation of the wizard and
	 * caches it so that the identical wizard object is returned on subsequent
	 * calls.
	 *
	 * @return assistente conforme seleção efetuada.
	 */
	@Override
	public IWizard getWizard() {
		if (wizard == null) {
			try {
				IServerManager serverManager = ServerActivator.getDefault().getServerManager();
				IAppServerInfo server = serverManager.newAppServer("Novo"); //$NON-NLS-1$
				newServer.setServer((IAppServerInfo) server);
				server.setServerType(newServer.getServerType());
				wizard = new NewServerWizard(newServer);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		return wizard;
	}

	/**
	 * @return returns whether a wizard has been created for this node.
	 */
	@Override
	public boolean isContentCreated() {
		return wizard != null;
	}

	public void setParentItem(IGroupInfo parent) {
		newServer.setParent(parent);
	}

}
