package br.com.totvs.tds.ui.server.wizards;

import org.eclipse.jface.wizard.IWizard;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;

public interface INewItemWizard extends IWizard {

	IItemInfo getNewItem();

	void setParentItem(IGroupInfo parent);

}
