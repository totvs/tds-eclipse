package br.com.totvs.tds.ui.server.dialog;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.nl.Messages;

public abstract class EditTitleAreaDialog extends TitleAreaDialog {

	/**
	 * Construtor.
	 *
	 * @param parentShell
	 */
	public EditTitleAreaDialog(Shell parentShell) {
		super(parentShell);
	}

	/**
	 *
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(Messages.EditTitleAreaDialog_Edit);
	}

	@Override
	public void create() {
		super.create();

		updateInput();
	}

	/**
	 * Create contents of the button bar.
	 *
	 * @param parent
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	abstract protected void doValidadeInput();

	/**
	 * Return the initial size of the dialog.
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(450, 300);
	}

	/**
	 *
	 * @return
	 */
	abstract public IItemInfo getItemInfo();

	@Override
	protected void okPressed() {
		if (!getItemInfo().isValid()) {
			setErrorMessage(getItemInfo().getMessageError());
			cancelPressed();
		} else {
			saveInput();
			super.okPressed();
		}
	}

	/**
	 * Salva dados editados
	 */
	abstract protected void saveInput();

	/**
	 *
	 * @param itemInfo
	 */
	abstract public void setItemInfo(IItemInfo itemInfo);

	/**
	 * Atualiza o diálogo
	 */
	abstract protected void updateInput();;

	/**
	 * Valida a entrada de dados
	 */
	protected void validadeInput() {
		setErrorMessage(null);

		doValidadeInput();

		getButton(IDialogConstants.OK_ID).setEnabled(getErrorMessage() == null);
	}

}
