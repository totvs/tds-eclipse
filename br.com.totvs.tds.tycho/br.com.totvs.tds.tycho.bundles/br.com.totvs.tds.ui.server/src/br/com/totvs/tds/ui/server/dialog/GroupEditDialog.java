package br.com.totvs.tds.ui.server.dialog;

import java.util.Arrays;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wb.swt.ResourceManager;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.nl.Messages;

public class GroupEditDialog extends EditTitleAreaDialog {

	private IGroupInfo groupInfo;
	private Text txtParent;
	private Text txtGroupName;

	/**
	 * Create the dialog.
	 *
	 * @param parentShell
	 */
	public GroupEditDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	public void create() {
		super.create();

		setTitle(String.format("Grupo %s", groupInfo.getName()));
		setMessage("Edição de atributos do item.", IMessageProvider.INFORMATION);
	}

	/**
	 * Create contents of the dialog.
	 *
	 * @param parent
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		setTitleImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/group_dialog.png"));

		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayout(new GridLayout(2, false));
		container.setLayoutData(new GridData(GridData.FILL_BOTH));

		Label lblNewLabel = new Label(container, SWT.NONE);
		lblNewLabel.setText(Messages.NewGroupWizardPage_target);

		txtParent = new Text(container, SWT.BORDER);
		txtParent.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		txtParent.setEnabled(false);
		txtParent.setEditable(false);

		Label l1 = new Label(container, SWT.NULL);
		l1.setText("Nome");

		txtGroupName = new Text(container, SWT.NULL);
		txtGroupName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtGroupName.setText("");

		txtGroupName.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(final ModifyEvent e) {
				validadeInput();
			}
		});

		return area;
	}

	@Override
	protected void doValidadeInput() {
		//
		String name = txtGroupName.getText();
		if (name.isEmpty()) {
			setErrorMessage("Informe o nome do grupo");
			return;
		}

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IGroupInfo group = serverManager.getGroup(name);
		if ((group != null) && (!group.equals(groupInfo))) {
			setErrorMessage("O nome de grupo informado Já existe");
			return;
		}
	}

	@Override
	public IItemInfo getItemInfo() {

		return groupInfo;
	}

	@Override
	protected void saveInput() {
		groupInfo.setName(txtGroupName.getText());
	}

	@Override
	public void setItemInfo(IItemInfo itemInfo) {
		groupInfo = (IGroupInfo) itemInfo;
	}

	@Override
	protected void updateInput() {
		IGroupInfo[] parents = groupInfo.getFullParent();
		String[] parentNames = Arrays.stream(parents).map(gi -> gi.getName()).toArray(String[]::new);

		txtParent.setText(Arrays.toString(parentNames));
		txtGroupName.setText(groupInfo.getName());
	}

}
