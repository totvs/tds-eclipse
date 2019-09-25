package br.com.totvs.tds.ui.server.wizards.server;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardSelectionPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Seleção de assistente para criação de novo servidor.
 *
 * @author acandido
 */
public class ServerSelectionPage extends WizardSelectionPage {

	private static final String WIZARD_SERVER = Messages.ServerSelectionPage_server_wizard_title;

	private Label lblNewLabel;
	private IGroupInfo parentElement;
	private TableViewer projectType;
	private ServerWizardNode selectedWizardNode;
	private Text txtParent;

	/**
	 * Construtor.
	 *
	 * @param parent elemento pai.
	 */
	public ServerSelectionPage() {
		super("serverSelectionPage"); //$NON-NLS-1$

		setTitle(String.format(WIZARD_SERVER, "")); //$NON-NLS-1$
		setDescription(Messages.ServerSelectionPage_selection_type_warning);
		setImageDescriptor(ServerUIIcons.getWizardServer());
	}

	@Override
	public void createControl(final Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));

		lblNewLabel = new Label(composite, SWT.NONE);
		lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		lblNewLabel.setText(Messages.ServerSelectionPage_group);

		txtParent = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
		txtParent.setEnabled(false);
		txtParent.setEditable(true);
		txtParent.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		Composite composite1 = new Composite(composite, SWT.NONE);
		GridLayout glComposite1 = new GridLayout(2, false);
		glComposite1.verticalSpacing = 0;
		glComposite1.marginHeight = 0;
		glComposite1.horizontalSpacing = 0;
		composite1.setLayout(glComposite1);
		composite1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));

		// Project type
		Label l1 = new Label(composite1, SWT.NONE);
		l1.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		l1.setText(Messages.ServerSelectionPage_server_type_warning);
		new Label(composite1, SWT.NONE);

		projectType = new TableViewer(composite);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.horizontalSpan = 2;
		projectType.getTable().setLayoutData(gridData);
		projectType.addSelectionChangedListener(new ISelectionChangedListener() {

			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				ISelection selection = event.getSelection();
				if (!selection.isEmpty() && selection instanceof IStructuredSelection) {
					Object o = ((IStructuredSelection) selection).getFirstElement();
					if (o instanceof ServerWizardNode) {
						// Now we set our selected node, which toggles the next button
						selectedWizardNode = (ServerWizardNode) o;
						setTitle(String.format(WIZARD_SERVER, selectedWizardNode.getName()));
						setSelectedNode(selectedWizardNode);
					}
				}
			}
		});

		projectType.setContentProvider(new ArrayContentProvider());
		projectType.setLabelProvider(new LabelProvider() {

			@Override
			public String getText(final Object element) {
				if (element instanceof ServerWizardNode) {
					return ((ServerWizardNode) element).getName();
				}
				return super.getText(element);
			}
		});

		txtParent.setText(parentElement.getName());

		fillServerList();
		setControl(composite);
	}

	protected void fillServerList() {
		ServerWizardNode[] wizardNodes = new ServerWizardNode[3];

		wizardNodes[0] = new ServerWizardNode("Protheus", "protheus", this); //$NON-NLS-1$ //$NON-NLS-2$
		wizardNodes[1] = new ServerWizardNode("Logix", "logix", this); //$NON-NLS-1$ //$NON-NLS-2$
		wizardNodes[2] = new ServerWizardNode("DBAccess", "dbaccess", this); //$NON-NLS-1$ //$NON-NLS-2$

		projectType.setInput(wizardNodes);
	}

	/**
	 * @return novo item
	 */
	public IItemInfo getNewItem() {
		return selectedWizardNode.getNewItem();
	}

	/**
	 * @return item ascendente (pai)
	 */
	public IGroupInfo getParentElement() {
		return parentElement;
	}

	/**
	 * Ajusta o item ascendente (pai).
	 *
	 * @param parent
	 */
	public void setParentItem(final IGroupInfo parent) {
		parentElement = parent;
	}

}
