package br.com.totvs.tds.ui.server.widget;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryDirNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryFileNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;

/**
 * ServerDirectoryDialog.
 *
 * @author Leo Watanabe
 *
 */
public class ServerDirectoryDialog extends TitleAreaDialog implements IItemSelectionChangedListener {

	private List<IServerDirectoryServerNode> servers;
	private boolean multi;
	private boolean fileSelection;

	private ServerDirectoryTreeViewer viewer;

	public ServerDirectoryDialog(final Shell parent, final IServerDirectoryServerNode server, final boolean multi,
			final boolean fileSelection) {
		super(parent);
		this.multi = multi;
		this.fileSelection = fileSelection;
		servers = new ArrayList<>();
		servers.add(server);
	}

	public ServerDirectoryDialog(final Shell parent, final List<IServerDirectoryServerNode> servers,
			final boolean multi, final boolean fileSelection) {
		super(parent);
		this.servers = servers;
		this.multi = multi;
		this.fileSelection = fileSelection;
	}

	/**
	 * Adiciona um servidor na �rvore principal.
	 *
	 * @param server
	 */
	public void addServerNode(final IServerDirectoryServerNode server) {
		viewer.addServerDirectoryNode(server);
	}

	// T�tulo do Dialog
	@Override
	protected void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Seleção no Servidor");
	}

	@Override
	public void create() {
		super.create();

		setTitle(String.format("Selecione um %s no servidor.", (fileSelection ? "arquivo" : "pasta")));
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		GridLayout layout = new GridLayout(); // 1, false);
		container.setLayout(layout);
		//
		viewer = new ServerDirectoryTreeViewer(container, multi);
		viewer.addServerDirectoryNodes(servers);
		viewer.addItemSelectionChangedListener(this);
		//
		return container;
	}

	/**
	 * Obt�m o item selecionado na �rvore principal.
	 *
	 * @return
	 */
	public IServerDirectoryItemNode getItem() {
		return viewer.getItem();
	}

	/**
	 * Obt�m a lista de itens selecionados na �rvore principal.
	 *
	 * @return
	 */
	public List<IServerDirectoryItemNode> getItems() {
		return viewer.getItems();
	}

	@Override
	public void selectionChanged() {
		boolean enabled = false;
		if (!multi) {
			IServerDirectoryItemNode item = getItem();
			// se for fileSelection verifica se � um IServerDirectoryFileNode
			// caso constrario verifica se � um IServerDirectoryDirNode
			enabled = (fileSelection) ? (item instanceof IServerDirectoryFileNode)
					: (item instanceof IServerDirectoryDirNode);
		} else {
			if (getItems() != null && !getItems().isEmpty()) {
				enabled = true;
				for (IServerDirectoryItemNode item : getItems()) {
					enabled = enabled && (fileSelection) ? (item instanceof IServerDirectoryFileNode)
							: (item instanceof IServerDirectoryDirNode);

				}
			}
		}
		String errorMessage = null;
		if (!enabled) {
			errorMessage = "Seleção inválida";
		}
		setErrorMessage(errorMessage);
		getButton(IDialogConstants.OK_ID).setEnabled(enabled);
	}

}
