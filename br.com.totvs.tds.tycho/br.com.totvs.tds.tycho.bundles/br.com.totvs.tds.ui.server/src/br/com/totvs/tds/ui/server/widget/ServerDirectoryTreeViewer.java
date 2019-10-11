package br.com.totvs.tds.ui.server.widget;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;
import br.com.totvs.tds.ui.server.fileSystem.ServerFileSystemFactory;

/**
 * ServerDirectoryTreeViewer.
 *
 * @author Leo Watanabe
 *
 */
public class ServerDirectoryTreeViewer extends TreeViewer {

	private IServerDirectoryItemNode root;
	private ISelectionChangedListener listener;

	private List<IServerDirectoryItemNode> items;
	private List<IItemSelectionChangedListener> listeners;

	public ServerDirectoryTreeViewer(final Composite parent, final boolean multi) {
		super(parent, (multi ? SWT.MULTI : SWT.NONE) | SWT.BORDER);

		root = ServerFileSystemFactory.createRootNode();
		createListener();
		createContents();

		items = new ArrayList<>();
		listeners = new ArrayList<>();
	}

	public void addItemSelectionChangedListener(final IItemSelectionChangedListener listener) {
		listeners.add(listener);
	}

	public void addServerDirectoryNode(final IServerDirectoryServerNode server) {
		root.addItemNode(server);
		refresh();
	}

	public void addServerDirectoryNodes(final List<IServerDirectoryServerNode> servers) {
		for (IServerDirectoryServerNode server : servers) {
			root.addItemNode(server);
		}
		refresh();
	}

	private void createContents() {
		Tree tree = getTree();
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.minimumHeight = 150;
		gd.minimumWidth = 350;
		tree.setLayoutData(gd);
		setContentProvider(new ServerDirectoryContentProvider());
		setLabelProvider(new ServerDirectoryLabelProvider());
		setInput(root);
		addSelectionChangedListener(listener);
	}

	private void createListener() {
		listener = new ISelectionChangedListener() {
			@SuppressWarnings("rawtypes")
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				ISelection selection = event.getSelection();
				if (selection.isEmpty()) {
					items.clear();
				} else if (selection instanceof IStructuredSelection) {
					items.clear();
					IStructuredSelection structuredSelection = (IStructuredSelection) selection;
					for (Iterator iterator = structuredSelection.iterator(); iterator.hasNext();) {
						Object obj = iterator.next();
						if (obj instanceof IServerDirectoryItemNode) {
							items.add((IServerDirectoryItemNode) obj);
						}
					}
				}

				notifyListener();
			}
		};
	}

	public IServerDirectoryItemNode getItem() {
		return items.isEmpty() ? null : items.get(0);
	}

	public List<IServerDirectoryItemNode> getItems() {
		return items;
	}

	private void notifyListener() {
		for (IItemSelectionChangedListener listener : listeners) {
			listener.selectionChanged();
		}
	}

	public void addServerDirectoryNode(IServerDirectoryServerNode serverNode, boolean busy) {
		boolean oldBusy = isBusy();
		setBusy(busy);
		
		addServerDirectoryNode(serverNode);
		
		setBusy(oldBusy);
	}

}
