package br.com.totvs.tds.ui.server.providers;

import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.widgets.ImageHyperlink;

import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;

public class ColumnIconsLabelProvider extends ColumnLabelProvider {

	Map<Object, ImageHyperlink> links = new HashMap<Object, ImageHyperlink>();

	private void addImageHyperLink(final ApplyPatchFileReturn applyPatchFileReturn, final Composite composite,
			final TableItem item) {
		if (applyPatchFileReturn != null) {
			String documentationURL = ""; // applyPatchFileReturn.getDocumentationURL();
			if (documentationURL != null && !documentationURL.isEmpty()) {
				int iconColumnIndex = 3;
				ImageHyperlink link = createImageHyperLinkItem(documentationURL, composite);
				links.put(item, link);
				link.setData("inputObject", applyPatchFileReturn); //$NON-NLS-1$
				TableEditor tableEditor = new TableEditor(item.getParent());
				tableEditor.grabHorizontal = true;
				tableEditor.setEditor(link, item, iconColumnIndex);
			}
		}
	}

	private ImageHyperlink createImageHyperLinkItem(final String documentationURL, final Composite table) {
		ImageHyperlink link = new ImageHyperlink(table, SWT.FILL | SWT.READ_ONLY | SWT.TRANSPARENT);
		link.setHref(documentationURL);
		Cursor cursor = new Cursor(link.getDisplay(), SWT.CURSOR_HAND);
		link.setCursor(cursor);
		link.setImage(ServerUIIcons.getLinkIcon().createImage());
		link.addHyperlinkListener(new HyperlinkAdapter() {
			@Override
			public void linkActivated(final org.eclipse.ui.forms.events.HyperlinkEvent e) {
				String href = String.valueOf(e.getHref());
				try {
					TDSUtil.openExternalBrowser(href);
				} catch (PartInitException e1) {
					ServerUIActivator.showStatus(IStatus.ERROR, "Não foi possível acionar o endereço.\n\tURL: %s", href,
							e1);
				} catch (MalformedURLException e1) {
					ServerUIActivator.showStatus(IStatus.ERROR, "Endereço com formato inválido.\n\tURL: %s", href, e1);
				}
			}

		});
		link.setBackground(new Color(link.getDisplay(), new RGB(255, 255, 255)));
		return link;
	}

	@Override
	public Color getBackground(final Object element) {
		return super.getBackground(element);
	}

	@Override
	public Image getImage(final Object element) {
		Image image = ServerUIIcons.getLinkIcon().createImage();
		return super.getImage(element);
	}

	@Override
	public String getText(final Object element) {
		return ""; //$NON-NLS-1$
	}

	@Override
	public void update(final ViewerCell cell) {
		TableItem item = (TableItem) cell.getItem();
		ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) cell.getElement();
		if (!links.containsKey(item)) {
			addImageHyperLink(applyPatchFileReturn, (Composite) cell.getViewerRow().getControl(), item);
		}
	}

}
