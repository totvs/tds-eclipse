package br.com.totvs.tds.ui.console;

import java.net.URL;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.console.IHyperlink;

import br.com.totvs.tds.ui.TDSUIActivator;

public class HttpLink implements IHyperlink {

	private URL url;

	/**
	 * Creates a new link to the specified url address
	 * 
	 * @param url
	 *            The url to open.
	 */
	public HttpLink(URL url) {
		this.url = url;
	}

	@Override
	public void linkEntered() {
	}

	@Override
	public void linkExited() {
	}

	@Override
	public void linkActivated() {
		try {
			IWebBrowser browser = PlatformUI.getWorkbench().getBrowserSupport().createBrowser(url.toString());
			browser.openURL(url);
		} catch (PartInitException e) {
			TDSUIActivator.logStatus(IStatus.ERROR, Messages.HttpLink_Navigator, e.getMessage(), e);
		}
	}
}