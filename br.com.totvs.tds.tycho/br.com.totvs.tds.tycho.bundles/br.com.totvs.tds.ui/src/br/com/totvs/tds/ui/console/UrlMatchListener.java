package br.com.totvs.tds.ui.console;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.console.IHyperlink;
import org.eclipse.ui.console.IPatternMatchListener;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.PatternMatchEvent;
import org.eclipse.ui.console.TextConsole;

import br.com.totvs.tds.ui.TDSUIActivator;

public class UrlMatchListener implements IPatternMatchListener {

	private static final Pattern PATTERN = Pattern.compile("https?://(www.)?([a-zA-Z0-9]+).[a-zA-Z0-9]*.[a-z]{3}.?(\\S+)*"); //$NON-NLS-1$
	private static final String LINE_QUALIFIER = null;

	/* (non-Javadoc)
	 * @see org.eclipse.ui.console.IPatternMatchListenerDelegate#connect(org.eclipse.ui.console.TextConsole)
	 */
	@Override
	public void connect(final TextConsole console) {

	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.console.IPatternMatchListenerDelegate#disconnect()
	 */
	@Override
	public void disconnect() {

	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.console.IPatternMatchListenerDelegate#matchFound(org.eclipse.ui.console.PatternMatchEvent)
	 */
	@Override
	public final void matchFound(final PatternMatchEvent event) {
		if (event.getSource() instanceof MessageConsole) {
			MessageConsole console = (MessageConsole) event.getSource();
			final IDocument doc = console.getDocument();
			final int offset = event.getOffset();
			final int lenght = event.getLength();

			try {
				String str = doc.get(offset, lenght-1);
				URL url = new URL(str); 
				IHyperlink link = new HttpLink(url);
				
				console.addHyperlink(link, offset, lenght);
			} catch (BadLocationException e) {
				TDSUIActivator.logStatus(IStatus.ERROR, "Ligação", e.getMessage(), e);
			} catch (NumberFormatException e) {
				TDSUIActivator.logStatus(IStatus.ERROR, "Ligação", e.getMessage(), e);
			} catch (MalformedURLException e) {
				TDSUIActivator.logStatus(IStatus.ERROR, "Ligação", e.getMessage(), e);
				e.printStackTrace();
			}
		}
	}

	@Override
	public final String getPattern() {
		return PATTERN.pattern();
	}

	@Override
	public final int getCompilerFlags() {
		return Pattern.CASE_INSENSITIVE;
	}

	@Override
	public final String getLineQualifier() {
		return LINE_QUALIFIER; 
	}

}
