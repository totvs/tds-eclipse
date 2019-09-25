package br.com.totvs.tds.ui.console;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.console.IPatternMatchListener;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.PatternMatchEvent;
import org.eclipse.ui.console.TextConsole;

public class SourceFileMatchListener implements IPatternMatchListener {

	private static final String FOLDER = "(\\w+\\\\)+"; //$NON-NLS-1$
	private static final String FILE = "(\\w+\\.\\w{1,3}]*)"; //$NON-NLS-1$
	private static final String LINE = "\\(([0-9]*)\\)"; //$NON-NLS-1$

	private static final Pattern PATTERN_LINE = Pattern.compile(FILE + LINE, Pattern.CASE_INSENSITIVE);
	private static final Pattern PATTERN_FILE = Pattern.compile(FOLDER + FILE, Pattern.CASE_INSENSITIVE);
	private static final Pattern FULL_PATTERN = Pattern.compile(FILE + LINE + "|" + FOLDER + FILE, //$NON-NLS-1$
			Pattern.CASE_INSENSITIVE);

	@Override
	public void connect(final TextConsole console) {

	}

	@Override
	public void disconnect() {
		ResourceLink.clearMap();
	}

	@Override
	public final void matchFound(final PatternMatchEvent event) {
		if (event.getSource() instanceof MessageConsole) {
			MessageConsole console = (MessageConsole) event.getSource();
			final IDocument doc = console.getDocument();
			final int offset = event.getOffset();
			final int lenght = event.getLength();

			try {
				String str = doc.get(offset, lenght);
				Matcher m = PATTERN_LINE.matcher(str);
				if (m.matches()) {
					if (m.groupCount() > 1) {
						String file = m.group(1);
						int line = Integer.parseInt(m.group(2));

						console.addHyperlink(ResourceLink.getLink(file, line), offset, lenght);
					}
				}

				m = PATTERN_FILE.matcher(str);
				if (m.matches()) {
					if (m.groupCount() > 1) {
						IPath path = Path.fromOSString(m.group(0));
						String project = path.segment(0);
						path = path.removeFirstSegments(1);
						String file = path.toPortableString();
						
						console.addHyperlink(ResourceLink.getLink(project, file), offset, lenght);
					}
				}
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			} catch (NumberFormatException e1) {
				e1.printStackTrace();
			}
		}
	}

	@Override
	public final String getPattern() {
		return FULL_PATTERN.pattern();
	}

	@Override
	public final int getCompilerFlags() {
		return Pattern.CASE_INSENSITIVE;
	}

	@Override
	public final String getLineQualifier() {
		return null;
	}

}
