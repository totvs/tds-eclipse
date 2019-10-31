package br.com.totvs.tds.ui.console;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Path;
import org.eclipse.ui.console.IHyperlink;

public class ResourceLink implements IHyperlink {

	static final private Map<String, ResourceLink> hyperlinkMap = new HashMap<String, ResourceLink>();

//	private final String filename;
//	private int line;
//	private String project;

	/**
	 * Creates a new link to the specified file at line <code>line_number</code>
	 *
	 * @param filename The path or filename to the file.
	 * @param ln       the line number inside the file you want to link.
	 */
	protected ResourceLink(final String filename) {
//		this.filename = filename;
	}

	static public ResourceLink getLink(String filename, final int line) {
		filename = filename.toLowerCase();
		ResourceLink result = hyperlinkMap.get(filename);

		if (result == null) {
			result = new ResourceLink(filename);
			hyperlinkMap.put(filename, result);
		}

//		result.line = line;

		return result;
	}

	static public ResourceLink getLink(final String project, final String fullpath) {
		final String filename = Path.fromOSString(fullpath).lastSegment().toLowerCase();
		ResourceLink result = hyperlinkMap.get(filename);

		if (result == null) {
			result = new ResourceLink(filename);
			hyperlinkMap.put(filename, result);
		}

		// result.project = project;

		return result;
	}

	@Override
	public void linkEntered() {
	}

	@Override
	public void linkExited() {
	}

	@Override
	public void linkActivated() {
		// abrir o link
	}

	/**
	 * Limpa mapa
	 */
	public static void clearMap() {
		hyperlinkMap.clear();

	}
}