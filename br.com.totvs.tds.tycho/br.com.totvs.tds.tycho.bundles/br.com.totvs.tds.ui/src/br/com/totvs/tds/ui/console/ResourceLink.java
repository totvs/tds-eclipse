package br.com.totvs.tds.ui.console;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Path;
import org.eclipse.ui.console.IHyperlink;

public class ResourceLink implements IHyperlink {

	static final private Map<String, ResourceLink> hyperlinkMap = new HashMap<String, ResourceLink>();

	private final String filename;
	private int line;
	private String project;

	/**
	 * Creates a new link to the specified file at line <code>line_number</code>
	 * 
	 * @param filename
	 *            The path or filename to the file.
	 * @param ln
	 *            the line number inside the file you want to link.
	 */
	protected ResourceLink(String filename) {
		this.filename = filename;
	}

	static public ResourceLink getLink(String filename, int line) {
		filename = filename.toLowerCase();
		ResourceLink result = hyperlinkMap.get(filename);

		if (result == null) {
			result = new ResourceLink(filename);
			hyperlinkMap.put(filename, result);
		}
		
		result.line = line;

		return result;
	}

	static public ResourceLink getLink(String project, String fullpath) {
		String filename = Path.fromOSString(fullpath).lastSegment().toLowerCase(); 
		ResourceLink result = hyperlinkMap.get(filename);

		if (result == null) {
			result = new ResourceLink(filename);
			hyperlinkMap.put(filename, result);
		}
		
		result.project = project;

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
//		INotification notification = notificationService.createNotification(INotification.EventType.OPEN_EDITOR, this);
//		notification.setData("project", project); //$NON-NLS-1$
//		notification.setData("resource", filename); //$NON-NLS-1$
//		notification.setData("line", line); //$NON-NLS-1$
//
//		notificationService.sendSync(notification);
//		if (notification.getExcpetion() != null) {
//	AppLogger.getInstance().getLogger(HttpLink.class).warn("X", "Implementar abertura de arquivo") ;
//		}
	}

	/**
	 * Limpa mapa
	 */
	public static void clearMap() {
		hyperlinkMap.clear();
		
	}
}