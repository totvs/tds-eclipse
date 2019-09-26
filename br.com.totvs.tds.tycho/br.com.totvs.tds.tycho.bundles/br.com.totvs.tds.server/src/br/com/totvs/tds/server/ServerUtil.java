package br.com.totvs.tds.server;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.internal.content.ContentTypeManager;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.content.IContentType;

@SuppressWarnings("restriction")
public class ServerUtil {

	private static List<String> sourceExtensions;

	/**
	 * Returns the content type extension.
	 * 
	 * @param contentTypeIdentifier - String
	 * @return String[]
	 */
	private static String[] getContentTypeExtensions(final String contentTypeIdentifier) {
		IContentType contentType = ContentTypeManager.getInstance().getContentType(contentTypeIdentifier);
		return (contentType == null) ? (new String[0]) : contentType.getFileSpecs(IContentType.FILE_EXTENSION_SPEC);
	}

	public static boolean isSourceFile(String fullNameOrExtension) {
		if (sourceExtensions == null) {
			sourceExtensions = new ArrayList<String>();
			
			String[] extensions = getContentTypeExtensions("br.com.totvs.tds.lsp.advplSource");
			sourceExtensions.addAll(Arrays.asList(extensions));

			extensions = getContentTypeExtensions("br.com.totvs.tds.lsp.advplAspSource");
			sourceExtensions.addAll(Arrays.asList(extensions));
		}

		IPath path = Path.fromOSString(fullNameOrExtension);
		String ext = path.getFileExtension().toUpperCase();

		return sourceExtensions.contains(ext);
	}

}