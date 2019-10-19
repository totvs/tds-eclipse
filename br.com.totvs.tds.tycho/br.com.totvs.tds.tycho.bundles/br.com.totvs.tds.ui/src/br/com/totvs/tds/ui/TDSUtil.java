package br.com.totvs.tds.ui;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;

/**
 * Classe utilit�ria geral do startup.
 */
public final class TDSUtil {

	private static final String[] EMPTY_ARRAY = new String[0];
	private static final byte[] EMPTY_BUFFER = new byte[0];

	private TDSUtil() {
	}

	/**
	 * Verifica se a plataforma � Windows.
	 *
	 * @return true caso seja Windows
	 */
	public static boolean isWindows() {
		return Platform.getOS().equals(Platform.OS_WIN32);
	}

	/**
	 * Verifica se a plataforma é Linux.
	 *
	 * @return true caso seja Linux
	 */
	public static boolean isLinux() {
		return Platform.getOS().equals(Platform.OS_LINUX);
	}

	public static String directoryDialog(final Shell shell, final String message) {
		return directoryDialog(shell, null, message, null);
	}

	public static String directoryDialog(final Shell shell, final String title, final String message,
			final String filterPath) {
		if (isRunningInTestMode()) {
			return System.getProperty("return"); //$NON-NLS-1$
		}

		final DirectoryDialog dialog = new DirectoryDialog(shell);

		if ((message != null) && !message.isEmpty()) {
			dialog.setMessage(message);
		}
		if ((title != null) && !title.isEmpty()) {
			dialog.setText(title);
		}
		if ((filterPath != null) && !filterPath.isEmpty()) {
			dialog.setFilterPath(filterPath);
		}

		return dialog.open();
	}

	public static boolean isRunningInTestMode() {

		return (Boolean.TRUE.toString().equals(System.getProperty("isTest", Boolean.FALSE.toString()))); //$NON-NLS-1$
	}

	public static String[] multFileDialog(final Shell shell, final String[] filter, final String[] filterNames) {
		if (isRunningInTestMode()) {
			return new String[] { System.getProperty("return") }; //$NON-NLS-1$
		}

		final FileDialog dialog = new FileDialog(shell, SWT.MULTI);
		dialog.setFilterExtensions(filter);
		dialog.setFilterNames(filterNames);
		dialog.open();

		final String folder = dialog.getFilterPath();

		final String[] files = dialog.getFileNames();
		for (int i = 0; i < files.length; i++) {
			files[i] = Paths.get(folder, files[i]).toString();
		}

		return files;
	}

	public static String fileDialog(final Shell shell, final String[] filter, final String[] filterNames) {
		if (isRunningInTestMode()) {
			return System.getProperty("return"); //$NON-NLS-1$
		}

		final FileDialog dialog = new FileDialog(shell, SWT.NONE);
		dialog.setFilterExtensions(filter);
		dialog.setFilterNames(filterNames);

		return dialog.open();
	}

	public static String fileSaveDialog(final Shell shell, final String[] filter, final String[] filterNames,
			final String defaultFilename) {
		if (isRunningInTestMode()) {
			return System.getProperty("return"); //$NON-NLS-1$
		}

		final FileDialog dialog = new FileDialog(shell, SWT.SAVE);
		dialog.setFilterExtensions(filter);
		dialog.setFilterNames(filterNames);
		dialog.setFileName(defaultFilename);

		return dialog.open();
	}

	public static String fileDialog(final Shell shell) {

		return fileDialog(shell, EMPTY_ARRAY, EMPTY_ARRAY);
	}

	public static boolean isMac() {

		return !isWindows() && !isLinux();
	}

	/**
	 * Uncompress a file and return the content.
	 *
	 * @param zipFile            - The zipped file
	 * @param fDestinationFolder - The folder to unzip.
	 * @param writeToDisk        - If it should write to disk.
	 * @return A list containing all unzipped files.
	 * @throws IOException - Any IOException thrown.
	 */
	public static List<File> unzipFile(final ZipFile zipFile, final String fDestinationFolder,
			final boolean writeToDisk) throws IOException {
		final List<File> fileList = unzipFileInternal(zipFile, fDestinationFolder, writeToDisk, null);
		return fileList;
	}

	public static File unzipSingleFile(final File zipFile, final String fileToUnzipPath, final String fDestination)
			throws IOException {
		return unzipSingleFile(zipFile, fileToUnzipPath, fDestination, null);
	}

	/**
	 * Extracts the informed file from the zip and returns its File Object
	 * representation.
	 *
	 * @param zipFile         - The zip file to extract from.
	 * @param fileToUnzipPath - The file that needs to be extracted
	 * @param fDestination    - The path where the file will be extracted to.
	 * @param monitor         - A monitor to follow the execution or null of there
	 *                        is no need for it.
	 * @return The File Object representation of the file extracted
	 * @throws IOException
	 */
	public static File unzipSingleFile(final File zipFile, final String fileToUnzipPath, final String fDestination,
			final IProgressMonitor monitor) throws IOException {
		final int buffer = 2048;
		final FileInputStream fis = new FileInputStream(zipFile);
		final ZipInputStream zis = new ZipInputStream(new BufferedInputStream(fis));
		File unzippedFile = null;
		try {
			ZipEntry entry;
			final String fDestinationFolder = getDestinationFolder(fDestination);

			while ((entry = zis.getNextEntry()) != null) {
				if ((monitor != null) && monitor.isCanceled()) {
					break;
				}
				final String entryName = entry.getName();
				final boolean isPathOk = entryName.equalsIgnoreCase(fileToUnzipPath.replaceAll("\\\\", "/"));
				final boolean isNameOk = entryName.equalsIgnoreCase(fileToUnzipPath);
				if (isPathOk || isNameOk) {
					unzippedFile = createUnzippedFile(fDestinationFolder, entry, fDestination);

					// create all non exists folders
					// else you will hit FileNotFoundException for compressed folder
					new File(unzippedFile.getParent()).mkdirs();

					if (/* writeToDisk */true) {
						int count;
						final byte[] data = new byte[buffer];
						// write the files to the disk
						final FileOutputStream fos = new FileOutputStream(unzippedFile);
						final BufferedOutputStream dest = new BufferedOutputStream(fos, buffer);
						while ((count = zis.read(data, 0, buffer)) != -1) {
							if ((monitor != null) && monitor.isCanceled()) {
								dest.flush();
								dest.close();
								break;
							}
							dest.write(data, 0, count);
						}
						dest.flush();
						dest.close();
					}
					break;
				}
			}
		} finally {
			zis.close();
			fis.close();
		}
		if ((monitor != null) && monitor.isCanceled()) {
			unzippedFile = null;
		}
		return unzippedFile;
	}

	public static byte[] unzipSingleFile(final File inputFile, final String fileToUnzipPath) throws IOException {
		byte[] buffer = EMPTY_BUFFER;
		final ZipFile zipFile = new ZipFile(inputFile.getAbsolutePath());
		final ZipEntry entry = zipFile.getEntry(fileToUnzipPath);

		if (entry != null) {
			final InputStream is = zipFile.getInputStream(entry);
			buffer = new byte[is.available()];
			is.read(buffer);
		}

		zipFile.close();

		return buffer;
	}

	/**
	 * Uncompress a file and return the content.
	 *
	 * @param zipFile            - The zipped file
	 * @param fDestinationFolder - The folder to unzip.
	 * @param writeToDisk        - If it should write to disk.
	 * @return A list containing all unzipped files.
	 * @throws IOException - Any IOException thrown.
	 */
	private static List<File> unzipFileInternal(final ZipFile zipFile, final String fDestination,
			final boolean writeToDisk, final IProgressMonitor monitor) throws IOException {
		ArrayList<File> fileList = new ArrayList<File>();
		final File target = new File(fDestination);

		try {
			final Enumeration<?> enu = zipFile.entries();
			while (enu.hasMoreElements()) {
				if ((monitor != null) && monitor.isCanceled()) {
					break;
				}
				final ZipEntry zipEntry = (ZipEntry) enu.nextElement();

				final String name = zipEntry.getName();
				// Do we need to create a directory ?
				final File file = new File(target, name);
				if (name.endsWith("/")) {
					file.mkdirs();
					continue;
				}

				final File parent = file.getParentFile();
				if (parent != null) {
					parent.mkdirs();
				}

				// Extract the file
				final InputStream is = zipFile.getInputStream(zipEntry);
				final FileOutputStream fos = new FileOutputStream(file);
				final byte[] bytes = new byte[1024];
				int length;
				while ((length = is.read(bytes)) >= 0) {
					if ((monitor != null) && monitor.isCanceled()) {
						break;
					}
					fos.write(bytes, 0, length);
				}
				is.close();
				fos.close();

				fileList.add(file);
			}
		} finally {
			zipFile.close();
		}
		if ((monitor != null) && monitor.isCanceled()) {
			fileList = null;
		}
		return fileList;
	}

	private static File createUnzippedFile(final String fDestinationFolder, final ZipEntry entry,
			final String fDestination) {
		String destinationFolder = fDestinationFolder;
		String bar = "\\"; //$NON-NLS-1$

		if (TDSUtil.isMac()) {
			bar = "//"; //$NON-NLS-1$
		}

		if (!fDestinationFolder.endsWith(bar)) {
			destinationFolder = fDestinationFolder + bar;
		}
		final String fileName = extractFileName(entry, fDestination);
		final String fullDestinationFileName = fileName.startsWith(destinationFolder) ? fileName
				: destinationFolder + fileName;
		final File unzippedFile = new File(fullDestinationFileName);
		return unzippedFile;
	}

	private static String extractFileName(final ZipEntry entry, final String fDestination) {
		final File file = new File(fDestination);
		String fileName = entry.getName();
		if (!file.isDirectory()) {
			fileName = file.getName();
		}
		return fileName;
	}

	private static String getDestinationFolder(final String fDestination) {
		String folderPath = fDestination;
		final File file = new File(fDestination);
		if (!file.isDirectory()) {
			folderPath = file.getParent();
		}
		return folderPath;
	}

	/**
	 * Opens the default OS browser and direct to the informed hyperlink.<br>
	 *
	 * @param urlString
	 * @throws PartInitException     - if the operation failed for some reason
	 * @throws MalformedURLException - if no protocol is specified, or an unknown
	 *                               protocol is found, or spec is null.See Also:
	 */
	public static void openExternalBrowser(final String urlString) throws PartInitException, MalformedURLException {
		final IWebBrowser browser = PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser();
		final URL url = new URL(urlString);
		browser.openURL(url);
	}

}