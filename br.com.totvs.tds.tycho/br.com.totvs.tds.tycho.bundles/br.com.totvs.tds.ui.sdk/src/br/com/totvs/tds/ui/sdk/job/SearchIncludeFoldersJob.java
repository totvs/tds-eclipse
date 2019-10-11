package br.com.totvs.tds.ui.sdk.job;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;

import br.com.totvs.tds.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

public class SearchIncludeFoldersJob extends Job {

	private String targetFolder;
	private int countVisitedFolders = 0;
	private List<String> folders = new ArrayList<String>();
	private FileFilter filterFolders;
	private FilenameFilter filterHeaderFile;

	public SearchIncludeFoldersJob(String targetFolder) {
		super(String.format(Messages.SearchIncludeFoldersJob_Search_folders_definition_files, targetFolder));

		this.targetFolder = targetFolder;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {

		monitor.beginTask(Messages.SearchIncludeFoldersJob_Searching, IProgressMonitor.UNKNOWN);

		filterFolders = new FileFilter() {

			@Override
			public boolean accept(File pathname) {

				return pathname.isDirectory();
			}
		};

		filterHeaderFile = new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {

				return name.toLowerCase().endsWith(".ch") || name.toLowerCase().endsWith(".LOGIX"); // #FIX: header //$NON-NLS-1$ //$NON-NLS-2$
																									// logix
			}
		};

		File dir = new File(targetFolder);
		if (!dir.isDirectory()) {
			return SdkUIActivator.showStatus(IStatus.ERROR, Messages.SearchIncludeFoldersJob_Target_resource_not_folder, targetFolder);
		}

		searchFolders(dir, monitor);

		if (monitor.isCanceled() && !folders.isEmpty()) {
			return SdkUIActivator.showStatus(IStatus.CANCEL, String.format(Messages.SearchIncludeFoldersJob_Partial_search, "dialog:result_search"), //$NON-NLS-2$
					Messages.SearchIncludeFoldersJob_Folders_were_found,
					URI.create("dialog:result_search"), folders.size(), countVisitedFolders, //$NON-NLS-1$
					ISDKPreferenceKeys.RESULT_SEARCH);
		}

		if (folders.isEmpty()) {
			return SdkUIActivator.showStatus(monitor.isCanceled() ? IStatus.CANCEL : IStatus.OK, Messages.SearchIncludeFoldersJob_No_definition_files_found,
					targetFolder);
		}

		synchronized (SdkUIActivator.getDefault().getPreferenceStore()) {
			StringJoiner result = new StringJoiner(IWrapperManager.INCLUDES_SEPARATOR);

			for (String folder : folders) {
				result.add(folder);
			}

			IPreferenceStore ps = SdkUIActivator.getDefault().getPreferenceStore();
			String actual = ps.getString(ISDKPreferenceKeys.RESULT_SEARCH);

			if (actual != null) {
				String[] items = actual.split(IWrapperManager.INCLUDES_SEPARATOR);
				for (String item : items) {
					if (!folders.contains(item)) {
						result.add(item);
					}
				}
			}

			ps.setValue(ISDKPreferenceKeys.RESULT_SEARCH, result.toString());
		}

		return SdkUIActivator.showStatus(IStatus.OK, String.format(Messages.SearchIncludeFoldersJob_Search_completed, "dialog:result_search"), //$NON-NLS-2$
				Messages.SearchIncludeFoldersJob_Folders_were_found,
				URI.create(Messages.SearchIncludeFoldersJob_15), folders.size(), countVisitedFolders,
				ISDKPreferenceKeys.RESULT_SEARCH);
	}

	private void searchFolders(final File folder, final IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return;
		}

		countVisitedFolders++;
		String parcialPath = folder.getAbsolutePath();
		parcialPath = "..." + parcialPath.substring(targetFolder.length() - 1); //$NON-NLS-1$
		monitor.subTask(String.format(" #%d [%s]", countVisitedFolders, parcialPath)); //$NON-NLS-1$

		String[] fileList = folder.list(filterHeaderFile);

		if ((fileList != null) && (fileList.length > 0)) {
			this.folders.add(folder.getAbsolutePath());
		}

		File[] folderList = folder.listFiles(filterFolders);
		if (folderList != null) {
			for (File subfolder : folderList) {
				searchFolders(subfolder, monitor);
			}
		}
	}

}
