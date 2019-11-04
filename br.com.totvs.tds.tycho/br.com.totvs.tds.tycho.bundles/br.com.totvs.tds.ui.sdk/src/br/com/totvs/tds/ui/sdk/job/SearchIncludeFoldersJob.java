package br.com.totvs.tds.ui.sdk.job;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.IProgressConstants;
import org.eclipse.ui.progress.UIJob;

import br.com.totvs.tds.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.sdk.wrapper.WrapperManager;
import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.SdkUIIcons;
import br.com.totvs.tds.ui.sdk.dialog.SearchIncludeResultDialog;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

public class SearchIncludeFoldersJob extends UIJob {
	public static final QualifiedName PROJECT = new QualifiedName("searchIncludeFoldersJob", "project");
	public static final QualifiedName CURRENT_LIST = new QualifiedName("searchIncludeFoldersJob", "current.list");

	private String targetFolder;
	private int countVisitedFolders = 0;
	private List<String> folders = new ArrayList<String>();
	private FileFilter filterFolders;
	private FilenameFilter filterHeaderFile;

	public SearchIncludeFoldersJob(final String targetFolder) {
		super(String.format(Messages.SearchIncludeFoldersJob_Search_folders_definition_files, targetFolder));

		this.targetFolder = targetFolder;

		setUser(true);
	}

	@Override
	public IStatus runInUIThread(final IProgressMonitor monitor) {
		monitor.beginTask(Messages.SearchIncludeFoldersJob_Searching, IProgressMonitor.UNKNOWN);

		filterFolders = pathname -> pathname.isDirectory();
		filterHeaderFile = (dir, name) -> name.toLowerCase().endsWith(".ch") || name.toLowerCase().endsWith(".LOGIX");

		final File dir = new File(targetFolder);
		if (!dir.isDirectory()) {
			return SdkUIActivator.showStatus(IStatus.ERROR, Messages.SearchIncludeFoldersJob_Target_resource_not_folder,
					targetFolder);
		}

		searchFolders(dir, monitor);

		if (folders.isEmpty()) {
			return SdkUIActivator.showStatus(monitor.isCanceled() ? IStatus.CANCEL : IStatus.OK,
					Messages.SearchIncludeFoldersJob_No_definition_files_found, targetFolder);
		}

		setProperty(IProgressConstants.ICON_PROPERTY, getImage());
		if (isModal(this)) {
			showResults();
		} else {
			setProperty(IProgressConstants.KEEP_PROPERTY, Boolean.TRUE);
			setProperty(IProgressConstants.ACTION_PROPERTY, getCompletedAction());
		}

		monitor.setTaskName(Messages.SearchIncludeFoldersJob_Search_completed);

		if (monitor.isCanceled()) {
			SdkUIActivator.logStatus(IStatus.CANCEL,
					Messages.SearchIncludeFoldersJob_Partial_search + "\n\t"
							+ Messages.SearchIncludeFoldersJob_Folders_were_found,
					folders.size(), countVisitedFolders, ISDKPreferenceKeys.RESULT_SEARCH);
		} else {
			return SdkUIActivator.logStatus(IStatus.OK, Messages.SearchIncludeFoldersJob_Folders_were_found,
					folders.size(), countVisitedFolders);
		}

		return SdkUIActivator.showStatus(monitor.isCanceled() ? IStatus.CANCEL : IStatus.OK, "Busca finalizada.");
	}

	protected void showResults() {
		Display.getDefault().asyncExec(() -> getCompletedAction().run());
	}

	protected Action getCompletedAction() {
		return new Action("Busca por pastas de definição") {
			@Override
			public void run() {
				final Object[] currentList = (Object[]) getProperty(SearchIncludeFoldersJob.CURRENT_LIST);
				final IProject project = (IProject) getProperty(SearchIncludeFoldersJob.PROJECT);

				final SearchIncludeResultDialog dialog = new SearchIncludeResultDialog();

				dialog.setTitle("Pastas com arquivos de definições");
				dialog.setMessage("Selecione as pastas a ser incluídas na busca.");
				dialog.setElements(folders);
				dialog.setBlockOnOpen(true);

				if (dialog.open() == Window.OK) {
					final Object[] result = dialog.getResult();

					if (result.length > 0) {
						try {
							final IProjectWrapper wrapperProject = WrapperManager.getInstance().getWrapper(project);
							final List<String> selecteds = new ArrayList<String>();

							for (final Object object : currentList) {
								selecteds.add((String) object);
							}
							for (final Object object : result) {
								selecteds.add((String) object);
							}

							wrapperProject.setIncludeSearchList(selecteds.toArray(new String[selecteds.size()]));
						} catch (final CoreException e) {
							SdkUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
						}
					}
				}
			}
		};
	}

	private boolean isModal(final Job job) {
		final Boolean isModal = (Boolean) getProperty(IProgressConstants.PROPERTY_IN_DIALOG);

		if (isModal == null) {
			return false;
		}

		return isModal.booleanValue();
	}

	private Image getImage() {
		return SdkUIIcons.getSearch().createImage(true);
	}

	private void searchFolders(final File folder, final IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return;
		}

		countVisitedFolders++;
		String parcialPath = folder.getAbsolutePath();
		parcialPath = "..." + parcialPath.substring(targetFolder.length() - 1); //$NON-NLS-1$
		monitor.subTask(String.format(" #%d [%s]", countVisitedFolders, parcialPath)); //$NON-NLS-1$

		final String[] fileList = folder.list(filterHeaderFile);

		if ((fileList != null) && (fileList.length > 0)) {
			this.folders.add(folder.getAbsolutePath());
		}

		final File[] folderList = folder.listFiles(filterFolders);
		if (folderList != null) {
			for (final File subfolder : folderList) {
				searchFolders(subfolder, monitor);
			}
		}
	}

}
