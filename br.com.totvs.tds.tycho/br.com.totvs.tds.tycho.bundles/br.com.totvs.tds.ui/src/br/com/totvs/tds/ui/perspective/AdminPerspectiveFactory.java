package br.com.totvs.tds.ui.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * Definição da perspectiva Admin.
 * 
 * @author acandido
 *
 */
public class AdminPerspectiveFactory implements IPerspectiveFactory {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
	 */
	@Override
	public void createInitialLayout(IPageLayout layout) {

		addPerspectiveShortcuts(layout);
		addNewWizardShortcut(layout);
		addViews(layout);
		addActionSets(layout);
		addShowViewShortcuts(layout);

		layout.addShowInPart("br.com.totvs.tds.sdk.adminPerspective"); //$NON-NLS-1$

	}

	/**
	 * @see org.eclipse.ui.IPageLayout#addPerspectiveShortcut(java.lang.String)
	 * @param layout
	 */
	private void addPerspectiveShortcuts(IPageLayout layout) {
		layout.addPerspectiveShortcut("br.com.totvs.tds.sdk.developerPerspective"); //$NON-NLS-1$
		layout.addPerspectiveShortcut("br.com.totvs.tds.sdk.adminPerspective"); //$NON-NLS-1$
		layout.addPerspectiveShortcut("org.eclipse.debug.ui.DebugPerspective"); //$NON-NLS-1$
	}

	/**
	 * @see org.eclipse.ui.IPageLayout#addNewWizardShortcut(java.lang.String)
	 * @param layout
	 */
	private void addNewWizardShortcut(IPageLayout layout) {

	}


	/**
	 * @see org.eclipse.ui.IPageLayout#addView(java.lang.String, int, float, java.lang.String)
	 * @param layout
	 */
	private void addViews(IPageLayout layout) {

		layout.addView("br.com.totvs.tds.ui.server.views.serverView", IPageLayout.LEFT, 0.20f, layout.getEditorArea()); //$NON-NLS-1$

		IFolderLayout folderLayout = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.70f, layout.getEditorArea()); //$NON-NLS-1$
		folderLayout.addView("org.eclipse.ui.console.ConsoleView"); //$NON-NLS-1$
		folderLayout.addView("br.com.totvs.tds.server.ui.ServerMonitor"); //$NON-NLS-1$

		folderLayout = layout.createFolder("right", IPageLayout.RIGHT, 0.75f, layout.getEditorArea()); //$NON-NLS-1$
		folderLayout.addView(IPageLayout.ID_OUTLINE);

	}

	/**
	 * @see org.eclipse.ui.IPageLayout#addActionSet(java.lang.String)
	 * @param layout
	 */
	private void addActionSets(IPageLayout layout) {

	}

	/**
	 * @see org.eclipse.ui.IPageLayout#addShowViewShortcut(java.lang.String)
	 * @param layout
	 */
	private void addShowViewShortcuts(IPageLayout layout) {
		layout.addShowViewShortcut("br.com.totvs.tds.server.ui.ServerMonitor"); //$NON-NLS-1$
		layout.addShowViewShortcut("org.eclipse.ui.console.ConsoleView"); //$NON-NLS-1$
	}

}
