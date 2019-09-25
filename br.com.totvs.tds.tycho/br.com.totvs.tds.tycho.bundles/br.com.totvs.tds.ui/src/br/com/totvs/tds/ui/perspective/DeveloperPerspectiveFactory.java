package br.com.totvs.tds.ui.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * Definição da perspectiva Desenvolvedor.
 * 
 * @author acandido
 *
 */
public class DeveloperPerspectiveFactory implements IPerspectiveFactory {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
	 */
	@Override
	public void createInitialLayout(final IPageLayout layout) {

		addPerspectiveShortcuts(layout);
		addNewWizardShortcut(layout);
		addViews(layout);
		addActionSets(layout);
		addShowViewShortcuts(layout);

		layout.addShowInPart("br.com.totvs.tds.sdk.developerPerspective"); //$NON-NLS-1$
	}

	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPageLayout#addPerspectiveShortcut(java.lang.String)
	 */
	private void addPerspectiveShortcuts(final IPageLayout layout) {
		layout.addPerspectiveShortcut("br.com.totvs.tds.sdk.platformPerspective"); //$NON-NLS-1$
		layout.addPerspectiveShortcut("br.com.totvs.tds.sdk.adminPerspective"); //$NON-NLS-1$
		layout.addPerspectiveShortcut("org.eclipse.debug.ui.DebugPerspective"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPageLayout#addNewWizardShortcut(java.lang.String)
	 */
	private void addNewWizardShortcut(final IPageLayout layout) {
		layout.addNewWizardShortcut("br.com.totvs.tds.sdk.ui.SDKProjectWizard"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPageLayout#addView(java.lang.String, int, float, java.lang.String)
	 */
	private void addViews(final IPageLayout layout) {

		layout.addView(IPageLayout.ID_PROJECT_EXPLORER, IPageLayout.LEFT, 0.20f, layout.getEditorArea());

		layout.addPlaceholder("br.com.totvs.tds.ui.server.views.serverView", IPageLayout.BOTTOM, 0.70f, //$NON-NLS-1$
				IPageLayout.ID_PROJECT_EXPLORER);

		IFolderLayout folderLayout = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.70f, layout.getEditorArea()); //$NON-NLS-1$
		folderLayout.addView("org.eclipse.ui.console.ConsoleView"); //$NON-NLS-1$
		folderLayout.addView(IPageLayout.ID_PROBLEM_VIEW);

		folderLayout.addPlaceholder("br.com.totvs.tds.server.ui.ServerMonitor"); //$NON-NLS-1$
		folderLayout.addPlaceholder(IPageLayout.ID_BOOKMARKS);

		folderLayout = layout.createFolder("right", IPageLayout.RIGHT, 0.75f, layout.getEditorArea()); //$NON-NLS-1$
		folderLayout.addView(IPageLayout.ID_OUTLINE);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPageLayout#addActionSet(java.lang.String)
	 */
	private void addActionSets(final IPageLayout layout) {
		layout.addActionSet("org.eclipse.debug.ui.launchActionSet"); //$NON-NLS-1$
		layout.addActionSet("org.eclipse.debug.ui.breakpointActionSet"); //$NON-NLS-1$
		layout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IPageLayout#addShowViewShortcut(java.lang.String)
	 */
	private void addShowViewShortcuts(final IPageLayout layout) {
		layout.addShowViewShortcut("br.com.totvs.tds.server.ui.ServerMonitor"); //$NON-NLS-1$
		layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		layout.addShowViewShortcut("org.eclipse.ui.console.ConsoleView"); //$NON-NLS-1$
	}
}
