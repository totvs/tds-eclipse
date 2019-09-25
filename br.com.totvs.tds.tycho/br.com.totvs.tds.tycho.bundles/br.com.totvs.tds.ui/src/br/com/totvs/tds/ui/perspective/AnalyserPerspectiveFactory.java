package br.com.totvs.tds.ui.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class AnalyserPerspectiveFactory implements IPerspectiveFactory {

	/* OK */private static final String CALLLISTVIEW = "br.com.totvs.tds.server.ui.profile.analyser.views.CallListView"; //$NON-NLS-1$
	/* OK */private static final String EXECUTIONPROFILESVIEW = "br.com.totvs.tds.server.ui.profile.analyser.views.ExecutionsView"; //$NON-NLS-1$
	/* OK */private static final String CALLTREEVIEW = "br.com.totvs.tds.server.ui.profile.analyser.views.CallTreeView"; //$NON-NLS-1$
	/* OK */public static final String ANALYSERPERSPECTIVEID = "br.com.totvs.tds.server.ui.profile"; //$NON-NLS-1$
	private static final String BOTTOMFOLDER = "br.com.totvs.tds.server.ui.profile.analyser.views.BottomFolder"; //$NON-NLS-1$
	/* OK */private static final String SQLLISTVIEW = "br.com.totvs.tds.server.ui.profile.analyser.views.SQLListView"; //$NON-NLS-1$

	// @Override
	@Override
	public void createInitialLayout(IPageLayout layout) {
		layout.setEditorAreaVisible(false);
		IFolderLayout folder = layout.createFolder(BOTTOMFOLDER, IPageLayout.LEFT, 1f, layout.getEditorArea());
		folder.addView(CALLLISTVIEW);
		folder.addView(SQLLISTVIEW);
		layout.addView(AnalyserPerspectiveFactory.EXECUTIONPROFILESVIEW, IPageLayout.LEFT, 0.2f,
				AnalyserPerspectiveFactory.BOTTOMFOLDER);
		layout.addView(AnalyserPerspectiveFactory.CALLTREEVIEW, IPageLayout.TOP, 0.5F,
				AnalyserPerspectiveFactory.BOTTOMFOLDER);
	}

}
