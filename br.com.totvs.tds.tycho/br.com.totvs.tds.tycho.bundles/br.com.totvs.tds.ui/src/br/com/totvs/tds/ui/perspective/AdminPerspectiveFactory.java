package br.com.totvs.tds.ui.perspective;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * Definição da perspectiva Admin.
 *
 * @author acandido
 *
 */
public class AdminPerspectiveFactory implements IPerspectiveFactory {

	private static final String PERSPECTIVE_ID = "br.com.totvs.tds.ui.adminPerspective";

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.
	 * IPageLayout)
	 */
	@Override
	public void createInitialLayout(final IPageLayout layout) {

		layout.addShowInPart(PERSPECTIVE_ID); // $NON-NLS-1$

	}

}
