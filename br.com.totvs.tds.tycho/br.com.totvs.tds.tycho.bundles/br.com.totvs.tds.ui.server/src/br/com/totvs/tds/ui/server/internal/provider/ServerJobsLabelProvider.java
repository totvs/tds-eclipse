package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class ServerJobsLabelProvider extends LabelProvider implements ITableLabelProvider {

	public Image getColumnImage(final Object obj, final int index) {
		return null;
	}

	public String getColumnText(final Object obj, final int index) {
//		if (obj instanceof IServerIniJobs) {
//			return ((IServerIniJobs) obj).getName();
//		}
		return ""; //$NON-NLS-1$
	}

}
