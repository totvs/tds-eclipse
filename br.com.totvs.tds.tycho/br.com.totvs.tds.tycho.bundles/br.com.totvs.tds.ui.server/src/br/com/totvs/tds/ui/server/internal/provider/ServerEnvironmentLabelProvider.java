package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * Label provider da tabela de ambientes.
 * 
 * @author eriky.kashivagui
 */
public class ServerEnvironmentLabelProvider extends LabelProvider implements ITableLabelProvider {

	@Override
	public Image getColumnImage(final Object obj, final int index) {
		return null;
	}

	@Override
	public String getColumnText(final Object obj, final int index) {
		/*
		 * if (obj instanceof IServerIniEnvironment) { return ((IServerIniEnvironment)
		 * obj).getName(); }
		 */		return ""; //$NON-NLS-1$
	}

}
