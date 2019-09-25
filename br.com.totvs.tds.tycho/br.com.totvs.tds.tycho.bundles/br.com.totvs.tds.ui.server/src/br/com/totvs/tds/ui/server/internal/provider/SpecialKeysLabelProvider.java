package br.com.totvs.tds.ui.server.internal.provider;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class SpecialKeysLabelProvider extends LabelProvider implements ITableLabelProvider {

	@Override
	public Image getColumnImage(Object element, int columnIndex) {
		return null;
	}

	@Override
	public String getColumnText(Object element, int columnIndex) {
//		if (element instanceof SimpleEntry<?, ?>) {
//			SimpleEntry<?, ?> simpleEntry = (SimpleEntry<String, String>) element;
//			switch (columnIndex) {
//			case 0:
//				return (String) simpleEntry.getKey();
//			case 1:
//				return (String) simpleEntry.getValue();
//			}
//		}
		return null;
	}

}
