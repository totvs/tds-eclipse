package br.com.totvs.tds.ui.sdk.widget;

import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.ui.sdk.SdkUIIcons;

/**
 * @author acandido
 */
class IncludeListLabelProvider extends StyledCellLabelProvider { // implements ILabelProvider {

	private static final Image FOLDER = SdkUIIcons.getFolder().createImage(true);
	private static final Image GLOBAL = SdkUIIcons.getGlobal().createImage(true);
	private static final Image WORKSPACE = SdkUIIcons.getWorkspace().createImage(true);

	@Override
	public void update(final ViewerCell cell) {
		final IncludeDataModel element = (IncludeDataModel) cell.getElement();
		StyledString styledString = new StyledString();
		String text = element.getFolder();
		Image image = null;

		if (element.getFolder().equals(IncludeDataModel.GLOBAL)) {
			image = GLOBAL;
			text = Messages.IncludeListLabelProvider_Global;
		} else if (element.getFolder().startsWith(IncludeDataModel.WORKSPACE)) {
			image = WORKSPACE;
		} else {
			image = FOLDER;
		}

		if (element.getMessage() != null) {
			Styler stylerLabel = new Styler() {
				@Override
				public void applyStyles(final TextStyle textStyle) {

					if (element.isWarning()) {
						textStyle.background = Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW);
					} else {
						textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);
						textStyle.background = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
					}
				}
			};

			Styler stylerMessage = new Styler() {
				@Override
				public void applyStyles(final TextStyle textStyle) {

					if (element.isWarning()) {
						textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_YELLOW);
					} else {
						textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
					}
				}
			};

			styledString.append(String.format("%s", text), stylerLabel); //$NON-NLS-1$
			styledString.append(String.format(" (%s)", element.getMessage().toLowerCase()), stylerMessage); //$NON-NLS-1$
		} else {
			styledString.append(element.getFolder());
		}

		cell.setText(styledString.toString());
		cell.setStyleRanges(styledString.getStyleRanges());
		cell.setImage(image);

		super.update(cell);

	}

//	@Override
//	public Image getImage(Object _element) {
//		final IncludeDataModel element = (IncludeDataModel) _element;
//		Image image = null;
//		
//		if (element.getFolder().equals(IncludeDataModel.GLOBAL)) {
//			image = GLOBAL;
//		} else if (element.getFolder().startsWith(IncludeDataModel.WORKSPACE)) {
//			image = WORKSPACE;
//		} else {
//			image = FOLDER;
//		}
//		
//		return image;
//	}
//
//	@Override
//	public String getText(Object _element) {
//		final IncludeDataModel element = (IncludeDataModel) _element;
//		String text = "";
//		
//		if (element.getFolder().equals(IncludeDataModel.GLOBAL)) {
//			text = "Global";
//		} else if (element.getFolder().startsWith(IncludeDataModel.WORKSPACE)) {
//			text = element.getFolder();
//		} else {
//			text = element.getFolder();
//		}
//		
//		return text;
//	}
}