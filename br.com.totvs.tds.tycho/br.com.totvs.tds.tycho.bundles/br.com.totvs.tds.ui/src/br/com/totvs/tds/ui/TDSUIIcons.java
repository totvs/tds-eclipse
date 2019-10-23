package br.com.totvs.tds.ui;

import org.eclipse.jface.resource.ImageDescriptor;

/**
 * Icons to be used from all UI plugins.
 *
 * @author acandido
 * @author daniel.yampolschi
 *
 */
public final class TDSUIIcons {

	/**
	 * Construtor.
	 */
	private TDSUIIcons() {
	}

	public static ImageDescriptor getIconTDS() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/tds.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteOut() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/writeout.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getIconWriteErr() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/writeerr.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getCancel() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/cancel.png"); //$NON-NLS-1$
	}

	public static ImageDescriptor getOk() {
		return TDSUIActivator.getDefault().getImageDescriptor("icons/ok.png"); //$NON-NLS-1$
	}

}
