package br.com.totvs.tds.ui.console;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsolePageParticipant;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.part.IPageBookViewPage;
import org.eclipse.ui.part.IShowInSource;
import org.eclipse.ui.part.IShowInTargetList;
import org.eclipse.ui.part.ShowInContext;

/**
 * Creates and manages process console specific actions
 *
 * @since 3.1
 */
@SuppressWarnings("restriction")
public class MessageConsolePageParticipant implements IConsolePageParticipant, IPropertyChangeListener, IShowInSource {

	// actions
//	private ShowWhenContentChangesAction fStdOut;
//	private ShowWhenContentChangesAction fStdErr;

	private MessageConsole fConsole;
	private IPageBookViewPage fPage;

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.console.IConsolePageParticipant#init(IPageBookViewPage,
	 * IConsole)
	 */
	@Override
	public void init(final IPageBookViewPage page, final IConsole console) {
//		IPreferenceStore store = DebugUIPlugin.getDefault().getPreferenceStore();
//		store.addPropertyChangeListener(this);
//
//		fPage = page;
//		fConsole = (MessageConsole) console;
//
//		fStdOut = new ShowStandardOutAction();
//		fStdErr = new ShowStandardErrAction();
//
//		IActionBars actionBars = fPage.getSite().getActionBars();
//		configureToolBar(actionBars.getToolBarManager());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.console.IConsolePageParticipant#dispose()
	 */
	@Override
	public void dispose() {
//		final IPreferenceStore store = DebugUIPlugin.getDefault().getPreferenceStore();
//		store.removePropertyChangeListener(this);
//
//		if (fStdOut != null) {
//			fStdOut.dispose();
//			fStdOut = null;
//		}
//		if (fStdErr != null) {
//			fStdErr.dispose();
//			fStdErr = null;
//		}

		fConsole = null;
	}

	/**
	 * Contribute actions to the toolbar
	 */
	protected void configureToolBar(final IToolBarManager mgr) {
//		mgr.appendToGroup(IConsoleConstants.OUTPUT_GROUP, fStdOut);
//		mgr.appendToGroup(IConsoleConstants.OUTPUT_GROUP, fStdErr);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> T getAdapter(final Class<T> required) {
		if (IShowInSource.class.equals(required)) {
			return (T) this;
		}
		if (IShowInTargetList.class.equals(required)) {
			return (T) this;
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.
	 * jface.util.PropertyChangeEvent)
	 */
	@Override
	public void propertyChange(final PropertyChangeEvent event) {
		final String property = event.getProperty();

//		if (property.equals(ShowStandardOutAction.ID)) {
//			final IPreferenceStore store = DebugUIPlugin.getDefault().getPreferenceStore();
//			final boolean activateOnOut = store.getBoolean(ShowStandardOutAction.ID);
//			fConsole.setAttribute(ShowStandardOutAction.ID, activateOnOut);
//			fConsole.firePropertyChange(event.getSource(), ShowStandardOutAction.ID, event.getOldValue(),
//					event.getNewValue());
//		} else if (property.equals(ShowStandardErrAction.ID)) {
//			final IPreferenceStore store = DebugUIPlugin.getDefault().getPreferenceStore();
//			final boolean activateOnErr = store.getBoolean(ShowStandardErrAction.ID);
//			fConsole.setAttribute(ShowStandardErrAction.ID, activateOnErr);
//			fConsole.firePropertyChange(event.getSource(), ShowStandardErrAction.ID, event.getOldValue(),
//					event.getNewValue());
//		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.console.IConsolePageParticipant#activated()
	 */
	@Override
	public void activated() {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.console.IConsolePageParticipant#deactivated()
	 */
	@Override
	public void deactivated() {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.part.IShowInSource#getShowInContext()
	 */
	@Override
	public ShowInContext getShowInContext() {
		// TODO Auto-generated method stub
		return null;
	}
}
