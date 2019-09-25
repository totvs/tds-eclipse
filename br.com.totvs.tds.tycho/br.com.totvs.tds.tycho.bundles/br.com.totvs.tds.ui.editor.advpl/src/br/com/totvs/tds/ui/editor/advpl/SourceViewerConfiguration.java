package br.com.totvs.tds.ui.editor.advpl;

import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.source.ISourceViewer;

public class SourceViewerConfiguration extends org.eclipse.jface.text.source.SourceViewerConfiguration {

	@Override
	public IPresentationReconciler getPresentationReconciler(ISourceViewer viewer) {
		// Defines a TextMate Presentation reconcilier
		return null; //new TMPresentationReconciler();
	}

}
